function hasField(ft::CompileType, p::Symbol)::Bool  
    return hasfield(ft.typ, p)
end

function isConcreteType(ft::CompileType)::Bool
    tt = ft.typ
    return isconcretetype(tt) || tt <: Type
end

function getFieldType(ft::CompileType, p::Symbol)::CompileType
    tt = ft.typ
    if hasfield(tt, p)
        return makeType(fieldtype(tt, p))
    else
        error("Internal error : getFieldType not checked")
    end
end

function getFromModule(mod::Core.Module, p::Symbol)::CompileType
    # todo handle typed global here
    if isdefined(mod, p)
        if isconst(mod, p)
            return makeConstVal(getproperty(mod, p))
        else
            return makeType(Any)
        end
    else
        error("Internal error : getFromModule not checked")
    end
end

function isModuleDefined(mod::Core.Module, p::Symbol)::Bool
    return isdefined(mod, p)::Bool
end



function getReturnType(ctx::GlobalContext, sig)::Vector{CompileType}
    param1 = sig.parameters[1]
    if param1 <: Type
        getReturnType(ctx, sig, param1.parameters[1], Base.to_tuple_type(sig.parameters[2:end]))
    else
        getReturnType(ctx, sig, param1.instance, Base.to_tuple_type(sig.parameters[2:end]))
    end
end

function getReturnType(ctx::GlobalContext, f, tt)::Vector{CompileType}
    getReturnType(ctx, Tuple{Core.Typeof(f), tt...}, f, tt)
end

function getReturnType(ctx::GlobalContext, sig, f, types;world = Core.Compiler.get_world_counter())::Vector{CompileType}
    interp = Core.Compiler.NativeInterpreter(world; inf_params = Core.Compiler.InferenceParams())
    if ccall(:jl_is_in_pure_context, Bool, ())
        error("code reflection cannot be used from generated functions")
    end
    if isa(f, Core.OpaqueClosure)
        error("InternalError : OpaqueClosure is unsupported")
    end
    if isa(f, Core.Builtin)
        argtypes = Any[Base.to_tuple_type(types).parameters...]
        rt = Core.Compiler.builtin_tfunction(interp, f, argtypes, nothing)
        rt = Core.Compiler.ignorelimited(rt)
        if rt isa Core.Compiler.Const
            return CompileType[makeConstVal(rt.val)]
        else
            return CompileType[makeType(Core.Compiler.widenconst(rt))]
        end
    end
    rts = CompileType[]
    for match in Base._methods_by_ftype(sig, -1, world)::Vector
        match = match::Core.MethodMatch
        method = Base.func_for_method_checked(match.method, types, match.sparams)
        mi = Core.Compiler.specialize_method(method, match.spec_types, match.sparams)::Core.Compiler.MethodInstance
        result = Core.Compiler.InferenceResult(mi)
        Core.Compiler.typeinf(interp, result, :global)
        if result.result isa Core.Compiler.InferenceState
            push!(rts, makeType(Any))
        else
            ty = Core.Compiler.ignorelimited(result.result)
            if ty isa Core.Compiler.Const
                push!(rts, makeConstVal(ty.val))
            else
                push!(rts, makeType(Core.Compiler.widenconst(ty)))
            end
        end
    end
    if hasfield(Core.Compiler.NativeInterpreter, :cache)
        cache = interp.cache
    else
        cache = interp.inf_cache
    end
    for i in cache
        push!(ctx.queue, KwFunCall(i.linfo))
    end
    return rts
end

function getKeywordMethodInstances(ctx::GlobalContext, sig)::Vector{Core.MethodInstance}
    # f(x...;kwargs...) => we lookup method for 
    #=
    if isa(f, Core.Builtin)
        error("Builtin function should never be called with kewyord")
    end
    =#
    types =  Base.to_tuple_type(sig.parameters[2:end])
    results = Vector{Core.MethodInstance}()
    for match in Base._methods_by_ftype(sig, -1, Core.Compiler.get_world_counter())::Vector
        match = match::Core.MethodMatch
        method = Base.func_for_method_checked(match.method, types, match.sparams)
        mi = Core.Compiler.specialize_method(method, match.spec_types, match.sparams)::Core.Compiler.MethodInstance
        push!(results, mi)
    end
    return results
end

function cacheLookup(eng::Engine, sig)::Vector{CompileType}
    ctx = eng.globalCtx
    if haskey(ctx.cache, sig)
        return ctx.cache[sig]
    else
        # TODO : the major slowdown source
        # we should use result from Julia's builtin compiler
        v = getReturnType(eng.globalCtx, sig)
        ctx.cache[sig] = v
        return v
    end
end

@nocheck function makeNameTupleType(args::Vector{Pair{Symbol, CompileType}})::CompileType
    return makeType(NamedTuple{tuple([i.first for i in args]...), Tuple{[i.second.typ for i in args]...}})
end

function getMethodMatches(eng::Engine, ms::MethodCallStruct)::Vector{CompileType}
    args = ms.fargs
    kwargs = ms.kwargs
    sig = Tuple{[i.typ.typ for i in args]...}
    if length(kwargs) > 0
        mis = getKeywordMethodInstances(eng.globalCtx, sig)
        if length(mis) == 0
            error("No method instance for keyword call")
        end
        kwargs_ = Vector{Pair{Symbol, CompileType}}()
        for i in mis
            for (k, node) in kwargs
                push!(kwargs_, k=>removeConst(node.typ))
            end
            push!(eng.globalCtx.queue, KwFunCall(i, kwargs_))
        end
        tup = NamedTuple{tuple([i.first for i in kwargs]...), Tuple{[i.second.typ.typ for i in kwargs]...}}
        sig = Tuple{typeof(Core.kwcall), tup, sig.parameters...}
    end
    result = cacheLookup(eng, sig)
    return result
end

function extractUniqueMatch(v)::CompileType
    v[1]
end

function checkFieldCompatibility(dest::CompileType, src::CompileType)::Bool
    return src.typ <: dest.typ
end

function isConstructor(v::CompileType)::Bool
    return isConstVal(v) && v.val isa Type
end

function tryApplyType(args::Vector{CompileType})::Maybe{CompileType}
    f = args[1].val
    try
        tt = Core.apply_type(f, (args[i].val for i in 2:length(args))...)
        return Just(makeConstVal(tt))
    catch e
        return None(CompileType)
    end
end

#=
    Untility for merging flow type
=#
function tryMergeFlowType(v1::FlowNode, v2::FlowNode)
    v1typ = v1.typ
    v2typ = v2.typ
    if v2typ.typ <: v1typ.typ
        return true
    else
        return false
    end
end

function tryMergeCompileValue(v1::CompileType, v2::CompileType)::Bool
    return v2.typ <: v1.typ
end

function tryMergeReturnFlowNode(v1::FlowNode, v2::FlowNode)::Bool
    return tryMergeFlowType(v1, v2)
end

function collectUnion(x)::Vector{Any}
    if x isa Union
        return vcat(collectUnion(x.a), collectUnion(x.b))
    else
        return Any[x]
    end
end

function splitUnion(x::CompileType, y::CompileType)::CompileType
    # note, we use the type of left value and the value of right type
    u1 = collectUnion(x.typ) 
    u2 = collectUnion(y.typ)
    makeType(Union{setdiff(u1, u2)...})
end

function isaType(x::CompileType)::Bool
    x.typ isa Type
end

function isaAny(x::CompileType)::Bool
    x.typ == Any
end

function isaBool(x::CompileType)::Bool
    x.typ == Bool
end

function isaUnion(x::CompileType)::Bool
    x.typ isa Union
end

function isaTuple(x::CompileType)::Bool
    x.typ <: Tuple
end

function getFirstParameter(x::CompileType)::CompileType
    return makeType(x.typ.parameters[1])
end

function tryMergeConstVal(v1::CompileType, v2::CompileType)::Bool
    if isConstVal(v1) && isConstVal(v2)
        return v1.val === v2.val
    else
        return false
    end
end
#=
function tryMergeVarDefFlowNode(eng::Engine, ast::JuAST, v::Vector{FlowNode}, allowUnion::Bool)::FlowNode
    # println("join node $([i.typ for i in v])")
    tmptyp = v[1].typ
    for i in v
        if !isBottomType(i.typ)
            tmptyp = i.typ
            break
        end
    end
    if isBottomType(tmptyp)
        # TODO : shouldn't be here
        return makePhiFlowNode(ast, v, makeBottomType())
    end
    allConst = true
    for i in 1:length(v)
        # skip all bottom value
        if isBottomType(v[i].typ)
            continue
        end
        v1 = tmptyp.typ
        v2 = v[i].typ.typ
        if !allowUnion
            if v1 != v2
                reportErrorIfEnlargeType(eng, v[1], v[i])
            end
        end
        # Note, if we allow union, then type may not equal
        if allConst && isConstVal(tmptyp) && isConstVal(v[i].typ)
            allConst = tryMergeConstVal(tmptyp, v[i].typ)
        else
            allConst = false
        end
        if !allConst
            tmptyp = makeType(Union{v1, v2})
        end
    end
    return makePhiFlowNode(ast, v, tmptyp)
end
=#

function tryMergeFlowNode(eng::Engine, ast::JuAST, v::Vector{FlowNode}, allowUnion::Bool)::FlowNode
    # println("join node $([i.typ for i in v])")
    tmptyp = v[1].typ
    for i in v
        if !isBottomType(i.typ)
            tmptyp = i.typ
            break
        end
    end
    if isBottomType(tmptyp)
        # TODO : shouldn't be here
        return makePhiFlowNode(ast, v, makeBottomType())
    end
    allConst = true
    for i in 1:length(v)
        # skip all bottom value
        if isBottomType(v[i].typ)
            continue
        end
        v1 = tmptyp.typ
        v2 = v[i].typ.typ
        if !allowUnion
            if v1 != v2
                reportErrorIfEnlargeType(eng, v[1], v[i])
            end
        end
        # Note, if we allow union, then type may not equal
        if allConst && isConstVal(tmptyp) && isConstVal(v[i].typ)
            allConst = tryMergeConstVal(tmptyp, v[i].typ)
        else
            allConst = false
        end
        if !allConst
            tmptyp = makeType(Union{v1, v2})
        end
    end
    return makePhiFlowNode(ast, v, tmptyp)
end

@nocheck function makeTupleType(tt::Vector{FlowNode})::CompileType
    makeType(Tuple{[i.typ.typ for i in tt]...})
end
