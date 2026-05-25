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



# Inference primitives moved to CompilerAdaptor.jl. The interface below stays in
# this file because it's not compiler-internals — it's the per-Engine caching layer
# that wraps inferReturnType / inferKeywordMethodHandles.

function cacheLookup(eng::Engine, sig)::Vector{CompileType}
    ctx = eng.globalCtx
    if haskey(ctx.cache, sig)
        return ctx.cache[sig]
    else
        # Routed through the runtime context (see docs/isolation.md). The
        # FunctionWrapper has an `Any` return slot for forward-reference
        # reasons; narrow it back to Vector{CompileType} here.
        v = ctx.rtctx.inferReturnType(ctx, sig)::Vector{CompileType}
        ctx.cache[sig] = v
        return v
    end
end

function getMethodMatches(eng::Engine, ms::MethodCallStruct)::Vector{CompileType}
    args = ms.fargs
    kwargs = ms.kwargs
    rtctx = eng.globalCtx.rtctx
    arg_types  = CompileType[fn.typ for fn in args]
    kwarg_types_for_lookup = Pair{Symbol, CompileType}[]
    if length(kwargs) > 0
        positional_sig = rtctx.makeCallSig(arg_types, kwarg_types_for_lookup)
        mis = inferKeywordMethodHandles(eng.globalCtx, positional_sig)
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
        for (k, node) in kwargs
            push!(kwarg_types_for_lookup, k => node.typ)
        end
    end
    sig = rtctx.makeCallSig(arg_types, kwarg_types_for_lookup)
    result = cacheLookup(eng, sig)
    return result
end

function extractUniqueMatch(v)::CompileType
    v[1]
end

# Mirrors getMethodMatches' sig-building logic so we look up the dyn-dispatch
# flag under the exact key inferReturnType stored it under. Must run AFTER
# getMethodMatches (the cache is populated as a side-effect of that call).
function hasDynamicDispatchForCall(eng::Engine, ms::MethodCallStruct)::Bool
    arg_types   = CompileType[fn.typ for fn in ms.fargs]
    kwarg_types = Pair{Symbol, CompileType}[k => node.typ for (k, node) in ms.kwargs]
    sig = eng.globalCtx.rtctx.makeCallSig(arg_types, kwarg_types)
    return get(eng.globalCtx.dynDispatchCache, sig, false)
end

# Look up the trace strings (one per dyn-dispatch site found in the
# transitive call tree) that inferReturnType stashed under the same sig.
# Returns an empty vector when `printDispatchTrace` is off or no trace
# was recorded.
function dispatchTraceForCall(eng::Engine, ms::MethodCallStruct)::Vector{String}
    arg_types   = CompileType[fn.typ for fn in ms.fargs]
    kwarg_types = Pair{Symbol, CompileType}[k => node.typ for (k, node) in ms.kwargs]
    sig = eng.globalCtx.rtctx.makeCallSig(arg_types, kwarg_types)
    return get(eng.globalCtx.dynDispatchTraceCache, sig, String[])
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

function collectUnion(@nospecialize x)::Vector{Any}
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
    x.typ === Any
end

function isaBool(x::CompileType)::Bool
    x.typ === Bool
end

function isaUnion(x::CompileType)::Bool
    x.typ isa Union
end

function isaTuple(x::CompileType)::Bool
    x.typ <: Tuple
end

# Single-method `@nospecialize` escape hatch: the caller's typed
# `CompileType.typ::Any` field flows in, dynamic-typed at the seam.
# Body asserts DataType (typeassert is a Builtin, not dyn dispatch) then
# reads the typed `.parameters::SimpleVector` field. runCheck! skips
# checking this body via `_isEscapeHatchMethod`.
function _firstTypeParam(@nospecialize t)
    return (t::DataType).parameters[1]
end

function getFirstParameter(x::CompileType)::CompileType
    return makeType(_firstTypeParam(x.typ))
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
            if !(v1 === v2)
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
            if !(v1 === v2)
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

