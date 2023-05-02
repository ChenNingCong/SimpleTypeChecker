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

function cacheLookup(eng::Engine, sig)
    ctx = eng.globalCtx
    if haskey(ctx.cache, sig)
        return ctx.cache[sig]
    else
        # TODO : the major slowdown source
        # we should use result from Julia's builtin compiler
        v = Base.code_typed_by_type(sig)
        if length(v) == 1
            ctx.cache[sig] = v
        end
        return v
    end
end

function getMethodMatches(eng::Engine, f::Function, tts::Vector{FlowNode})
    sig = Tuple{typeof(f), [i.typ.typ for i in tts]...}
    cacheLookup(eng, sig)
end

function getMethodMatches(eng::Engine, tts::Vector{FlowNode})
    sig = Tuple{[i.typ.typ for i in tts]...}
    cacheLookup(eng, sig)
end

function extractUniqueMatch(v::Vector{Any})::CompileType
    makeType(v[1][2])
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

function tryMergeFlowNode(eng::Engine, ex::JuExpr, v::Vector{FlowNode}, allowUnion::Bool)::FlowNode
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
        return makePhiFlowNode(ex, v, makeBottomType())
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
    return makePhiFlowNode(ex, v, tmptyp)
end