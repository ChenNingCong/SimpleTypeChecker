function hasField(rtctx::RuntimeContext, ft::CompileType, p::Symbol)::Bool
    # Symbols are interned per-runtime — pass as String so the driver can
    # intern into its own Symbol table on its side.
    return rtctx.hasFieldOf(ft, String(p))
end

function isConcreteType(rtctx::RuntimeContext, ft::CompileType)::Bool
    return rtctx.isConcrete(ft)
end

function getFieldType(rtctx::RuntimeContext, ft::CompileType, p::Symbol)::CompileType
    pstr = String(p)
    if rtctx.hasFieldOf(ft, pstr)
        return unwrap(rtctx.getFieldTypeOf(ft, pstr), CompileType)
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

# Membrane-aware variants that take a const CompileType handle (wrapping a
# Module) instead of a raw Module reference. Used by inferGetProperty so
# the Module value never crosses into the inferencer.
function constModuleIsDefined(rtctx::RuntimeContext, modct::CompileType, p::Symbol)::Bool
    return rtctx.constModuleIsDefined(modct, String(p))
end

function constModuleGetField(rtctx::RuntimeContext, modct::CompileType, p::Symbol)::CompileType
    return unwrap(rtctx.constModuleGetField(modct, String(p)), CompileType)
end

# Tuple-destructure check — delegates to the driver. Returns (isStruct,
# lenOk, isTupleOrPair) so the inferencer can report the right failure mode.
function tupleDestructInfo(rtctx::RuntimeContext, ct::CompileType, n::Int)::Tuple{Bool, Bool, Bool}
    return rtctx.tupleDestructInfo(ct, n)
end



# Inference primitives moved to CompilerAdaptor.jl. The interface below stays in
# this file because it's not compiler-internals — it's the per-Engine caching layer
# that wraps inferReturnType / inferKeywordMethodHandles.

function cacheLookup(eng::Engine, sig_h::OpaqueHandle{CallSigK})::Vector{CompileType}
    ctx = eng.globalCtx
    # Cache key is the raw Tuple type — the OpaqueHandle wraps it for
    # transport across the seam but the in-process dict is keyed by the
    # underlying value so writes from inferReturnType (which unwraps
    # internally) line up with lookups from here.
    sig_raw = sig_h.val
    if haskey(ctx.cache, sig_raw)
        return ctx.cache[sig_raw]
    else
        # FunctionWrapper auto-wraps the return into OpaqueHandle{CompileTypeVecK};
        # unwrap to recover the typed Vector here.
        v = unwrap(ctx.rtctx.inferReturnType(ctx, sig_h), Vector{CompileType})
        ctx.cache[sig_raw] = v
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
        positional_sig_h = rtctx.makeCallSig(arg_types, kwarg_types_for_lookup)
        mis = inferKeywordMethodHandles(eng.globalCtx, positional_sig_h.val)
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
    sig_h = rtctx.makeCallSig(arg_types, kwarg_types_for_lookup)
    result = cacheLookup(eng, sig_h)
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
    sig_h = eng.globalCtx.rtctx.makeCallSig(arg_types, kwarg_types)
    return get(eng.globalCtx.dynDispatchCache, sig_h.val, false)
end

# Look up the trace strings (one per dyn-dispatch site found in the
# transitive call tree) that inferReturnType stashed under the same sig.
# Returns an empty vector when `printDispatchTrace` is off or no trace
# was recorded.
function dispatchTraceForCall(eng::Engine, ms::MethodCallStruct)::Vector{String}
    arg_types   = CompileType[fn.typ for fn in ms.fargs]
    kwarg_types = Pair{Symbol, CompileType}[k => node.typ for (k, node) in ms.kwargs]
    sig_h = eng.globalCtx.rtctx.makeCallSig(arg_types, kwarg_types)
    return get(eng.globalCtx.dynDispatchTraceCache, sig_h.val, String[])
end

function checkFieldCompatibility(rtctx::RuntimeContext, dest::CompileType, src::CompileType)::Bool
    return rtctx.subtypeOf(src, dest)
end

function isConstructor(rtctx::RuntimeContext, v::CompileType)::Bool
    return isConstVal(v) && rtctx.constIsTypeValue(v)
end

function tryApplyType(rtctx::RuntimeContext, args::Vector{CompileType})::Maybe{CompileType}
    tt = unwrap(rtctx.applyTypeOf(args), CompileType)
    if isBottomType(rtctx, tt)
        return None(CompileType)
    else
        return Just(tt)
    end
end

#=
    Untility for merging flow type
=#
function tryMergeFlowType(rtctx::RuntimeContext, v1::FlowNode, v2::FlowNode)
    if rtctx.subtypeOf(v2.typ, v1.typ)
        return true
    else
        return false
    end
end

function tryMergeCompileValue(rtctx::RuntimeContext, v1::CompileType, v2::CompileType)::Bool
    return rtctx.subtypeOf(v2, v1)
end

function tryMergeReturnFlowNode(rtctx::RuntimeContext, v1::FlowNode, v2::FlowNode)::Bool
    return tryMergeFlowType(rtctx, v1, v2)
end

# `collectUnion` / `splitUnion` moved behind the seam — both peek `.a`/`.b`
# on Union values, which is target-runtime data. The driver does the
# decomposition and hands back a fresh CompileType handle.
function splitUnion(rtctx::RuntimeContext, x::CompileType, y::CompileType)::CompileType
    return unwrap(rtctx.splitUnionOf(x, y), CompileType)
end

function isaType(rtctx::RuntimeContext, x::CompileType)::Bool
    return rtctx.isTypeValue(x)
end

function isaAny(rtctx::RuntimeContext, x::CompileType)::Bool
    return rtctx.isAny(x)
end

function isaBool(rtctx::RuntimeContext, x::CompileType)::Bool
    return rtctx.isBool(x)
end

function isaUnion(rtctx::RuntimeContext, x::CompileType)::Bool
    return rtctx.isUnion(x)
end

function isaTuple(rtctx::RuntimeContext, x::CompileType)::Bool
    return rtctx.isTupleType(x)
end

# Driver-side: get-first-parameter on a CompileType handle. Peeks `.typ`
# and `t.parameters[1]` in the driver runtime; returns a freshly-wrapped
# CompileType via the membrane.
function getFirstParameter(rtctx::RuntimeContext, x::CompileType)::CompileType
    return unwrap(rtctx.getFirstParameterOf(x), CompileType)
end

function tryMergeConstVal(rtctx::RuntimeContext, v1::CompileType, v2::CompileType)::Bool
    if isConstVal(v1) && isConstVal(v2)
        return rtctx.constEqualsConst(v1, v2)
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
    rtctx = eng.globalCtx.rtctx
    tmptyp = v[1].typ
    for i in v
        if !isBottomType(rtctx, i.typ)
            tmptyp = i.typ
            break
        end
    end
    if isBottomType(rtctx, tmptyp)
        # TODO : shouldn't be here
        return makePhiFlowNode(ast, v, makeBottomType())
    end
    allConst = true
    for i in 1:length(v)
        # skip all bottom value
        if isBottomType(rtctx, v[i].typ)
            continue
        end
        if !allowUnion
            if !rtctx.typeEq(tmptyp, v[i].typ)
                reportErrorIfEnlargeType(eng, v[1], v[i])
            end
        end
        # Note, if we allow union, then type may not equal
        if allConst && isConstVal(tmptyp) && isConstVal(v[i].typ)
            allConst = tryMergeConstVal(rtctx, tmptyp, v[i].typ)
        else
            allConst = false
        end
        if !allConst
            tmptyp = unwrap(rtctx.makeUnionOf(tmptyp, v[i].typ), CompileType)
        end
    end
    return makePhiFlowNode(ast, v, tmptyp)
end

