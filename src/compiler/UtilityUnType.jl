# UtilityUnType — driver-side adapter glue.
#
# This file lives in `src/compiler/` (and is excluded from self-check)
# because it legitimately peeks `OpaqueHandle.val::Any` — using the
# wrapped Tuple-type as a Dict key for per-signature memoization on top
# of the `inferReturnType` membrane op. Reaching past the opaque handle
# is *driver privilege*; from the analyzer side it would be a layering
# violation. The other unwrap-doing membrane wrappers live next to it
# in `src/compiler/CompilerOps.jl`; this file adds the same kind of
# unwrap-using behavior, just with memoization on top.
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
        v = inferReturnType(ctx.rtctx, ctx, sig_h)
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
        positional_sig_h = makeCallSig(rtctx, arg_types, kwarg_types_for_lookup)
        mis = inferKeywordMethodHandles(eng.globalCtx, positional_sig_h.val)
        if length(mis) == 0
            error("No method instance for keyword call")
        end
        kwargs_ = Vector{Pair{Symbol, CompileType}}()
        for i in mis
            for (k, node) in kwargs
                push!(kwargs_, k=>removeConst(rtctx, node.typ))
            end
            push!(eng.globalCtx.queue, KwFunCall(i, kwargs_))
        end
        for (k, node) in kwargs
            push!(kwarg_types_for_lookup, k => node.typ)
        end
    end
    sig_h = makeCallSig(rtctx, arg_types, kwarg_types_for_lookup)
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
    sig_h = makeCallSig(eng.globalCtx.rtctx, arg_types, kwarg_types)
    return get(eng.globalCtx.dynDispatchCache, sig_h.val, false)
end

# Look up the trace strings (one per dyn-dispatch site found in the
# transitive call tree) that inferReturnType stashed under the same sig.
# Returns an empty vector when `printDispatchTrace` is off or no trace
# was recorded.
function dispatchTraceForCall(eng::Engine, ms::MethodCallStruct)::Vector{String}
    arg_types   = CompileType[fn.typ for fn in ms.fargs]
    kwarg_types = Pair{Symbol, CompileType}[k => node.typ for (k, node) in ms.kwargs]
    sig_h = makeCallSig(eng.globalCtx.rtctx, arg_types, kwarg_types)
    return get(eng.globalCtx.dynDispatchTraceCache, sig_h.val, String[])
end

# `tryApplyType` returns Just(t) if Core.apply_type succeeds, None otherwise.
# Driver-side applyTypeOf returns a bottom CompileType on failure; we test
# via isBottomType and project that into Maybe{CompileType}.
function tryApplyType(rtctx::RuntimeContext, args::Vector{CompileType})::Maybe{CompileType}
    tt = applyTypeOf(rtctx, args)
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

function tryMergeConstVal(rtctx::RuntimeContext, v1::CompileType, v2::CompileType)::Bool
    if isConstVal(v1) && isConstVal(v2)
        return isConstEqualsConst(rtctx, v1, v2)
    else
        return false
    end
end

function tryMergeFlowNode(eng::Engine, ast::JuAST, v::Vector{FlowNode}, allowUnion::Bool)::FlowNode
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
            if !isTypeEq(rtctx, tmptyp, v[i].typ)
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
            tmptyp = makeUnionOf(rtctx, tmptyp, v[i].typ)
        end
    end
    return makePhiFlowNode(ast, v, tmptyp)
end
