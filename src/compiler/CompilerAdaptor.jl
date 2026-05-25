# CompilerAdaptor — sole owner of Julia compiler/runtime implementation details.
#
# No file outside this one should reference Core.Compiler.*, Base._methods_by_ftype,
# Base.to_tuple_type, ccall(:jl_*), Core.MethodInstance/Core.Method by their built-in
# names, or reach into .sparam_vals / .specTypes / .def / .sig / .linfo / .parent fields.
# Use the aliases (MethodHandle, MethodDef) and accessor functions defined here instead.
#
# Currently targets Julia 1.12+. When porting to a future Julia version, edit this file
# only — if a port requires touching files outside CompilerAdaptor, the adaptor was
# leaking; fix the leak rather than the call site.

# Type aliases MethodHandle and MethodDef are defined in CompilerAdaptorTypes.jl,
# which loads before InferenceDefinition.jl so its struct fields can use the aliases.

# === Runtime status ===

@nocheck function isInPureContext()::Bool
    return ccall(:jl_is_in_pure_context, Bool, ())
end

@nocheck function currentWorld()::UInt
    return Core.Compiler.get_world_counter()
end

# === Inference primitives ===

@nocheck function inferReturnType(ctx::GlobalContext, sig, f, types; world = currentWorld())::Vector{CompileType}
    interp = Core.Compiler.NativeInterpreter(world; inf_params = Core.Compiler.InferenceParams())
    if isInPureContext()
        error("code reflection cannot be used from generated functions")
    end
    if isa(f, Core.OpaqueClosure)
        error("InternalError : OpaqueClosure is unsupported")
    end
    if isa(f, Core.Builtin)
        argtypes = Any[Base.to_tuple_type(types).parameters...]
        rt = Core.Compiler.builtin_tfunction(interp, f, argtypes, nothing)
        rt = Core.Compiler.ignorelimited(rt)
        # Builtins lower directly — no dispatch chain to walk.
        if ctx.deepDispatchCheck
            ctx.dynDispatchCache[sig] = false
        end
        if rt isa Core.Compiler.Const
            return CompileType[makeConstVal(rt.val)]
        else
            return CompileType[makeType(Core.Compiler.widenconst(rt))]
        end
    end
    rts = CompileType[]
    # If we ever fail to materialize a frame for one of the matches, we can't
    # see into that callee — treat that as equivalent to dynamic dispatch
    # (Julia's trim would reject it for the same reason: no IR to verify).
    unableToInfer = false
    hasDyn = false
    for match in Base._methods_by_ftype(sig, -1, world)::Vector
        match = match::Core.MethodMatch
        mi = Core.Compiler.specialize_method(match.method, match.spec_types, match.sparams)::MethodHandle
        result = Core.Compiler.InferenceResult(mi)
        frame = Core.Compiler.InferenceState(result, :global, interp)
        if frame === nothing
            # Source unavailable (e.g. generated function source we can't materialize)
            push!(rts, makeType(Any))
            unableToInfer = true
            continue
        end
        Core.Compiler.typeinf(interp, frame)
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
        # The seed match's IR lives in `result.src`. interp.codegen only
        # captures *freshly* inferred MIs (i.e. ones not already cached in
        # the system image), so the seed itself often isn't there — we must
        # walk result.src too. This is what catches dynamic dispatch when
        # the user calls something like `println(::String)` whose IR has a
        # `:call Base.print` that survives optimization because `stdout`
        # widens to abstract `IO`. Skipped when deep mode is off — the
        # walk is the expensive part of this function.
        if ctx.deepDispatchCheck && !hasDyn && result.src isa Core.CodeInfo
            if _codeinfo_has_dyncall(result.src)
                hasDyn = true
            end
        end
    end
    # Julia 1.12: completed inference results live in `interp.codegen` as
    # CodeInstance → CodeInfo entries. Each CodeInstance.def is a MethodHandle.
    # This is how nested method instances visited during inference get queued
    # for the type checker to recurse into.
    for codeinst in keys(interp.codegen)
        mi_ = codeinst.def
        if mi_ isa MethodHandle
            push!(ctx.queue, KwFunCall(mi_))
        end
    end
    # Walk every CodeInfo the inferencer produced for this call's transitive
    # tree, looking for any `:call` (post-inference) that isn't a builtin /
    # intrinsic — that's dynamic dispatch deep in the chain. Statically
    # resolved calls become `:invoke`; remaining `:call`s with a non-Builtin
    # callee are the trim-killing kind. Only run this in deep mode; in
    # shallow mode the cache stays empty and inferFunCall falls back to
    # the `tt.typ === Any` heuristic.
    if ctx.deepDispatchCheck
        ctx.dynDispatchCache[sig] = unableToInfer || hasDyn || _codegen_has_dyncall(interp)
    end
    return rts
end

# A post-inference call statement is "statically resolved" iff its head is
# `:invoke` (Julia turned the dispatch into a known MethodInstance) OR its head
# is `:call` and the callee is a Builtin / IntrinsicFunction (these lower
# directly without going through the method table). Anything else with head
# `:call` is dynamic dispatch — unless the inferencer was able to constprop
# the whole call to a Const result, in which case there is no real runtime
# dispatch happening (the value is folded at compile time).
@nocheck function _callee_inferred_type(f, ci::Core.CodeInfo)
    if f isa Core.SSAValue
        if f.id <= length(ci.ssavaluetypes)
            return ci.ssavaluetypes[f.id]
        end
    elseif f isa Core.SlotNumber
        if f.id <= length(ci.slottypes)
            return ci.slottypes[f.id]
        end
    elseif f isa Core.Argument
        if f.n <= length(ci.slottypes)
            return ci.slottypes[f.n]
        end
    end
    return nothing
end

@nocheck function _is_static_callee(f, ci::Core.CodeInfo)::Bool
    if f isa Core.IntrinsicFunction || f isa Core.Builtin
        return true
    end
    if f isa GlobalRef
        if isdefined(f.mod, f.name)
            v = getfield(f.mod, f.name)
            if v isa Core.IntrinsicFunction || v isa Core.Builtin
                return true
            end
        end
        return false
    end
    if f isa QuoteNode
        v = f.value
        return v isa Core.IntrinsicFunction || v isa Core.Builtin
    end
    # SSA / slot / argument callee: only static if the inferred callee value
    # itself is a Builtin/Intrinsic. A Const-known *user* function still
    # requires method-table lookup against the inferred arg types — if the
    # optimizer didn't rewrite to :invoke, dispatch couldn't be uniquely
    # resolved, so it's dynamic.
    t = _callee_inferred_type(f, ci)
    if t isa Core.Const
        v = t.val
        if v isa Core.IntrinsicFunction || v isa Core.Builtin
            return true
        end
    end
    return false
end

@nocheck function _codeinfo_has_dyncall(ci::Core.CodeInfo)::Bool
    code = ci.code
    ssatypes = ci.ssavaluetypes
    for i in 1:length(code)
        stmt = code[i]
        if !(stmt isa Expr)
            continue
        end
        h = stmt.head
        # `:invoke`, `:foreigncall` (ccall), `:new`, `:gc_preserve_*`, etc.
        # are all statically resolved.
        if h !== :call
            continue
        end
        if length(stmt.args) == 0
            continue
        end
        # If the call's result is a Const, the inferencer fully folded the
        # dispatch at compile time — there's no runtime method-table lookup
        # left. This eliminates a false positive on calls Julia constprop'd
        # (e.g. `zero(Vec3{Float64})` lowers to `:call %2` whose result is
        # `Core.Const([0.0, 0.0, 0.0])`).
        if i <= length(ssatypes) && ssatypes[i] isa Core.Const
            continue
        end
        if _is_static_callee(stmt.args[1], ci)
            continue
        end
        return true
    end
    return false
end

@nocheck function _codegen_has_dyncall(interp)::Bool
    for codeinst in keys(interp.codegen)
        src = interp.codegen[codeinst]
        if src isa Core.CodeInfo
            if _codeinfo_has_dyncall(src)
                return true
            end
        end
    end
    return false
end

function inferReturnType(ctx::GlobalContext, sig)::Vector{CompileType}
    param1 = sig.parameters[1]
    if param1 <: Type
        inferReturnType(ctx, sig, param1.parameters[1], Base.to_tuple_type(sig.parameters[2:end]))
    elseif Base.issingletontype(param1)
        inferReturnType(ctx, sig, param1.instance, Base.to_tuple_type(sig.parameters[2:end]))
    else
        # Callable types without a singleton instance — closures that
        # capture variables, callable structs with fields, FunctionWrappers.
        # The downstream path uses only `sig` to locate methods; pass
        # `nothing` as the placeholder for `f`. The Builtin / OpaqueClosure
        # branches won't trigger (isa check fails on nothing), and the main
        # `_methods_by_ftype(sig, ...)` path doesn't need f.
        inferReturnType(ctx, sig, nothing, Base.to_tuple_type(sig.parameters[2:end]))
    end
end

function inferReturnType(ctx::GlobalContext, f, tt)::Vector{CompileType}
    inferReturnType(ctx, Tuple{Core.Typeof(f), tt...}, f, tt)
end

@nocheck function inferKeywordMethodHandles(ctx::GlobalContext, sig)::Vector{MethodHandle}
    results = MethodHandle[]
    for match in Base._methods_by_ftype(sig, -1, currentWorld())::Vector
        match = match::Core.MethodMatch
        mi = Core.Compiler.specialize_method(match.method, match.spec_types, match.sparams)::MethodHandle
        push!(results, mi)
    end
    return results
end

# === MethodHandle accessors ===

@nocheck function methodHandleDef(mi::MethodHandle)
    return mi.def
end

@nocheck function methodHandleModule(mi::MethodHandle)::Core.Module
    v = mi.def
    if v isa Module
        return v
    else
        return v.module
    end
end

@nocheck function methodHandleSpecTypes(mi::MethodHandle)
    return mi.specTypes
end

@nocheck function methodHandleSparamCount(mi::MethodHandle)::Int
    return length(mi.sparam_vals)
end

@nocheck function methodHandleSparamValue(mi::MethodHandle, i::Int)
    return mi.sparam_vals[i]
end

@nocheck function methodHandleSpecTypesLength(mi::MethodHandle)::Int
    return length(mi.specTypes.parameters)
end

@nocheck function methodHandleSpecTypesParam(mi::MethodHandle, i::Int)
    return mi.specTypes.parameters[i]
end

@nocheck function makeSparamCompileType(mi::MethodHandle, i::Int)::CompileType
    return makeConstVal(methodHandleSparamValue(mi, i))
end

@nocheck function makeArgCompileType(mi::MethodHandle, i::Int)::CompileType
    return makeType(methodHandleSpecTypesParam(mi, i))
end

# === MethodDef accessors ===

@nocheck function methodDefSig(meth::MethodDef)
    return meth.sig
end

@nocheck function isConcreteMethodDef(meth::MethodDef)::Bool
    z = meth.sig
    if z isa UnionAll
        return false
    end
    if !(z <: Tuple)
        return false
    end
    for i in 2:length(z.parameters)
        if !Base.isconcretetype(z.parameters[i])
            return false
        end
    end
    return true
end

@nocheck function methodDefCodeInfo(meth::MethodDef)::Any
    Base.code_typed_by_type(meth.sig)[1].first
end

# === CodeInfo accessor (used by API.jl and the runCheck pipeline) ===

@nocheck function codeInfoMethodHandle(ci)
    return ci.parent
end
