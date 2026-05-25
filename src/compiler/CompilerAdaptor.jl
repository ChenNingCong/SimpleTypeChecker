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
        if rt isa Core.Compiler.Const
            return CompileType[makeConstVal(rt.val)]
        else
            return CompileType[makeType(Core.Compiler.widenconst(rt))]
        end
    end
    rts = CompileType[]
    for match in Base._methods_by_ftype(sig, -1, world)::Vector
        match = match::Core.MethodMatch
        mi = Core.Compiler.specialize_method(match.method, match.spec_types, match.sparams)::MethodHandle
        result = Core.Compiler.InferenceResult(mi)
        frame = Core.Compiler.InferenceState(result, :global, interp)
        if frame === nothing
            # Source unavailable (e.g. generated function source we can't materialize)
            push!(rts, makeType(Any))
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
    return rts
end

function inferReturnType(ctx::GlobalContext, sig)::Vector{CompileType}
    param1 = sig.parameters[1]
    if param1 <: Type
        inferReturnType(ctx, sig, param1.parameters[1], Base.to_tuple_type(sig.parameters[2:end]))
    else
        inferReturnType(ctx, sig, param1.instance, Base.to_tuple_type(sig.parameters[2:end]))
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
