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

function isInPureContext()::Bool
    return ccall(:jl_is_in_pure_context, Bool, ())
end

function currentWorld()::UInt
    return Core.Compiler.get_world_counter()
end

# === Inference primitives ===

@nocheck function inferReturnType(ctx::GlobalContext, @nospecialize(sig), @nospecialize(f), @nospecialize(types); world = currentWorld())::Vector{CompileType}
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
    # Seed-frame CodeInfos (one per matched method). Tracked so that when
    # the user opts into `printDispatchTrace`, we can walk these in addition
    # to interp.codegen — the seed's IR is the most likely place the user
    # call's dispatch issues show up.
    seedCIs = Any[]
    for match in Base._methods_by_ftype(sig, -1, world)::Vector
        match = match::Core.MethodMatch
        mi = Core.Compiler.specialize_method(match.method, match.spec_types, match.sparams)::MethodHandle
        # Concrete-signature fast path. When the flag is on AND the call sig
        # is fully concrete AND the cached CodeInstance has a concrete
        # rettype (or a `rettype_const`), trust it and skip transitive-edge
        # queuing. Parametric methods like `getindex(::Vector{T}, ::Int)::T`
        # are handled implicitly: `specialize_method` resolves T from
        # `match.spec_types`, so the cached CI's rettype is already the
        # substituted leaf type (`Float64` for a `Vector{Float64}` call).
        # No manual sparam dance needed.
        if ctx.concreteShortCircuit && !ctx.deepDispatchCheck && _is_concrete_call_sig(sig)
            ci = get(Core.Compiler.code_cache(interp), mi, nothing)
            if ci isa Core.CodeInstance
                if isdefined(ci, :rettype_const)
                    push!(rts, makeConstVal(ci.rettype_const))
                    continue
                elseif Base.isconcretetype(ci.rettype)
                    push!(rts, makeType(ci.rettype))
                    continue
                end
                # Cached but rettype not concrete (e.g. Any, Union) — fall
                # through to the conservative path so we keep queuing
                # transitive edges and don't lose diagnostic depth.
            end
        end
        # Cache short-circuit. When deep-dispatch detection is off (the
        # common path), a cached CodeInstance already carries rettype +
        # transitive edges, so we can skip the full inference + optimizer
        # pipeline that typeinf would otherwise re-run. The deep-dispatch
        # path still needs `result.src` for the dyncall scan, so we fall
        # through to the original code when ctx.deepDispatchCheck is true.
        if !ctx.deepDispatchCheck
            ci = get(Core.Compiler.code_cache(interp), mi, nothing)
            if ci isa Core.CodeInstance
                # rettype_const is #undef unless const-prop produced a value
                if isdefined(ci, :rettype_const)
                    push!(rts, makeConstVal(ci.rettype_const))
                else
                    push!(rts, makeType(ci.rettype))
                end
                for edge in ci.edges
                    if edge isa MethodHandle
                        push!(ctx.queue, KwFunCall(edge))
                    end
                end
                continue
            end
        end
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
        if ctx.deepDispatchCheck && result.src isa Core.CodeInfo
            push!(seedCIs, result.src)
            if !hasDyn && _codeinfo_has_dyncall(result.src)
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
        hadDyn = unableToInfer || hasDyn || _codegen_has_dyncall(interp)
        ctx.dynDispatchCache[sig] = hadDyn
        if hadDyn && ctx.printDispatchTrace
            traceLines = String[]
            for ci in seedCIs
                if ci isa Core.CodeInfo
                    _codeinfo_collect_dyncalls!(traceLines, ci)
                end
            end
            _codegen_collect_dyncalls_into!(traceLines, interp)
            ctx.dynDispatchTraceCache[sig] = traceLines
        end
    end
    return rts
end

# Concrete-call-sig predicate. The sig that reaches `inferReturnType` is a
# Tuple type whose first parameter is the function type (`typeof(f)` or a
# singleton Type) and whose remaining parameters are the argument types.
# We need every component to be a concrete leaf type: no `UnionAll`, no
# `Union` (other than the single-element Bottom edge case), no abstract
# supertype. `Base.isconcretetype` already checks all of these uniformly.
# We also reject sigs whose first parameter isn't a concrete function tag
# (e.g. when the caller passes `nothing` as the placeholder for a callable
# struct whose type isn't a singleton — covered by the 2-arg `inferReturnType`
# dispatcher's third branch).
# Escape hatch only — NOT the recommended entry point.
#
# Pushes every registered method-def onto the work queue as its OWN
# specialization, using the method's declared signature. For real
# package analysis, prefer harvesting concrete MethodInstances from a
# test run (`Base.specializations(method)` after `Pkg.test(pkg)`) and
# feeding those via `check(ctx, f, tt)` — the resulting concrete sigs
# (e.g. `Foo{Matrix{Float64}}`) actually resolve field types and
# dispatch targets.
#
# This helper seeds the bare declared sig, which for `function f(x::Foo)`
# where `Foo{M}` is parametric becomes `Foo{M} where M`. The inferencer
# then can't resolve `x.field` because `M` is unbound, producing
# `getproperty on non-concrete type Foo{M} where M` warnings that aren't
# real bugs — just noise from shape-only seeding. Use only when test runs
# aren't available and you accept that noise.
@nocheck function seedAllRegistered!(ctx::GlobalContext)::Nothing
    for meth in keys(ctx.methodDefs)
        sig = meth.sig
        # Skip parametric — no concrete sparams available without a call site.
        sig isa UnionAll && continue
        mi = try
            Core.Compiler.specialize_method(meth, sig, Core.svec())
        catch
            continue
        end
        mi isa MethodHandle || continue
        push!(ctx.queue, KwFunCall(mi))
    end
    return nothing
end

@nocheck function _is_concrete_call_sig(@nospecialize(sig))::Bool
    sig isa DataType || return false
    sig <: Tuple || return false
    params = sig.parameters
    isempty(params) && return false
    for p in params
        Base.isconcretetype(p) || return false
    end
    return true
end


# A post-inference call statement is "statically resolved" iff its head is
# `:invoke` (Julia turned the dispatch into a known MethodInstance) OR its head
# is `:call` and the callee is a Builtin / IntrinsicFunction (these lower
# directly without going through the method table). Anything else with head
# `:call` is dynamic dispatch — unless the inferencer was able to constprop
# the whole call to a Const result, in which case there is no real runtime
# dispatch happening (the value is folded at compile time).
function _callee_inferred_type(@nospecialize(f), ci::Core.CodeInfo)
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

function _is_static_callee(@nospecialize(f), ci::Core.CodeInfo)::Bool
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

# Trace-collecting variants. When the user opts into `printDispatchTrace`
# we walk the same IR but instead of short-circuiting on first dyncall
# we accumulate every offending statement into `out` as one human-readable
# line per site. Format: "<method-name> @ <file>:<line> — <callee>(...)"
# or "<method-name> — <callee>(...)" when no source line is recorded.
@nocheck function _stringify_callee(f, ci::Core.CodeInfo)::String
    if f isa GlobalRef
        return string(f.mod, '.', f.name)
    elseif f isa QuoteNode
        return string(f.value)
    elseif f isa Core.SSAValue || f isa Core.SlotNumber || f isa Core.Argument
        t = _callee_inferred_type(f, ci)
        if t isa Core.Const
            return string(t.val)
        elseif t === nothing
            return "<unknown>"
        else
            return string("::", t)
        end
    end
    return string(f)
end

@nocheck function _stringify_stmt(stmt::Expr, ci::Core.CodeInfo)::String
    parts = String[]
    push!(parts, _stringify_callee(stmt.args[1], ci))
    push!(parts, "(")
    n = length(stmt.args)
    for i in 2:n
        a = stmt.args[i]
        push!(parts, _stringify_callee(a, ci))
        if i != n
            push!(parts, ", ")
        end
    end
    push!(parts, ")")
    return join(parts)
end

# Try to recover a (file, line) for statement `i` in `ci`. CodeInfo carries
# a `codelocs::Vector{Int32}` of indices into `linetable::Vector{LineInfoNode}`
# (Julia 1.10–1.12). If anything's not in the expected shape, return ("", 0).
@nocheck function _stmt_source_loc(ci::Core.CodeInfo, i::Int)::Tuple{String, Int}
    if !isdefined(ci, :codelocs) || !isdefined(ci, :linetable)
        return ("", 0)
    end
    locs = ci.codelocs
    lt = ci.linetable
    if !(locs isa Vector) || !(lt isa Vector)
        return ("", 0)
    end
    if i > length(locs)
        return ("", 0)
    end
    idx = Int(locs[i])
    if idx <= 0 || idx > length(lt)
        return ("", 0)
    end
    info = lt[idx]
    file_str::String = ""
    line_num::Int = 0
    if isdefined(info, :file)
        f = info.file
        if f isa Symbol
            file_str = String(f)
        elseif f isa String
            file_str = f
        end
    end
    if isdefined(info, :line)
        l = info.line
        if l isa Int32 || l isa Int64
            line_num = Int(l)
        end
    end
    return (file_str, line_num)
end

@nocheck function _method_label(ci::Core.CodeInfo)::String
    if isdefined(ci, :parent)
        p = ci.parent
        if p isa Core.MethodInstance
            d = p.def
            if d isa Core.Method
                return string(d.module, '.', d.name)
            elseif d isa Module
                return string(d, ".<toplevel>")
            end
        end
    end
    return "<anonymous>"
end

@nocheck function _codeinfo_collect_dyncalls!(out::Vector{String}, ci::Core.CodeInfo)::Nothing
    code = ci.code
    ssatypes = ci.ssavaluetypes
    label = _method_label(ci)
    for i in 1:length(code)
        stmt = code[i]
        if !(stmt isa Expr) ; continue ; end
        h = stmt.head
        if h !== :call ; continue ; end
        if length(stmt.args) == 0 ; continue ; end
        if i <= length(ssatypes) && ssatypes[i] isa Core.Const ; continue ; end
        if _is_static_callee(stmt.args[1], ci) ; continue ; end
        floc = _stmt_source_loc(ci, i)
        stmt_str = _stringify_stmt(stmt, ci)
        if floc[2] == 0
            push!(out, string(label, " — ", stmt_str))
        else
            push!(out, string(label, " @ ", floc[1], ":", floc[2], " — ", stmt_str))
        end
    end
    return nothing
end

@nocheck function _codegen_collect_dyncalls(interp)::Vector{String}
    out = String[]
    _codegen_collect_dyncalls_into!(out, interp)
    return out
end

@nocheck function _codegen_collect_dyncalls_into!(out::Vector{String}, interp)::Nothing
    for codeinst in keys(interp.codegen)
        src = interp.codegen[codeinst]
        if src isa Core.CodeInfo
            _codeinfo_collect_dyncalls!(out, src)
        end
    end
    return nothing
end

function inferReturnType(ctx::GlobalContext, @nospecialize(sig))::Vector{CompileType}
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

# Seam-facing variant — receives phantom-typed handles from FunctionWrapper,
# unwraps and delegates into the raw 2-arg method above. The inferencer
# never calls this directly; FunctionWrapper invokes it via the
# rtctx.inferReturnType field.
function inferReturnType(ctx_h::OpaqueHandle{GlobalContextK}, sig_h::OpaqueHandle{CallSigK})::Vector{CompileType}
    return inferReturnType(unwrap(ctx_h, GlobalContext), sig_h.val)
end

function inferReturnType(ctx::GlobalContext, @nospecialize(f), @nospecialize(tt))::Vector{CompileType}
    inferReturnType(ctx, Tuple{Core.Typeof(f), tt...}, f, tt)
end

@nocheck function inferKeywordMethodHandles(ctx::GlobalContext, @nospecialize(sig))::Vector{MethodHandle}
    results = MethodHandle[]
    for match in Base._methods_by_ftype(sig, -1, currentWorld())::Vector
        match = match::Core.MethodMatch
        mi = Core.Compiler.specialize_method(match.method, match.spec_types, match.sparams)::MethodHandle
        push!(results, mi)
    end
    return results
end

# === MethodHandle accessors ===

function methodHandleDef(mi::MethodHandle)
    return mi.def
end

function methodHandleModule(mi::MethodHandle)::Core.Module
    v = mi.def
    if v isa Module
        return v
    else
        return v.module
    end
end

# Driver-side: return the method's defining module wrapped as a const
# CompileType. The Module reference stays inside `.val`; the analyzer
# only sees the opaque CompileType. Used to populate `Engine.modCT`.
function methodHandleModuleCT(mi::MethodHandle)::CompileType
    return makeConstVal(methodHandleModule(mi))
end

function methodHandleSpecTypes(mi::MethodHandle)
    return mi.specTypes
end

function methodHandleSparamCount(mi::MethodHandle)::Int
    return length(mi.sparam_vals)
end

function methodHandleSparamValue(mi::MethodHandle, i::Int)
    return mi.sparam_vals[i]
end

# Single-method @nospecialize escape hatches. `mi.specTypes::Any` (the
# declared field type on MethodInstance), but at runtime always either a
# concrete Tuple-shaped DataType (post-specialization) or a UnionAll
# wrapper. The typeassert below narrows the runtime value to DataType,
# after which `.parameters::Core.SimpleVector` and SimpleVector
# `getindex` are statically typed.
function _specTypesLength(@nospecialize st)::Int
    return length((st::DataType).parameters)
end

function _specTypesParam(@nospecialize(st), i::Int)
    return (st::DataType).parameters[i]
end

function methodHandleSpecTypesLength(mi::MethodHandle)::Int
    return _specTypesLength(mi.specTypes)
end

function methodHandleSpecTypesParam(mi::MethodHandle, i::Int)
    return _specTypesParam(mi.specTypes, i)
end

function makeSparamCompileType(mi::MethodHandle, i::Int)::CompileType
    return makeConstVal(methodHandleSparamValue(mi, i))
end

function makeArgCompileType(mi::MethodHandle, i::Int)::CompileType
    return makeType(methodHandleSpecTypesParam(mi, i))
end

# === MethodDef accessors ===

function methodDefSig(meth::MethodDef)
    return meth.sig
end

function isConcreteMethodDef(meth::MethodDef)::Bool
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

# Compatibility shim: a no-op `@nospecialize_singlemethod` style entry
# isn't a real macro yet — the escape-hatch detection lives in
# `_escapeHatchMask` (Inference.jl) and `_isEscapeHatchMethod`.

# `Method.nospecialize` exposed as a UInt32 bitmask. Bit `(i-1)` set means
# the i-th user-visible argument (positional index 1 = first user arg,
# excluding the function itself) is `@nospecialize`d.
function methodDefNospecializeMask(meth::MethodDef)::UInt32
    return UInt32(meth.nospecialize)
end

# Number of positional args (including function slot). `m.nargs` is the
# Julia field that counts function + positional args.
function methodDefNargs(meth::MethodDef)::Int
    return Int(meth.nargs)
end

# Is this method varargs?
function methodDefIsVarArg(meth::MethodDef)::Bool
    return Bool(meth.isva)
end

# Number of keyword args declared on this method.
function methodDefNKw(meth::MethodDef)::Int
    return Int(meth.nkw)
end

# Body-side companion to `_escapeHatchMask` (Inference.jl). Returns true
# if THIS method is the only one on its function AND declares
# `@nospecialize` on some positional slot AND has no kwargs/varargs.
# When true, `runCheck!` skips checking the body — the user has explicitly
# opted out at the typed-to-Any seam.
#
# Driver-side because it peeks `meth.module` / `meth.name` on a Core.Method
# and resolves the function value via `getfield(mod, name)` — both
# target-runtime ops that the inferencer side cannot see.
@nocheck function _isEscapeHatchMethod(meth::MethodDef)::Bool
    if methodDefNKw(meth) != 0 || methodDefIsVarArg(meth)
        return false
    end
    if methodDefNospecializeMask(meth) == UInt32(0)
        return false
    end
    mod = meth.module
    name = meth.name
    if !isdefined(mod, name)
        return false
    end
    fval = getfield(mod, name)
    if !(fval isa Function)
        return false
    end
    return length(_methodsOfImpl(fval)) == 1
end

# Seam-facing variant: callable arrives as an opaque handle. Unwraps to
# `Any` (the callable is still untyped — we can't narrow further before
# `methods()` is called). The inner @nospecialize impl is the escape
# hatch the inferencer recognizes for skipping abstract-arg checks.
function methodsOf(h::OpaqueHandle{CallableK})::Vector{MethodDef}
    return _methodsOfImpl(h.val)
end

function _methodsOfImpl(@nospecialize(f))::Vector{MethodDef}
    ms = methods(f)
    result = MethodDef[]
    for m in ms
        push!(result, m::MethodDef)
    end
    return result
end

# === CodeInfo accessor (used by API.jl and the runCheck pipeline) ===

function codeInfoMethodHandle(@nospecialize ci)
    return ci.parent
end

# === RuntimeContext default factory ===
#
# Builds a RuntimeContext that delegates every operation to the live
# JIT — i.e. behavior identical to before the isolation refactor.
# Tests / alternative drivers construct their own RuntimeContext with
# different fields. See `docs/isolation.md`.
# Build the call signature used for method lookup. Driver-side because
# the splatted `Tuple{...}` apply_type with Any-typed elements is
# fundamentally untrimmable — keeping this on the driver side is
# what lets the inferencer stay clean. The @nocheck here is at the
# right place: it's the adaptor seam to Core.apply_type, not legacy
# laziness in the inference logic.
@nocheck function makeCallSig(fargs_h::OpaqueHandle{CompileTypeVecK},
                              kwargs_h::OpaqueHandle{CompileTypePairVecK})
    fargs  = unwrap(fargs_h,  Vector{CompileType})
    kwargs = unwrap(kwargs_h, Vector{Pair{Symbol, CompileType}})
    positional = Tuple{[ct.typ for ct in fargs]...}
    if length(kwargs) == 0
        return positional
    end
    keys = tuple([p.first for p in kwargs]...)
    valtypes = Tuple{[p.second.typ for p in kwargs]...}
    tup = NamedTuple{keys, valtypes}
    return Tuple{typeof(Core.kwcall), tup, positional.parameters...}
end

# Build a CompileType wrapping the Tuple type of a value-position tuple
# expression. Same untrimmable splat, same reason it lives here.
@nocheck function makeTupleTypeOf(args_h::OpaqueHandle{CompileTypeVecK})::CompileType
    args = unwrap(args_h, Vector{CompileType})
    return makeType(Tuple{[ct.typ for ct in args]...})
end

# Subtype query — driver-side because it peeks `.typ` on both sides of the
# `<:` and because `<:` itself is a runtime-type op. The opaque CompileType
# values stay sealed for callers; only the adaptor cracks them.
@nocheck function subtypeOf(a_h::OpaqueHandle{CompileTypeK}, b_h::OpaqueHandle{CompileTypeK})::Bool
    a = unwrap(a_h, CompileType)
    b = unwrap(b_h, CompileType)
    return a.typ <: b.typ
end

# Single-peek isa-checks. Each cracks `.typ` and compares against a
# driver-runtime constant (Any / Bool / Type / Union / Tuple). The
# inferencer cannot do this directly — its `Any` etc. live in a
# different runtime than the type values being checked.
@nocheck function isAny(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).typ === Any
end

@nocheck function isBool(h::OpaqueHandle{CompileTypeK})::Bool
    t = unwrap(h, CompileType).typ
    # `Bool` is the obvious case. `Core.Compiler.InterConditional` is the
    # narrowing-aware boolean lattice element the upstream inferencer
    # produces for `!x`, `x === y`, `x isa T` patterns — it's treated as
    # Bool everywhere the compiler uses it as a condition. Accept it
    # here so Glob's `if !isempty(relpath)` and similar don't fire
    # `JTC-TYPE: non-boolean type InterConditional used as condition`.
    return t === Bool || t === Core.Compiler.InterConditional
end

@nocheck function isTypeValue(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).typ isa Type
end

@nocheck function isUnion(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).typ isa Union
end

@nocheck function isTupleType(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).typ <: Tuple
end

# "Can flow into typed slots" — concrete, or a Type-value (DataType, UnionAll,
# etc. that's itself in a type-slot position). Driver-side because the
# `isconcretetype` peek and the `<: Type` check both crack `.typ`.
@nocheck function isConcrete(h::OpaqueHandle{CompileTypeK})::Bool
    tt = unwrap(h, CompileType).typ
    return isconcretetype(tt) || tt <: Type
end

# `t.typ === Union{}` — the inferencer's bottom marker. The Union{} here
# is the driver-runtime Union{}, which is what the type values being
# checked actually came from.
@nocheck function isBottom(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).typ === Union{}
end

# Field-shape ops. Both peek `.typ`; getFieldTypeOf additionally
# constructs a fresh CompileType (driver-side because `fieldtype` is a
# runtime-type op and `makeType` wraps a target-runtime value).
@nocheck function hasFieldOf(h::OpaqueHandle{CompileTypeK}, p::String)::Bool
    return hasfield(unwrap(h, CompileType).typ, Symbol(p))
end

@nocheck function getFieldTypeOf(h::OpaqueHandle{CompileTypeK}, p::String)::CompileType
    return makeType(fieldtype(unwrap(h, CompileType).typ, Symbol(p)))
end

# ─── Constant-inspection cluster ────────────────────────────────────────────
#
# Each op cracks `.val` and asks ONE question. The inferencer never sees the
# raw value — only the Bool / re-wrapped handle.

@nocheck function constIsTypeValue(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).val isa Type
end

@nocheck function constIsModule(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).val isa Module
end

@nocheck function constModuleIsDefined(h::OpaqueHandle{CompileTypeK}, p::String)::Bool
    mod = unwrap(h, CompileType).val::Module
    return isdefined(mod, Symbol(p))
end

# Does the receiver type have a non-default `Base.getproperty(::T, ::Symbol)`
# override? Used by the analyzer to decide whether `x.s` should accept
# Any when `s` isn't a struct field.
@nocheck function hasUserGetproperty(h::OpaqueHandle{CompileTypeK})::Bool
    ct = unwrap(h, CompileType)
    # Operate on a representative concrete instance type when ct is a const.
    t = ct.typ
    if t isa Type
        sig = Tuple{typeof(Base.getproperty), t, Symbol}
        ms = try
            Base._methods_by_ftype(sig, -1, Base.get_world_counter())
        catch
            return false
        end
        ms isa Vector || return false
        for m in ms
            m isa Core.MethodMatch || continue
            mod = m.method.module
            if mod !== Base && mod !== Core
                return true
            end
        end
    end
    return false
end

@nocheck function constModuleGetField(h::OpaqueHandle{CompileTypeK}, p::String)::CompileType
    mod = unwrap(h, CompileType).val::Module
    psym = Symbol(p)
    if isconst(mod, psym)
        return makeConstVal(getproperty(mod, psym))
    else
        return makeType(Any)
    end
end

@nocheck function constIsFunction(h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(h, CompileType).val isa Function
end

@nocheck function constIsBuiltinOrIntrin(h::OpaqueHandle{CompileTypeK})::Bool
    v = unwrap(h, CompileType).val
    return v isa Core.Builtin || v isa Core.IntrinsicFunction
end

# Used in the dispatch-failure note: if the called function is `===` or `!==`,
# suggest using `isa Nothing` instead. Hardcoded targets are driver-runtime
# constants — same Builtin instance the user's program would reference.
@nocheck function constEqualsBuiltinEq(h::OpaqueHandle{CompileTypeK})::Bool
    v = unwrap(h, CompileType).val
    return v === (Core.:(===)) || v === (Base.:(!==))
end

# `a.val === b.val` — used by const-merging in tryMergeConstVal. Both sides
# cracked driver-side so the `===` is between two driver-runtime values.
@nocheck function constEqualsConst(a_h::OpaqueHandle{CompileTypeK}, b_h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(a_h, CompileType).val === unwrap(b_h, CompileType).val
end

# `makeType(ct.val)` — converts a const value that IS a Type into the
# corresponding type-value CompileType. Mirrors the old `lift()` helper but
# the peek happens driver-side.
@nocheck function liftConstToType(h::OpaqueHandle{CompileTypeK})::CompileType
    return makeType(unwrap(h, CompileType).val)
end

# Re-wrap a const CompileType's `.val` as a CallableK handle so the rewrapped
# value can be passed to `methodsOf`. The `.val` here is the function/callable
# itself — `methodsOf` will dynamic-dispatch on it via @nospecialize.
@nocheck function getCallableFromConst(h::OpaqueHandle{CompileTypeK})
    return OpaqueHandle{CallableK}(unwrap(h, CompileType).val)
end

# ─── Type-construction cluster ──────────────────────────────────────────────

# `Core.apply_type(args[1].val, args[2].val, ...)` wrapped as a const
# CompileType handle. Failure (caught from Core.apply_type) is reported as
# a bottom-CompileType handle — caller checks via `isBottom` membrane op.
@nocheck function applyTypeOf(args_h::OpaqueHandle{CompileTypeVecK})::CompileType
    args = unwrap(args_h, Vector{CompileType})
    f = args[1].val
    try
        tt = Core.apply_type(f, (args[i].val for i in 2:length(args))...)
        return makeConstVal(tt)
    catch
        return makeBottomType()
    end
end

# `makeConstVal(TypeVar(name))` — used by `where` binding construction.
# Name crosses the seam as String — driver interns into its own Symbol.
@nocheck function makeTypeVarOf(name::String)::CompileType
    return makeConstVal(TypeVar(Symbol(name)))
end

# `makeConstVal(UnionAll(tv.val, base.val))` — UnionAll construction from a
# TypeVar const + a Type const.
@nocheck function makeUnionAllOf(tv_h::OpaqueHandle{CompileTypeK}, base_h::OpaqueHandle{CompileTypeK})::CompileType
    tv = unwrap(tv_h, CompileType).val
    base = unwrap(base_h, CompileType).val
    return makeConstVal(UnionAll(tv, base))
end

# `a.typ === b.typ` — driver-runtime type identity. Used in tryMergeFlowNode
# to detect when two branches agree on type exactly.
@nocheck function typeEq(a_h::OpaqueHandle{CompileTypeK}, b_h::OpaqueHandle{CompileTypeK})::Bool
    return unwrap(a_h, CompileType).typ === unwrap(b_h, CompileType).typ
end

# `makeType(Union{a.typ, b.typ})` — driver-runtime Union construction.
@nocheck function makeUnionOf(a_h::OpaqueHandle{CompileTypeK}, b_h::OpaqueHandle{CompileTypeK})::CompileType
    a = unwrap(a_h, CompileType).typ
    b = unwrap(b_h, CompileType).typ
    return makeType(Union{a, b})
end

# Walks a (possibly-nested) Union's `.a`/`.b` chain into a Vector{Any}.
# Driver-side helper for splitUnionOf — never crosses the seam.
@nocheck function _collectUnion(@nospecialize x)::Vector{Any}
    if x isa Union
        return vcat(_collectUnion(x.a), _collectUnion(x.b))
    else
        return Any[x]
    end
end

# `splitUnionOf(x, y) = makeType(Union{collectUnion(x.typ) \ collectUnion(y.typ)...})`.
# Used for negative-side type narrowing (e.g. `if x isa Nothing ... else /* x is
# T \ Nothing */ end`). Driver-side because Union decomposition peeks `.a/.b`.
@nocheck function splitUnionOf(x_h::OpaqueHandle{CompileTypeK}, y_h::OpaqueHandle{CompileTypeK})::CompileType
    u1 = _collectUnion(unwrap(x_h, CompileType).typ)
    u2 = _collectUnion(unwrap(y_h, CompileType).typ)
    return makeType(Union{setdiff(u1, u2)...})
end

# ─── Display cluster ────────────────────────────────────────────────────────
#
# All `repr` / `string` / `show` of `.val` / `.typ` happens driver-side.
# The inferencer receives a String. Strings cross the seam by value (immutable
# heap-allocated bytes — safe to copy across).
#
# `_type_to_string` and friends live in InferenceErrorUtility.jl. They're
# isa-dispatched and trim-safe (single-method `@nospecialize` entry then
# manual branching by `isa`). We call them here from the driver side.

@nocheck function typeToString(h::OpaqueHandle{CompileTypeK})::String
    return _type_to_string(unwrap(h, CompileType).typ)
end

@nocheck function constToString(h::OpaqueHandle{CompileTypeK})::String
    ct = unwrap(h, CompileType)
    if ct.isConst
        return repr(ct.val)
    else
        return _type_to_string(ct.typ)
    end
end

# Evaluate a lambda AST in the given module. Reconstructs a Julia Expr
# from `ast.tree` (a JuliaSyntax tree node) and `Core.eval`s it in the
# target module — both target-runtime ops. On success, returns the
# anonymous function wrapped as a const CompileType. On failure
# (eval throws), returns the Function type-value so the caller falls
# back to opaque Function.
@nocheck function evalLambdaInModule(modCT_h::OpaqueHandle{CompileTypeK},
                                     ast_h::OpaqueHandle{JuASTK})::CompileType
    modCT = unwrap(modCT_h, CompileType)
    mod = modCT.val::Module
    ast = unwrap(ast_h, JuAST)
    try
        lam_expr = Expr(ast.tree)
        # JuliaSyntax materializes `f(args) do x; body end` as
        # `Expr(:do_lambda, params_tuple, body_block)` — a non-standard
        # head Core.eval can't handle. Rewrite to a real `:->` lambda.
        if lam_expr isa Expr && lam_expr.head === :do_lambda
            lam_expr = Expr(:(->), lam_expr.args[1], lam_expr.args[2])
        end
        fn = Core.eval(mod, lam_expr)
        return makeConstVal(fn)
    catch
        return makeType(Function)
    end
end

# `whichMethod(sig)` -> `Base.which(sig)`. The single Method whose signature
# best matches the lookup tuple. Driver-side because `Base.which` queries
# the live method table.
@nocheck function whichMethod(sig_h::OpaqueHandle{CallSigK})::MethodDef
    return Base.which(sig_h.val)
end

# `includeFile(modCT, path)` -> `Base.include(mod, path)`. Materializes the
# top-level statements (module decls, method defs, etc.) of `path` into
# `mod`. Live-JIT driver does the real `include`; trim drivers would need
# their own implementation (no-op or pre-baked module table). Returns
# nothing — failure throws, caller handles.
@nocheck function includeFile(modCT_h::OpaqueHandle{CompileTypeK}, path::String)::Nothing
    modCT = unwrap(modCT_h, CompileType)
    mod = modCT.val::Module
    Base.include(mod, path)
    return nothing
end

# Construct a fresh Module via `Core.Module(Symbol(name), stdImports,
# defaultUsing)` and wrap it in a CompileType for transport across the seam.
# The analyzer asks for this when it needs a target Module for `includeFile`
# but doesn't want to reference the driver's `Main` directly — important in
# the standalone .so / .exe case where the embedded libjulia's `Main` is
# polluted with the type-checker's own bindings. The driver chooses the
# constructor flavor (stdImports / defaultUsing) so the caller can ask for
# either a real-flavored Main or a clean isolated scratch namespace.
@nocheck function createMainModule(name::String, stdImports::Bool, defaultUsing::Bool)::OpaqueHandle{CompileTypeK}
    m = Core.Module(Symbol(name), stdImports, defaultUsing)
    return OpaqueHandle{CompileTypeK}(makeConstVal(m))
end

# Walk a UnionAll's `.body` chain to its non-UnionAll base, then format
# the full diagnostic suffix. Driver-side because UnionAll peek is target-
# runtime-only.
@nocheck function explainAbstractType(h::OpaqueHandle{CompileTypeK})::String
    base = _type_to_string(unwrap(h, CompileType).typ)
    val = unwrap(h, CompileType).typ
    if val isa UnionAll
        while val isa UnionAll
            val = val.body
        end
        return string(base, ", \nfully applied type should be ", _type_to_string(val))
    end
    return base
end

# ─── Type-constant ops ──────────────────────────────────────────────────────
#
# Each returns a CompileType wrapping a driver-runtime Type constant. The
# inferencer NEVER references `Any` / `Bool` / `Nothing` etc. by name — every
# such use goes through one of these ops so the Type value comes from the
# target runtime, not the analyzer's.

makeAnyType()::CompileType      = makeType(Any)
makeBoolType()::CompileType     = makeType(Bool)
makeNothingType()::CompileType  = makeType(Nothing)
makeExprType()::CompileType     = makeType(Expr)
makeIntType()::CompileType      = makeType(Int)
makeTypeType()::CompileType     = makeType(Type)
makeFunctionType()::CompileType = makeType(Function)
makeBottomTypeOf()::CompileType = makeBottomType()

# ─── Literal-wrap ops ───────────────────────────────────────────────────────
#
# Primitives cross the seam by value; the driver wraps them as const
# CompileTypes here.

wrapLiteralBool(b::Bool)::CompileType       = makeConstVal(b)
wrapLiteralInt(i::Int)::CompileType         = makeConstVal(i)
wrapLiteralFloat64(f::Float64)::CompileType = makeConstVal(f)
wrapLiteralString(s::String)::CompileType   = makeConstVal(s)
wrapLiteralChar(c::Char)::CompileType       = makeConstVal(c)
wrapLiteralNothing()::CompileType           = makeConstVal(nothing)
# Symbols cross as Strings — driver re-interns into ITS Symbol table.
wrapLiteralSymbolName(s::String)::CompileType = makeConstVal(Symbol(s))

# ─── Built-in global lookups ────────────────────────────────────────────────
#
# Specific target-runtime constants the inferencer needs to reference (for
# expanding `&&` / `||` to `&` / `|`, for the `isa` callable in narrowing,
# for `Base.identity` as the default operand, etc.). Each is a dedicated
# op — no generic "resolve any global" that would defeat the seam.

lookupBaseIdentity()::CompileType = makeConstVal(Base.identity)
lookupIsa()::CompileType          = makeConstVal(isa)
lookupBitAnd()::CompileType       = makeConstVal(&)
lookupBitOr()::CompileType        = makeConstVal(|)
lookupAnyValue()::CompileType     = makeConstVal(Any)

# ─── Composite type derivation ──────────────────────────────────────────────

# `makeMatrixOf(eltype) = makeType(Matrix{eltype.typ})`. Driver-side because
# the parametric apply on `Matrix` peeks `eltype.typ` (target-runtime).
@nocheck function makeMatrixOf(h::OpaqueHandle{CompileTypeK})::CompileType
    elt = unwrap(h, CompileType).typ
    return makeType(Matrix{elt})
end

# `makeVectorOf(eltype) = makeType(Vector{eltype.typ})`. Mirrors
# `makeMatrixOf` for 1-D results — used by typed-comprehension support
# (`T[expr for x in xs]` returns `Vector{T}`).
@nocheck function makeVectorOf(h::OpaqueHandle{CompileTypeK})::CompileType
    elt = unwrap(h, CompileType).typ
    return makeType(Vector{elt})
end

# `getFirstParameterOf(h) = makeType(t.parameters[1])` for a DataType.
# Used to extract the element type from `Vector{T}`, etc.
@nocheck function getFirstParameterOf(h::OpaqueHandle{CompileTypeK})::CompileType
    t = unwrap(h, CompileType).typ
    return makeType((t::DataType).parameters[1])
end

# `getTypeParameterOf(h, i) = makeType(t.parameters[i])`. Variable index
# used for tuple-destructure: walks each element type of a Tuple{...} sig.
@nocheck function getTypeParameterOf(h::OpaqueHandle{CompileTypeK}, i::Int)::CompileType
    t = unwrap(h, CompileType).typ
    return makeType((t::DataType).parameters[i])
end

# `removeConstOf(h) = isConst ? makeType(.typ) : h` — strip constness while
# keeping the type-value form. Used when the inferencer wants the "value
# type" without locking to a specific const.
@nocheck function removeConstOf(h::OpaqueHandle{CompileTypeK})::CompileType
    ct = unwrap(h, CompileType)
    if ct.isConst
        return makeType(ct.typ)
    else
        return ct
    end
end

# `tupleDestructInfo(h, n)` — driver-side decision for whether `h` can be
# destructured into `n` slots. Three Bools out:
#   (1) is `.typ` a DataType?
#   (2) does it have at least `n` parameters?
#   (3) is it <: Tuple or <: Pair?
# The inferencer uses these to report the specific failure mode without
# peeking into target-runtime type structure.
@nocheck function tupleDestructInfo(h::OpaqueHandle{CompileTypeK}, n::Int)::Tuple{Bool, Bool, Bool}
    t = unwrap(h, CompileType).typ
    if !(t isa DataType)
        return (false, false, false)
    end
    lenOk = length((t::DataType).parameters) >= n
    isTupleOrPair = (t <: Tuple) || (t <: Pair)
    return (true, lenOk, isTupleOrPair)
end

# Default approveCallSite: every dispatch-clean call is allowed. The
# strict-mode factory (StrictRuntimeContext.jl) overrides this slot to
# enforce stricter rules like `push!(::Vector{T}, ::T) where {T}`.
@nocheck function approveCallSiteDefault(fargs_h::OpaqueHandle{CompileTypeVecK})::String
    return ""
end

# Per-field kwargs default to the live-JIT driver. Tests pass only the
# fields they want to override; adding a new field to RuntimeContext
# does not break existing call sites.
function defaultRuntimeContext(;
    methodsOfFn::MethodsOfFn                       = MethodsOfFn(methodsOf),
    inferReturnTypeFn::InferReturnTypeFn           = InferReturnTypeFn(inferReturnType),
    makeCallSigFn::MakeCallSigFn                   = MakeCallSigFn(makeCallSig),
    makeTupleTypeFn::MakeTupleTypeFn               = MakeTupleTypeFn(makeTupleTypeOf),
    subtypeOfFn::SubtypeOfFn                       = SubtypeOfFn(subtypeOf),
    isAnyFn::IsAnyFn                               = IsAnyFn(isAny),
    isBoolFn::IsBoolFn                             = IsBoolFn(isBool),
    isTypeValueFn::IsTypeValueFn                   = IsTypeValueFn(isTypeValue),
    isUnionFn::IsUnionFn                           = IsUnionFn(isUnion),
    isTupleTypeFn::IsTupleTypeFn                   = IsTupleTypeFn(isTupleType),
    isConcreteFn::IsConcreteFn                     = IsConcreteFn(isConcrete),
    isBottomFn::IsBottomFn                         = IsBottomFn(isBottom),
    hasFieldOfFn::HasFieldOfFn                     = HasFieldOfFn(hasFieldOf),
    getFieldTypeOfFn::GetFieldTypeOfFn             = GetFieldTypeOfFn(getFieldTypeOf),
    constIsTypeValueFn::ConstIsTypeValueFn         = ConstIsTypeValueFn(constIsTypeValue),
    constIsModuleFn::ConstIsModuleFn               = ConstIsModuleFn(constIsModule),
    constModuleIsDefinedFn::ConstModuleIsDefinedFn = ConstModuleIsDefinedFn(constModuleIsDefined),
    constModuleGetFieldFn::ConstModuleGetFieldFn   = ConstModuleGetFieldFn(constModuleGetField),
    constIsFunctionFn::ConstIsFunctionFn           = ConstIsFunctionFn(constIsFunction),
    constIsBuiltinOrIntrinFn::ConstIsBuiltinOrIntrinFn = ConstIsBuiltinOrIntrinFn(constIsBuiltinOrIntrin),
    constEqualsBuiltinEqFn::ConstEqualsBuiltinEqFn = ConstEqualsBuiltinEqFn(constEqualsBuiltinEq),
    constEqualsConstFn::ConstEqualsConstFn         = ConstEqualsConstFn(constEqualsConst),
    liftConstToTypeFn::LiftConstToTypeFn           = LiftConstToTypeFn(liftConstToType),
    getCallableFromConstFn::GetCallableFromConstFn = GetCallableFromConstFn(getCallableFromConst),
    applyTypeOfFn::ApplyTypeOfFn                   = ApplyTypeOfFn(applyTypeOf),
    makeTypeVarOfFn::MakeTypeVarOfFn               = MakeTypeVarOfFn(makeTypeVarOf),
    makeUnionAllOfFn::MakeUnionAllOfFn             = MakeUnionAllOfFn(makeUnionAllOf),
    typeEqFn::TypeEqFn                             = TypeEqFn(typeEq),
    makeUnionOfFn::MakeUnionOfFn                   = MakeUnionOfFn(makeUnionOf),
    splitUnionOfFn::SplitUnionOfFn                 = SplitUnionOfFn(splitUnionOf),
    typeToStringFn::TypeToStringFn                 = TypeToStringFn(typeToString),
    constToStringFn::ConstToStringFn               = ConstToStringFn(constToString),
    explainAbstractTypeFn::ExplainAbstractTypeFn   = ExplainAbstractTypeFn(explainAbstractType),
    evalLambdaInModuleFn::EvalLambdaInModuleFn     = EvalLambdaInModuleFn(evalLambdaInModule),
    whichMethodFn::WhichMethodFn                   = WhichMethodFn(whichMethod),
    includeFileFn::IncludeFileFn                   = IncludeFileFn(includeFile),
    createMainModuleFn::CreateMainModuleFn         = CreateMainModuleFn(createMainModule),
    # Type-constant cluster
    makeAnyTypeFn::MakeAnyTypeFn                   = MakeAnyTypeFn(makeAnyType),
    makeBoolTypeFn::MakeBoolTypeFn                 = MakeBoolTypeFn(makeBoolType),
    makeNothingTypeFn::MakeNothingTypeFn           = MakeNothingTypeFn(makeNothingType),
    makeExprTypeFn::MakeExprTypeFn                 = MakeExprTypeFn(makeExprType),
    makeIntTypeFn::MakeIntTypeFn                   = MakeIntTypeFn(makeIntType),
    makeTypeTypeFn::MakeTypeTypeFn                 = MakeTypeTypeFn(makeTypeType),
    makeFunctionTypeFn::MakeFunctionTypeFn         = MakeFunctionTypeFn(makeFunctionType),
    makeBottomTypeOfFn::MakeBottomTypeOfFn         = MakeBottomTypeOfFn(makeBottomTypeOf),
    builtinMacroResultTypeFn::BuiltinMacroResultTypeFn = BuiltinMacroResultTypeFn(builtinMacroResultType),
    hasUserGetpropertyFn::HasUserGetpropertyFn     = HasUserGetpropertyFn(hasUserGetproperty),
    # Literal-wrap cluster
    wrapLiteralBoolFn::WrapLiteralBoolFn           = WrapLiteralBoolFn(wrapLiteralBool),
    wrapLiteralIntFn::WrapLiteralIntFn             = WrapLiteralIntFn(wrapLiteralInt),
    wrapLiteralFloat64Fn::WrapLiteralFloat64Fn     = WrapLiteralFloat64Fn(wrapLiteralFloat64),
    wrapLiteralStringFn::WrapLiteralStringFn       = WrapLiteralStringFn(wrapLiteralString),
    wrapLiteralCharFn::WrapLiteralCharFn           = WrapLiteralCharFn(wrapLiteralChar),
    wrapLiteralSymbolNameFn::WrapLiteralSymbolNameFn = WrapLiteralSymbolNameFn(wrapLiteralSymbolName),
    wrapLiteralNothingFn::WrapLiteralNothingFn     = WrapLiteralNothingFn(wrapLiteralNothing),
    # Built-in global lookups
    lookupBaseIdentityFn::LookupBaseIdentityFn     = LookupBaseIdentityFn(lookupBaseIdentity),
    lookupIsaFn::LookupIsaFn                       = LookupIsaFn(lookupIsa),
    lookupBitAndFn::LookupBitAndFn                 = LookupBitAndFn(lookupBitAnd),
    lookupBitOrFn::LookupBitOrFn                   = LookupBitOrFn(lookupBitOr),
    lookupAnyValueFn::LookupAnyValueFn             = LookupAnyValueFn(lookupAnyValue),
    # Composite type derivation
    makeMatrixOfFn::MakeMatrixOfFn                 = MakeMatrixOfFn(makeMatrixOf),
    makeVectorOfFn::MakeVectorOfFn                 = MakeVectorOfFn(makeVectorOf),
    getFirstParameterOfFn::GetFirstParameterOfFn   = GetFirstParameterOfFn(getFirstParameterOf),
    getTypeParameterOfFn::GetTypeParameterOfFn     = GetTypeParameterOfFn(getTypeParameterOf),
    removeConstOfFn::RemoveConstOfFn               = RemoveConstOfFn(removeConstOf),
    tupleDestructInfoFn::TupleDestructInfoFn       = TupleDestructInfoFn(tupleDestructInfo),
    # Strategy overlay — default is no-op; strict factory overrides this slot.
    approveCallSiteFn::ApproveCallSiteFn           = ApproveCallSiteFn(approveCallSiteDefault),
)::RuntimeContext
    return RuntimeContext(methodsOfFn, inferReturnTypeFn,
                          makeCallSigFn, makeTupleTypeFn,
                          subtypeOfFn,
                          isAnyFn, isBoolFn, isTypeValueFn,
                          isUnionFn, isTupleTypeFn,
                          isConcreteFn, isBottomFn,
                          hasFieldOfFn, getFieldTypeOfFn,
                          constIsTypeValueFn, constIsModuleFn,
                          constModuleIsDefinedFn, constModuleGetFieldFn,
                          constIsFunctionFn, constIsBuiltinOrIntrinFn,
                          constEqualsBuiltinEqFn, constEqualsConstFn,
                          liftConstToTypeFn, getCallableFromConstFn,
                          applyTypeOfFn, makeTypeVarOfFn,
                          makeUnionAllOfFn, typeEqFn, makeUnionOfFn,
                          splitUnionOfFn,
                          typeToStringFn, constToStringFn,
                          explainAbstractTypeFn, evalLambdaInModuleFn,
                          whichMethodFn, includeFileFn, createMainModuleFn,
                          makeAnyTypeFn, makeBoolTypeFn, makeNothingTypeFn,
                          makeExprTypeFn, makeIntTypeFn, makeTypeTypeFn,
                          makeFunctionTypeFn, makeBottomTypeOfFn,
                          builtinMacroResultTypeFn, hasUserGetpropertyFn,
                          wrapLiteralBoolFn, wrapLiteralIntFn,
                          wrapLiteralFloat64Fn, wrapLiteralStringFn,
                          wrapLiteralCharFn, wrapLiteralSymbolNameFn,
                          wrapLiteralNothingFn,
                          lookupBaseIdentityFn, lookupIsaFn,
                          lookupBitAndFn, lookupBitOrFn, lookupAnyValueFn,
                          makeMatrixOfFn, makeVectorOfFn,
                          getFirstParameterOfFn,
                          getTypeParameterOfFn, removeConstOfFn,
                          tupleDestructInfoFn,
                          approveCallSiteFn)
end
