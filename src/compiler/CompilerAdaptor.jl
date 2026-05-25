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
    # Seed-frame CodeInfos (one per matched method). Tracked so that when
    # the user opts into `printDispatchTrace`, we can walk these in addition
    # to interp.codegen — the seed's IR is the most likely place the user
    # call's dispatch issues show up.
    seedCIs = Any[]
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

# Seam-facing variant — receives phantom-typed handles from FunctionWrapper,
# unwraps and delegates into the raw 2-arg method above. The inferencer
# never calls this directly; FunctionWrapper invokes it via the
# rtctx.inferReturnType field.
function inferReturnType(ctx_h::OpaqueHandle{GlobalContextK}, sig_h::OpaqueHandle{CallSigK})::Vector{CompileType}
    return inferReturnType(unwrap(ctx_h, GlobalContext), sig_h.val)
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
    return unwrap(h, CompileType).typ === Bool
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
@nocheck function hasFieldOf(h::OpaqueHandle{CompileTypeK}, p::Symbol)::Bool
    return hasfield(unwrap(h, CompileType).typ, p)
end

@nocheck function getFieldTypeOf(h::OpaqueHandle{CompileTypeK}, p::Symbol)::CompileType
    return makeType(fieldtype(unwrap(h, CompileType).typ, p))
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

@nocheck function constModuleIsDefined(h::OpaqueHandle{CompileTypeK}, p::Symbol)::Bool
    mod = unwrap(h, CompileType).val::Module
    return isdefined(mod, p)
end

@nocheck function constModuleGetField(h::OpaqueHandle{CompileTypeK}, p::Symbol)::CompileType
    mod = unwrap(h, CompileType).val::Module
    if isconst(mod, p)
        return makeConstVal(getproperty(mod, p))
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
@nocheck function makeTypeVarOf(name::Symbol)::CompileType
    return makeConstVal(TypeVar(name))
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
                          typeToStringFn, constToStringFn)
end
