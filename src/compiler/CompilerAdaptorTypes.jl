# CompilerAdaptor — public type aliases + RuntimeContext vtable shape.
#
# This file is loaded first so that subsequent files (InferenceDefinition.jl etc.)
# can use the adaptor's opaque names instead of referencing Core.* directly.
# The actual implementations of the adaptor functions live in CompilerAdaptor.jl,
# which is loaded later (after the types from InferenceDefinition.jl are available).
#
# When porting to a future Julia version: if these aliases ever need to point at
# different underlying types, only this file changes.

using FunctionWrappers: FunctionWrapper

const MethodHandle = Core.MethodInstance
const MethodDef    = Core.Method

# ─── Opaque-handle phantom-type machinery ────────────────────────────────────
#
# Every value that crosses the RuntimeContext seam is conceptually an
# opaque handle owned by the driver runtime — the inferencer never
# inspects it. To enforce that at the seam itself, each handle is
# parameterized by a `K<:HandleKind` phantom (a singleton tag).
# `OpaqueHandle{CompileTypeK}` and `OpaqueHandle{MethodDefK}` are distinct
# types — Julia rejects passing one where the other is expected.
#
# Today the wrapped `val` is `Any` because we still flow real
# CompileType/etc. values across the seam. When we move to UInt64
# iddict keys (the "real handle" world), only the `val` field type
# changes — the phantom parameter and FunctionWrapper signatures stay.

abstract type HandleKind end
struct CompileTypeK        <: HandleKind end  # a CompileType
struct CompileTypeVecK     <: HandleKind end  # Vector{CompileType}
struct CompileTypePairVecK <: HandleKind end  # Vector{Pair{Symbol, CompileType}} (kwargs)
struct CallableK           <: HandleKind end  # @nospecialize callable for methodsOf
struct CallSigK            <: HandleKind end  # Julia Tuple type used as method-lookup sig
struct GlobalContextK      <: HandleKind end  # ::GlobalContext (defined later)

struct OpaqueHandle{K<:HandleKind}
    val::Any
end

# Auto-wrap at the seam boundary. FunctionWrapper's call-time conversion
# invokes this when the inferencer hands a raw value into a phantom-typed
# slot. No-op overload prevents double-wrapping.
@inline Base.convert(::Type{OpaqueHandle{K}}, x) where {K<:HandleKind} = OpaqueHandle{K}(x)
@inline Base.convert(::Type{OpaqueHandle{K}}, h::OpaqueHandle{K}) where {K<:HandleKind} = h

# Driver-side extraction. The caller asserts the wrapped value's
# concrete type — `unsafe` because nothing checks the kind matches
# `T`. Kept inline so the unwrap elides through the optimizer.
@inline unwrap(h::OpaqueHandle, ::Type{T}) where T = h.val::T

# ─── RuntimeContext vtable ───────────────────────────────────────────────────
#
# Every operation that reaches into the live JIT (method lookup,
# return-type inference, `Core.eval`, `<:`, etc.) is reached through a
# field below. FunctionWrapper fields are concrete-typed, so reads of
# them dispatch statically — same trick as MacroExpander.
const MethodsOfFn        = FunctionWrapper{Vector{MethodDef},               Tuple{OpaqueHandle{CallableK}}}
const InferReturnTypeFn  = FunctionWrapper{OpaqueHandle{CompileTypeVecK},   Tuple{OpaqueHandle{GlobalContextK}, OpaqueHandle{CallSigK}}}
# Type-construction ops:
#   makeCallSig   -> a Julia Tuple type used as a method-lookup signature
#                    (kwarg case: Tuple{typeof(Core.kwcall), NamedTuple{...}, ...})
#   makeTupleType -> a CompileType wrapping the constructed Tuple type for
#                    value-position tuple expressions in the source.
const MakeCallSigFn      = FunctionWrapper{OpaqueHandle{CallSigK},          Tuple{OpaqueHandle{CompileTypeVecK}, OpaqueHandle{CompileTypePairVecK}}}
const MakeTupleTypeFn    = FunctionWrapper{OpaqueHandle{CompileTypeK},      Tuple{OpaqueHandle{CompileTypeVecK}}}
# Type-query op: `subtypeOf(a, b)` answers `a.typ <: b.typ`.
const SubtypeOfFn        = FunctionWrapper{Bool,                            Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
# Single-arg isa-checks on CompileType handles. Each one is a single
# `===` or `isa` peek on the underlying `.typ`, gathered behind the
# seam so the inferencer never compares against analyzer-runtime
# constants like Any/Bool/Union{}/Tuple.
const IsAnyFn       = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const IsBoolFn      = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const IsTypeValueFn = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const IsUnionFn     = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const IsTupleTypeFn = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}

struct RuntimeContext
    methodsOf::MethodsOfFn
    inferReturnType::InferReturnTypeFn
    makeCallSig::MakeCallSigFn
    makeTupleType::MakeTupleTypeFn
    subtypeOf::SubtypeOfFn
    isAny::IsAnyFn
    isBool::IsBoolFn
    isTypeValue::IsTypeValueFn
    isUnion::IsUnionFn
    isTupleType::IsTupleTypeFn
end
