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

# Opaque-handle marker for values that cross the RuntimeContext seam.
#
# Semantically this is NOT the analyzer's `Any` — the inferencer's world
# does not contain `Any` as a type. Conceptually each value crossing the
# seam is an iddict-resolved handle owned by the driver runtime; the
# inferencer never inspects it. We use Julia's `Any` slot here only
# because Julia has no nominal `OpaqueHandle` type and FunctionWrapper
# requires a concrete element type. When we move to a real handle scheme
# (UInt64 keys into a driver-side IdDict pin table), only this alias
# changes — call sites stay untouched.
const OpaqueHandle = Any

# RuntimeContext — the runtime-isolation seam. Every operation that
# reaches into the live JIT (method lookup, return-type inference,
# `Core.eval`, etc.) eventually moves behind a field here. Phase A
# (the current pass) only wires the first call — `methodsOf` — so the
# struct intentionally has just one field. Each new reroute extends
# this struct and `defaultRuntimeContext()` in CompilerAdaptor.jl.
#
# FunctionWrapper is concrete, so reads of these fields dispatch
# statically — the same trick we already use for MacroExpander.
#
# Where a field's natural output type (e.g. Vector{CompileType}) lives
# in an inference type defined after this file, we erase it to `Any`
# and let the call site re-narrow with `::T`. The FunctionWrapper
# field itself is still concrete, so dispatch into the trampoline
# stays static — the Any only shows up as the trampoline's reported
# return value, which Julia eliminates after the call-site assertion.
# Mirrors the MacroExpander pattern already in InferenceDefinition.jl.
const MethodsOfFn        = FunctionWrapper{Vector{MethodDef}, Tuple{OpaqueHandle}}
const InferReturnTypeFn  = FunctionWrapper{OpaqueHandle,      Tuple{OpaqueHandle, OpaqueHandle}}
# Type-construction ops. Inputs are Vector{CompileType} (or pair-vector for
# kwargs); both vectors arrive across the seam as opaque handles.
# Outputs:
#   makeCallSig  -> a Julia Tuple type used as a method-lookup signature
#                   (kwarg case: Tuple{typeof(Core.kwcall), NamedTuple{...}, ...})
#   makeTupleType -> a CompileType wrapping the constructed Tuple type for
#                    value-position tuple expressions in the source.
const MakeCallSigFn      = FunctionWrapper{OpaqueHandle,      Tuple{OpaqueHandle, OpaqueHandle}}
const MakeTupleTypeFn    = FunctionWrapper{OpaqueHandle,      Tuple{OpaqueHandle}}
# Type-query op. Both args are opaque CompileType handles. Returns whether
# the FIRST arg's underlying type is a subtype of the SECOND's — i.e.
# `subtypeOf(a, b)` answers `a.typ <: b.typ`. Driver-side because `<:`
# peeks `.typ`.
const SubtypeOfFn        = FunctionWrapper{Bool,              Tuple{OpaqueHandle, OpaqueHandle}}
# Single-arg isa-checks on CompileType handles. Each one is a single
# `===` or `isa` peek on the underlying `.typ`, gathered behind the
# seam so the inferencer never compares against analyzer-runtime
# constants like Any/Bool/Union{}/Tuple.
const IsAnyFn       = FunctionWrapper{Bool, Tuple{OpaqueHandle}}
const IsBoolFn      = FunctionWrapper{Bool, Tuple{OpaqueHandle}}
const IsTypeValueFn = FunctionWrapper{Bool, Tuple{OpaqueHandle}}
const IsUnionFn     = FunctionWrapper{Bool, Tuple{OpaqueHandle}}
const IsTupleTypeFn = FunctionWrapper{Bool, Tuple{OpaqueHandle}}

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
