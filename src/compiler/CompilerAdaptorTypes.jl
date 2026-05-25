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
const MethodsOfFn        = FunctionWrapper{Vector{MethodDef}, Tuple{Any}}
const InferReturnTypeFn  = FunctionWrapper{Any,               Tuple{Any, Any}}
# Type-construction ops. Inputs are Vector{CompileType} (or pair-vector for
# kwargs) — typed Any here because CompileType is defined in a later file.
# Outputs:
#   makeCallSig  -> a Julia Tuple type used as a method-lookup signature
#                   (kwarg case: Tuple{typeof(Core.kwcall), NamedTuple{...}, ...})
#   makeTupleType -> a CompileType wrapping the constructed Tuple type for
#                    value-position tuple expressions in the source.
const MakeCallSigFn      = FunctionWrapper{Any,               Tuple{Any, Any}}
const MakeTupleTypeFn    = FunctionWrapper{Any,               Tuple{Any}}

struct RuntimeContext
    methodsOf::MethodsOfFn
    inferReturnType::InferReturnTypeFn
    makeCallSig::MakeCallSigFn
    makeTupleType::MakeTupleTypeFn
end
