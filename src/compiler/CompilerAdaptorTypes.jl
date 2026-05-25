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
const MethodsOfFn = FunctionWrapper{Vector{MethodDef}, Tuple{Any}}

struct RuntimeContext
    methodsOf::MethodsOfFn
end
