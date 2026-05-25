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
# Composite type-query ops:
#   isConcrete -> isconcretetype(t.typ) || t.typ <: Type
#                 (the "type can flow into typed code" check)
#   isBottom   -> t.typ === Union{}  (replaces what was once Inference's
#                 isBottomType helper; the Union{} on the inferencer side
#                 is the analyzer-runtime's Union{}, not the target's)
const IsConcreteFn = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const IsBottomFn   = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
# Field-shape ops on a CompileType:
#   hasFieldOf       -> hasfield(t.typ, name)
#   getFieldTypeOf   -> makeType(fieldtype(t.typ, name))  (wrapped back as a handle)
# Name args cross the seam as plain Symbols — Symbols are interned in the
# driver runtime and shared by reference, so they don't need a handle wrap.
const HasFieldOfFn     = FunctionWrapper{Bool,                       Tuple{OpaqueHandle{CompileTypeK}, Symbol}}
const GetFieldTypeOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, Symbol}}

# ─── Constant-inspection ops ─────────────────────────────────────────────────
#
# Each consumer of `CompileType.val` gets a specific membrane op — never
# expose a generic `getConstVal::Any` (that would defeat the no-dyn-dispatch
# rule the inferencer is being scrubbed for). Every op below cracks `.val`
# in the driver runtime and returns a primitive (Bool / OpaqueHandle / etc.).
#
#   constIsTypeValue       -> `.val isa Type`
#   constIsModule          -> `.val isa Module`
#   constIsFunction        -> `.val isa Function`
#   constIsBuiltinOrIntrin -> `.val isa Core.Builtin || .val isa Core.IntrinsicFunction`
#   constEqualsBuiltinEq   -> `.val === ===` || `.val === !==` (used in diagnostics)
#   constEqualsConst       -> `a.val === b.val` (const-value equality)
#   liftConstToType        -> `makeType(.val)` (const Type-value → type-value handle)
#   getCallableFromConst   -> rewrap `.val` as a CallableK handle for methodsOf
const ConstIsTypeValueFn       = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstIsModuleFn          = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstIsFunctionFn        = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstIsBuiltinOrIntrinFn = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstEqualsBuiltinEqFn   = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstEqualsConstFn       = FunctionWrapper{Bool, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
const LiftConstToTypeFn        = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}}}
const GetCallableFromConstFn   = FunctionWrapper{OpaqueHandle{CallableK},    Tuple{OpaqueHandle{CompileTypeK}}}

# ─── Type-construction ops ───────────────────────────────────────────────────
#
#   applyTypeOf   -> `Core.apply_type(args...)` wrapped as a const CompileType handle.
#                    Returns a bottom-CompileType handle on failure (caller checks via isBottom).
#   makeTypeVarOf -> `makeConstVal(TypeVar(name))` (used in `where` binding)
#   makeUnionAllOf -> `makeConstVal(UnionAll(tv.val, base.val))`
#   typeEq        -> `a.typ === b.typ` (driver-runtime type identity)
#   makeUnionOf   -> `makeType(Union{a.typ, b.typ})` (driver-runtime union construction)
const ApplyTypeOfFn   = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeVecK}}}
const MakeTypeVarOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{Symbol}}
const MakeUnionAllOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
const TypeEqFn        = FunctionWrapper{Bool,                       Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
const MakeUnionOfFn   = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}

# ─── Display ops ─────────────────────────────────────────────────────────────
#
# The driver owns all `Base.show` / `string` / `repr` calls on `.val` / `.typ`.
# The inferencer receives a `String` back — Strings cross the seam by value.
#
#   typeToString  -> repr of `.typ` (a Type)
#   constToString -> repr of `.val` (a const value, may not be a Type)
const TypeToStringFn  = FunctionWrapper{String, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstToStringFn = FunctionWrapper{String, Tuple{OpaqueHandle{CompileTypeK}}}

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
    isConcrete::IsConcreteFn
    isBottom::IsBottomFn
    hasFieldOf::HasFieldOfFn
    getFieldTypeOf::GetFieldTypeOfFn
    # Constant-inspection cluster
    constIsTypeValue::ConstIsTypeValueFn
    constIsModule::ConstIsModuleFn
    constIsFunction::ConstIsFunctionFn
    constIsBuiltinOrIntrin::ConstIsBuiltinOrIntrinFn
    constEqualsBuiltinEq::ConstEqualsBuiltinEqFn
    constEqualsConst::ConstEqualsConstFn
    liftConstToType::LiftConstToTypeFn
    getCallableFromConst::GetCallableFromConstFn
    # Type-construction cluster
    applyTypeOf::ApplyTypeOfFn
    makeTypeVarOf::MakeTypeVarOfFn
    makeUnionAllOf::MakeUnionAllOfFn
    typeEq::TypeEqFn
    makeUnionOf::MakeUnionOfFn
    # Display cluster
    typeToString::TypeToStringFn
    constToString::ConstToStringFn
end
