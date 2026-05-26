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
struct JuASTK              <: HandleKind end  # a JuAST node (analyzer-owned, carried across as opaque)

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
# Name args cross the seam as Strings. Symbols are interned per-runtime —
# a Symbol from one runtime is not `===` to a Symbol from another. The
# driver does `Symbol(s)` on its side to look up by name.
const HasFieldOfFn     = FunctionWrapper{Bool,                       Tuple{OpaqueHandle{CompileTypeK}, String}}
const GetFieldTypeOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, String}}

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
# Module accessors operate on a const CompileType handle wrapping a Module.
# The Module itself never crosses into the inferencer — it stays as part of
# the opaque `.val`. `constModuleIsDefined` asks `isdefined(mod, sym)`;
# `constModuleGetField` returns a fresh CompileType handle (const if the
# binding is const, type-value Any otherwise).
const ConstModuleIsDefinedFn   = FunctionWrapper{Bool,                       Tuple{OpaqueHandle{CompileTypeK}, String}}
const ConstModuleGetFieldFn    = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, String}}
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
const MakeTypeVarOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{String}}
const MakeUnionAllOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
const TypeEqFn        = FunctionWrapper{Bool,                       Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
const MakeUnionOfFn   = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}
# `splitUnionOf(x, y)` -> `makeType(Union{collectUnion(x.typ) \ collectUnion(y.typ) ...})`.
# Removes a Union-shaped narrowing target from x. Driver-side because Union
# decomposition peeks `.a` / `.b`.
const SplitUnionOfFn  = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{CompileTypeK}}}

# ─── Display ops ─────────────────────────────────────────────────────────────
#
# The driver owns all `Base.show` / `string` / `repr` calls on `.val` / `.typ`.
# The inferencer receives a `String` back — Strings cross the seam by value.
#
#   typeToString  -> repr of `.typ` (a Type)
#   constToString -> repr of `.val` (a const value, may not be a Type)
const TypeToStringFn  = FunctionWrapper{String, Tuple{OpaqueHandle{CompileTypeK}}}
const ConstToStringFn = FunctionWrapper{String, Tuple{OpaqueHandle{CompileTypeK}}}
# `explainAbstractType(typ)` -> diagnostic string. Walks `typ.typ` if it's
# a UnionAll, unwraps to the body, and appends "fully applied type should
# be <T>". Lives driver-side because the UnionAll walk peeks `.body`.
const ExplainAbstractTypeFn = FunctionWrapper{String, Tuple{OpaqueHandle{CompileTypeK}}}

# ─── Lambda eval ────────────────────────────────────────────────────────────
#
# `evalLambdaInModule(modCT, ast)` reconstructs a Julia Expr from `ast.tree`
# and evals it in the Module wrapped by `modCT`. Returns a const CompileType
# wrapping the resulting anonymous function. On failure, returns the
# Function type-value (caller falls back to opaque Function).
#
# `ast` crosses the seam as an OpaqueHandle{JuASTK} — the JuAST stays
# analyzer-side but `Expr(ast.tree)` happens driver-side via the handle.
const EvalLambdaInModuleFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, OpaqueHandle{JuASTK}}}

# ─── Source-file ingestion + method lookup ─────────────────────────────────
#
# `whichMethod(sig)` -> `Base.which(sig)`: the single Method whose signature
# best matches `sig`. Used by mapAST2Method when attaching a parsed function
# definition to a live method-table entry. CallSigK because the inferencer
# already constructs the Tuple-shaped lookup signature via makeCallSig and
# similar; whichMethod is the natural sink for that handle type.
#
# `includeFile(modCT, path)` -> `Base.include(mod, path)`: materializes the
# modules/methods declared in `path` into the live runtime of `mod`. Live-JIT
# driver does the real `include` so a fresh source file's `module CaseTest`
# becomes a reachable binding before mapAST2Method walks it. A trim-mode
# driver could no-op (binary's image must pre-bake the modules) or fail
# loudly. Either way the analyzer stays clean.
const WhichMethodFn = FunctionWrapper{MethodDef, Tuple{OpaqueHandle{CallSigK}}}
const IncludeFileFn = FunctionWrapper{Nothing,   Tuple{OpaqueHandle{CompileTypeK}, String}}

# ─── Type-constant ops ──────────────────────────────────────────────────────
#
# The inferencer's `Any` is not the target runtime's `Any` — same name, two
# different Type tables. Each well-known type constant the inferencer wants
# to widen to / compare against has a dedicated membrane op that returns a
# CompileType wrapping the target-runtime Type. The inferencer never spells
# `Any`, `Bool`, `Nothing`, etc. literally.
const MakeAnyTypeFn      = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeBoolTypeFn     = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeNothingTypeFn  = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeExprTypeFn     = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeIntTypeFn      = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeTypeTypeFn     = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeFunctionTypeFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const MakeBottomTypeOfFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}

# ─── Literal-wrap ops ───────────────────────────────────────────────────────
#
# Primitive AST literal values cross the seam as primitives. The driver
# constructs the target-runtime CompileType via `makeConstVal` on its side.
# Symbols cross as Strings — driver interns into its own Symbol table.
const WrapLiteralBoolFn       = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{Bool}}
const WrapLiteralIntFn        = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{Int}}
const WrapLiteralFloat64Fn    = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{Float64}}
const WrapLiteralStringFn     = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{String}}
const WrapLiteralCharFn       = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{Char}}
const WrapLiteralSymbolNameFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{String}}
const WrapLiteralNothingFn    = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}

# ─── Built-in global lookups ────────────────────────────────────────────────
#
# Specific well-known target-runtime values the inferencer needs to compare
# against or use as literals: Base.identity, the `isa` callable, `&` / `|`
# bit ops, the `Any` const-as-value. Each is a dedicated op so the seam
# stays explicit (no generic "lookup any global").
const LookupBaseIdentityFn = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const LookupIsaFn          = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const LookupBitAndFn       = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const LookupBitOrFn        = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}
const LookupAnyValueFn     = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{}}

# ─── Composite type derivation ──────────────────────────────────────────────
#
# `makeMatrixOf(eltype)` -> `makeType(Matrix{eltype.typ})` — replaces an
#   inferencer-side `Matrix{typ.typ}` peek (currently a self-check fire).
# `getFirstParameterOf(h)` -> `makeType(t.parameters[1])` for a DataType.
# `getTypeParameterOf(h, i)` -> `makeType(t.parameters[i])` (variable index;
#   used in tuple-destructure to wrap each element type).
# `removeConstOf(h)` -> `makeType(t.typ)` if isConst else t itself.
const MakeMatrixOfFn         = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}}}
const GetFirstParameterOfFn  = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}}}
const GetTypeParameterOfFn   = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}, Int}}
const RemoveConstOfFn        = FunctionWrapper{OpaqueHandle{CompileTypeK}, Tuple{OpaqueHandle{CompileTypeK}}}
# `tupleDestructInfo(h, n)` -> `(isDataType, length(t.parameters) >= n,
# (t <: Tuple || t <: Pair))`. Decides whether the inferencer can destructure
# `h` into `n` slots. Returns three Bools so the inferencer can report the
# specific failure mode without peeking `.typ`/`.parameters`.
const TupleDestructInfoFn    = FunctionWrapper{Tuple{Bool, Bool, Bool}, Tuple{OpaqueHandle{CompileTypeK}, Int}}

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
    constModuleIsDefined::ConstModuleIsDefinedFn
    constModuleGetField::ConstModuleGetFieldFn
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
    splitUnionOf::SplitUnionOfFn
    # Display cluster
    typeToString::TypeToStringFn
    constToString::ConstToStringFn
    explainAbstractType::ExplainAbstractTypeFn
    evalLambdaInModule::EvalLambdaInModuleFn
    whichMethod::WhichMethodFn
    includeFile::IncludeFileFn
    # Type-constant cluster
    makeAnyType::MakeAnyTypeFn
    makeBoolType::MakeBoolTypeFn
    makeNothingType::MakeNothingTypeFn
    makeExprType::MakeExprTypeFn
    makeIntType::MakeIntTypeFn
    makeTypeType::MakeTypeTypeFn
    makeFunctionType::MakeFunctionTypeFn
    makeBottomTypeOf::MakeBottomTypeOfFn
    # Literal-wrap cluster
    wrapLiteralBool::WrapLiteralBoolFn
    wrapLiteralInt::WrapLiteralIntFn
    wrapLiteralFloat64::WrapLiteralFloat64Fn
    wrapLiteralString::WrapLiteralStringFn
    wrapLiteralChar::WrapLiteralCharFn
    wrapLiteralSymbolName::WrapLiteralSymbolNameFn
    wrapLiteralNothing::WrapLiteralNothingFn
    # Built-in global lookups
    lookupBaseIdentity::LookupBaseIdentityFn
    lookupIsa::LookupIsaFn
    lookupBitAnd::LookupBitAndFn
    lookupBitOr::LookupBitOrFn
    lookupAnyValue::LookupAnyValueFn
    # Composite type derivation
    makeMatrixOf::MakeMatrixOfFn
    getFirstParameterOf::GetFirstParameterOfFn
    getTypeParameterOf::GetTypeParameterOfFn
    removeConstOf::RemoveConstOfFn
    tupleDestructInfo::TupleDestructInfoFn
end
