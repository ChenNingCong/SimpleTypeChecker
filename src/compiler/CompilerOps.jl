# CompilerOps — analyzer-callable wrappers around the RuntimeContext seam.
#
# RULE: `unwrap` only appears here (and in CompilerAdaptor.jl). The core
# analyzer loop (everything under src/inference/) must NOT call `unwrap`
# directly, and must NOT access `rtctx.*` fields directly when the op
# returns an OpaqueHandle. Instead, the analyzer calls these wrappers,
# which hide the OpaqueHandle plumbing and return concrete analyzer-side
# types (CompileType / Bool / String / etc.).
#
# Wrapper naming: same name as the rtctx field where possible — analyzer
# call `fooOp(rtctx, ...)` (function call) shadows-by-arity the field
# access `rtctx.fooOp(...)` (FunctionWrapper call). When the rtctx field
# uses an `*Of` suffix, the wrapper drops it (`getFieldTypeOf` →
# `getFieldType`) to keep analyzer call sites readable.
#
# The file lives in src/compiler/ so file-path classification answers
# "is this driver code?" — yes, because it touches `unwrap`.

# ─── Bool-returning passthroughs ────────────────────────────────────────────

hasField(rtctx::RuntimeContext, ft::CompileType, p::Symbol)::Bool =
    rtctx.hasFieldOf(ft, String(p))

isConcreteType(rtctx::RuntimeContext, ft::CompileType)::Bool =
    rtctx.isConcrete(ft)

constModuleIsDefined(rtctx::RuntimeContext, modct::CompileType, p::Symbol)::Bool =
    rtctx.constModuleIsDefined(modct, String(p))

tupleDestructInfo(rtctx::RuntimeContext, ct::CompileType, n::Int)::Tuple{Bool, Bool, Bool} =
    rtctx.tupleDestructInfo(ct, n)

checkFieldCompatibility(rtctx::RuntimeContext, dest::CompileType, src::CompileType)::Bool =
    rtctx.subtypeOf(src, dest)

isConstructor(rtctx::RuntimeContext, v::CompileType)::Bool =
    isConstVal(v) && rtctx.constIsTypeValue(v)

isaType(rtctx::RuntimeContext, x::CompileType)::Bool     = rtctx.isTypeValue(x)
isaAny(rtctx::RuntimeContext, x::CompileType)::Bool      = rtctx.isAny(x)
isaBool(rtctx::RuntimeContext, x::CompileType)::Bool     = rtctx.isBool(x)
isaUnion(rtctx::RuntimeContext, x::CompileType)::Bool    = rtctx.isUnion(x)
isaTuple(rtctx::RuntimeContext, x::CompileType)::Bool    = rtctx.isTupleType(x)
isConstModule(rtctx::RuntimeContext, x::CompileType)::Bool = rtctx.constIsModule(x)
isConstFunction(rtctx::RuntimeContext, x::CompileType)::Bool = rtctx.constIsFunction(x)
isConstBuiltinOrIntrin(rtctx::RuntimeContext, x::CompileType)::Bool = rtctx.constIsBuiltinOrIntrin(x)
isConstEqualsBuiltinEq(rtctx::RuntimeContext, x::CompileType)::Bool = rtctx.constEqualsBuiltinEq(x)
isConstEqualsConst(rtctx::RuntimeContext, a::CompileType, b::CompileType)::Bool = rtctx.constEqualsConst(a, b)
isTypeEq(rtctx::RuntimeContext, a::CompileType, b::CompileType)::Bool = rtctx.typeEq(a, b)
isBottomType(rtctx::RuntimeContext, t::CompileType)::Bool = rtctx.isBottom(t)

# ─── String-returning passthroughs ──────────────────────────────────────────

toString(rtctx::RuntimeContext, t::CompileType)::String      = rtctx.typeToString(t)
toConstString(rtctx::RuntimeContext, t::CompileType)::String = rtctx.constToString(t)
explainAbstractType(rtctx::RuntimeContext, t::CompileType)::String = rtctx.explainAbstractType(t)

# ─── CompileType-returning wrappers (these hide `unwrap`) ───────────────────

getFieldType(rtctx::RuntimeContext, ft::CompileType, p::Symbol)::CompileType =
    unwrap(rtctx.getFieldTypeOf(ft, String(p)), CompileType)

constModuleGetField(rtctx::RuntimeContext, modct::CompileType, p::Symbol)::CompileType =
    unwrap(rtctx.constModuleGetField(modct, String(p)), CompileType)

# Type-constant ops — no-arg, return CompileType wrapping a target-runtime Type.
makeAnyType(rtctx::RuntimeContext)::CompileType      = unwrap(rtctx.makeAnyType(), CompileType)
makeBoolType(rtctx::RuntimeContext)::CompileType     = unwrap(rtctx.makeBoolType(), CompileType)
makeNothingType(rtctx::RuntimeContext)::CompileType  = unwrap(rtctx.makeNothingType(), CompileType)
makeExprType(rtctx::RuntimeContext)::CompileType     = unwrap(rtctx.makeExprType(), CompileType)
makeIntType(rtctx::RuntimeContext)::CompileType      = unwrap(rtctx.makeIntType(), CompileType)
makeTypeType(rtctx::RuntimeContext)::CompileType     = unwrap(rtctx.makeTypeType(), CompileType)
makeFunctionType(rtctx::RuntimeContext)::CompileType = unwrap(rtctx.makeFunctionType(), CompileType)
makeBottomTypeOf(rtctx::RuntimeContext)::CompileType = unwrap(rtctx.makeBottomTypeOf(), CompileType)

# Built-in global lookups — no-arg, return CompileType wrapping a const.
lookupBaseIdentity(rtctx::RuntimeContext)::CompileType = unwrap(rtctx.lookupBaseIdentity(), CompileType)
lookupIsa(rtctx::RuntimeContext)::CompileType          = unwrap(rtctx.lookupIsa(), CompileType)
lookupBitAnd(rtctx::RuntimeContext)::CompileType       = unwrap(rtctx.lookupBitAnd(), CompileType)
lookupBitOr(rtctx::RuntimeContext)::CompileType        = unwrap(rtctx.lookupBitOr(), CompileType)
lookupAnyValue(rtctx::RuntimeContext)::CompileType     = unwrap(rtctx.lookupAnyValue(), CompileType)

# Literal-wrap ops — primitive in, CompileType out.
wrapLiteralBool(rtctx::RuntimeContext, b::Bool)::CompileType         = unwrap(rtctx.wrapLiteralBool(b), CompileType)
wrapLiteralInt(rtctx::RuntimeContext, i::Int)::CompileType           = unwrap(rtctx.wrapLiteralInt(i), CompileType)
wrapLiteralFloat64(rtctx::RuntimeContext, f::Float64)::CompileType   = unwrap(rtctx.wrapLiteralFloat64(f), CompileType)
wrapLiteralString(rtctx::RuntimeContext, s::String)::CompileType     = unwrap(rtctx.wrapLiteralString(s), CompileType)
wrapLiteralChar(rtctx::RuntimeContext, c::Char)::CompileType         = unwrap(rtctx.wrapLiteralChar(c), CompileType)
wrapLiteralSymbolName(rtctx::RuntimeContext, s::String)::CompileType = unwrap(rtctx.wrapLiteralSymbolName(s), CompileType)
wrapLiteralNothing(rtctx::RuntimeContext)::CompileType               = unwrap(rtctx.wrapLiteralNothing(), CompileType)

# Type-construction ops on existing CompileType handles.
makeTupleType(rtctx::RuntimeContext, args::Vector{CompileType})::CompileType =
    unwrap(rtctx.makeTupleType(args), CompileType)

makeUnionAllOf(rtctx::RuntimeContext, tv::CompileType, base::CompileType)::CompileType =
    unwrap(rtctx.makeUnionAllOf(tv, base), CompileType)

makeUnionOf(rtctx::RuntimeContext, a::CompileType, b::CompileType)::CompileType =
    unwrap(rtctx.makeUnionOf(a, b), CompileType)

splitUnion(rtctx::RuntimeContext, x::CompileType, y::CompileType)::CompileType =
    unwrap(rtctx.splitUnionOf(x, y), CompileType)

applyTypeOf(rtctx::RuntimeContext, args::Vector{CompileType})::CompileType =
    unwrap(rtctx.applyTypeOf(args), CompileType)

makeTypeVarOf(rtctx::RuntimeContext, name::String)::CompileType =
    unwrap(rtctx.makeTypeVarOf(name), CompileType)

# Composite type derivation.
makeMatrixOf(rtctx::RuntimeContext, eltype::CompileType)::CompileType =
    unwrap(rtctx.makeMatrixOf(eltype), CompileType)

getFirstParameter(rtctx::RuntimeContext, x::CompileType)::CompileType =
    unwrap(rtctx.getFirstParameterOf(x), CompileType)

getTypeParameter(rtctx::RuntimeContext, x::CompileType, i::Int)::CompileType =
    unwrap(rtctx.getTypeParameterOf(x, i), CompileType)

# Constant-inspection — the lift / removeConst pair.
removeConst(rtctx::RuntimeContext, typ::CompileType)::CompileType =
    typ.isConst ? unwrap(rtctx.removeConstOf(typ), CompileType) : typ

lift(rtctx::RuntimeContext, v::CompileType)::CompileType =
    isConstVal(v) ? unwrap(rtctx.liftConstToType(v), CompileType) :
                    error("Internal error : lift called on non-const CompileType")

# Lambda evaluation. JuAST crosses as OpaqueHandle{JuASTK}; wrap it here so
# the analyzer just passes the raw JuAST + module CompileType.
evalLambdaInModule(rtctx::RuntimeContext, modCT::CompileType, ast::JuAST)::CompileType =
    unwrap(rtctx.evalLambdaInModule(modCT, OpaqueHandle{JuASTK}(ast)), CompileType)

# Inference primitive. Returns Vector{CompileType}.
inferReturnType(rtctx::RuntimeContext, ctx::GlobalContext, sig_h::OpaqueHandle{CallSigK})::Vector{CompileType} =
    unwrap(rtctx.inferReturnType(ctx, sig_h), Vector{CompileType})

# Method-lookup signature builder. The CallSigK handle stays opaque on the
# analyzer side — the only consumer is the cache lookup + whichMethod, both
# of which take the handle directly. So this returns the OpaqueHandle by
# design (it's a transport token, not an analyzer value).
makeCallSig(rtctx::RuntimeContext,
            fargs::Vector{CompileType},
            kwargs::Vector{Pair{Symbol, CompileType}})::OpaqueHandle{CallSigK} =
    rtctx.makeCallSig(fargs, kwargs)

# Method lookup via the membrane. CallSigK handle in, MethodDef out.
whichMethod(rtctx::RuntimeContext, sig_h::OpaqueHandle{CallSigK})::MethodDef =
    rtctx.whichMethod(sig_h)

# Source-file ingestion (live-JIT does Base.include; trim rtctx may stub).
includeFile(rtctx::RuntimeContext, modCT::CompileType, path::String)::Nothing =
    rtctx.includeFile(modCT, path)

# Fresh Module construction via the seam. Lets standalone entries get a
# scratch Module without naming `Main` directly — the driver decides whether
# the new module is real-flavored (Base imports + default using) or a clean
# isolated namespace. Use `:Main` as the default name to mirror the
# conventional include target.
createMainModule(rtctx::RuntimeContext, name::String, stdImports::Bool, defaultUsing::Bool)::CompileType =
    unwrap(rtctx.createMainModule(name, stdImports, defaultUsing), CompileType)
createMainModule(rtctx::RuntimeContext)::CompileType =
    createMainModule(rtctx, "Main", true, true)

# Callable extraction for methodsOf.
getCallableFromConst(rtctx::RuntimeContext, v::CompileType)::OpaqueHandle{CallableK} =
    rtctx.getCallableFromConst(v)

# Methods-of dispatch. The CallableK handle stays opaque (it carries an Any
# pointer to the callable on the driver side). Returns Vector{MethodDef}.
methodsOf(rtctx::RuntimeContext, callable::OpaqueHandle{CallableK})::Vector{MethodDef} =
    rtctx.methodsOf(callable)
