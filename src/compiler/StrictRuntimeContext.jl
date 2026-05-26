# Strict-mode rtctx factory — PoC for membrane-as-strategy-overlay.
#
# The default rtctx (`defaultRuntimeContext()`) approves every dispatch-clean
# call. A "strict mode" tightens that: certain Base operations like
# `push!(::Vector{T}, ::T)` that Julia accepts via implicit `convert(T, x)`
# get rejected when the actual arg type isn't `T`.
#
# Implementation: wrap the default rtctx and override only the
# `approveCallSite` field. The driver-side rule peeks `.typ` (allowed —
# this is driver code) and returns a rejection message when the rule
# matches but the types don't.
#
# Adding a new rule = adding an isa-test in `approveCallSiteStrict`. No
# changes to the analyzer or to other membrane ops. Composition with
# tracing/recording etc. would similarly wrap one field at a time.

# Driver-side strict approval. Peeks .typ — explicitly allowed here, this
# is the membrane's driver half. Returns "" if no rule triggers.
@nocheck function approveCallSiteStrict(fargs_h::OpaqueHandle{CompileTypeVecK})::String
    fargs = unwrap(fargs_h, Vector{CompileType})
    if length(fargs) < 2
        return ""
    end

    # Rule 1: push!(v::Vector{T}, x) requires typeof(x) == T (no convert).
    # The callee CompileType is a Const wrapping the function value.
    callee_ct = fargs[1]
    if !callee_ct.isConst
        return ""
    end
    callee = callee_ct.val
    if callee === Base.push! && length(fargs) >= 3
        vec_ct = fargs[2]
        item_ct = fargs[3]
        vec_t = vec_ct.typ
        if vec_t isa DataType && vec_t <: Vector && length(vec_t.parameters) >= 1
            T = vec_t.parameters[1]
            item_t = item_ct.isConst ? typeof(item_ct.val) : item_ct.typ
            if !(item_t === T)
                # Compose message via ccall-friendly String building.
                return "push!(::Vector{$(T)}, ::$(item_t)) requires exact element-type match (would implicitly convert)"
            end
        end
    end

    return ""
end

# Public factory. Take any base rtctx (defaults to the live-JIT one) and
# return a new rtctx with only `approveCallSite` swapped. All other ops
# pass through unchanged.
function strictRuntimeContext(base::RuntimeContext = defaultRuntimeContext())::RuntimeContext
    return RuntimeContext(
        base.methodsOf, base.inferReturnType,
        base.makeCallSig, base.makeTupleType,
        base.subtypeOf,
        base.isAny, base.isBool, base.isTypeValue,
        base.isUnion, base.isTupleType,
        base.isConcrete, base.isBottom,
        base.hasFieldOf, base.getFieldTypeOf,
        base.constIsTypeValue, base.constIsModule,
        base.constModuleIsDefined, base.constModuleGetField,
        base.constIsFunction, base.constIsBuiltinOrIntrin,
        base.constEqualsBuiltinEq, base.constEqualsConst,
        base.liftConstToType, base.getCallableFromConst,
        base.applyTypeOf, base.makeTypeVarOf,
        base.makeUnionAllOf, base.typeEq, base.makeUnionOf,
        base.splitUnionOf,
        base.typeToString, base.constToString,
        base.explainAbstractType, base.evalLambdaInModule,
        base.whichMethod, base.includeFile, base.createMainModule,
        base.makeAnyType, base.makeBoolType, base.makeNothingType,
        base.makeExprType, base.makeIntType, base.makeTypeType,
        base.makeFunctionType, base.makeBottomTypeOf,
        base.wrapLiteralBool, base.wrapLiteralInt,
        base.wrapLiteralFloat64, base.wrapLiteralString,
        base.wrapLiteralChar, base.wrapLiteralSymbolName,
        base.wrapLiteralNothing,
        base.lookupBaseIdentity, base.lookupIsa,
        base.lookupBitAnd, base.lookupBitOr, base.lookupAnyValue,
        base.makeMatrixOf, base.getFirstParameterOf,
        base.getTypeParameterOf, base.removeConstOf,
        base.tupleDestructInfo,
        ApproveCallSiteFn(approveCallSiteStrict),
    )
end
