Core.eval(SimpleTypeChecker.Inference, quote
function expander_view(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    args = FlowNode[]
    for i in ast.args
       rel = inferExpr(eng, ctx, i)
       ctx = rel.ctx
       push!(args, rel.node)
    end
    ms = MethodCallStruct(makeLiteralFlowNode(ast, makeConstJuASTVal(Base.maybeview)), args)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        # TODO : report mismatched method table here
        reportErrorFunCall(eng, ast, ms, length(mm), false)
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorBroadcastBottom(eng, ast, ms)
    end
    node = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function expander_nocheck(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    InferResult(ctx, makeLiteralFlowNode(ast, makeConstJuASTVal(nothing)))
end

function expander_inline(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    return inferExpr(ast.args[1])
end

addGlobalExpander!(Base, Symbol("@view"), expander_view)
addGlobalExpander!(Base, Symbol("@inline"), expander_inline)
addGlobalExpander!(@__MODULE__, Symbol("@nocheck"), expander_nocheck)

end)