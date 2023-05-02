function walkArrayRef(walker::JuExprWalker, ex::JuExpr, ast::ArrayRef)
  walkExpr(walker, ast.arr)
  for i in ast.i
    walkExpr(walker, i)
  end
end

function walkArraySet(walker::JuExprWalker, ex::JuExpr, ast::ArraySet)
  walkExpr(walker, ast.arr)
  for i in ast.i
    walkExpr(walker, i)
  end
  walkExpr(walker, ast.v)
end

function walkAssign(walker::JuExprWalker, ex::JuExpr, ast::Assign)
  walkExpr(walker, ast.rhs)
end

function walkBlock(walker::JuExprWalker, ex::JuExpr, ast::Block)
  for i in ast.stmts
    walkExpr(walker, i)
  end
end

function walkBreakStmt(walker::JuExprWalker, ex::JuExpr, ast::BreakStmt)

end

function walkContinueStmt(walker::JuExprWalker, ex::JuExpr, ast::ContinueStmt)

end

function walkCurlyCall(walker::JuExprWalker, ex::JuExpr, ast::CurlyCall)
  walkExpr(walker, ast.f)
  for i in ast.args
    walkExpr(walker, i)
  end
end

function walkDeclaration(walker::JuExprWalker, ex::JuExpr, ast::Declaration)
  typval = ast.typ
  if typval isa Nothing
  else
    walkExpr(walker, typval)
  end
end

function walkForStmt(walker::JuExprWalker, ex::JuExpr, ast::ForStmt)
  walkExpr(walker, ast.iter)
  walkExpr(walker, ast.body)
end

function walkFunCall(walker::JuExprWalker, ex::JuExpr, ast::FunCall)
  walkExpr(walker, ast.f)
  for i in ast.args
    walkExpr(walker, i)
  end
  for i in ast.kwargs
    walkExpr(walker, i)
  end
end

function walkFunDef(walker::JuExprWalker, ex::JuExpr, ast::FunDef)
  rtval = ast.rt
  if rtval isa Nothing
  else
    walkExpr(walker, rtval)
  end
  walkExpr(walker, ast.body)
end

function walkGetProperty(walker::JuExprWalker, ex::JuExpr, ast::GetProperty)
  walkExpr(walker, ast.x)
end

function walkIfStmt(walker::JuExprWalker, ex::JuExpr, ast::IfStmt)
  else_val = ast.else_
  if else_val isa Nothing
  else
    walkExpr(walker, else_val)
  end
end

function walkLiteral(walker::JuExprWalker, ex::JuExpr, ast::Literal)

end

function walkModDef(walker::JuExprWalker, ex::JuExpr, ast::ModDef)
  walkExpr(walker, ast.stmts)
end

function walkReturn(walker::JuExprWalker, ex::JuExpr, ast::Return)
  eval = ast.e
  if eval isa Nothing
  else
    walkExpr(walker, eval)
  end
end

function walkSetProperty(walker::JuExprWalker, ex::JuExpr, ast::SetProperty)
  walkExpr(walker, ast.x)
  walkExpr(walker, ast.v)
end

function walkTypedAssert(walker::JuExprWalker, ex::JuExpr, ast::TypedAssert)
  walkExpr(walker, ast.lhs)
  walkExpr(walker, ast.rhs)
end

function walkTypedAssign(walker::JuExprWalker, ex::JuExpr, ast::TypedAssign)
  walkExpr(walker, ast.typ)
  walkExpr(walker, ast.rhs)
end

function walkVar(walker::JuExprWalker, ex::JuExpr, ast::Var)

end

function walkWhileStmt(walker::JuExprWalker, ex::JuExpr, ast::WhileStmt)
  walkExpr(walker, ast.cond)
  walkExpr(walker, ast.body)
end

