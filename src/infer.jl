
module MyTest
using JuliaSyntax
include("interface.jl")
banned_list = ["eval","include"]
for i in names(Interface, all = true)
    if !startswith(string(i), "#") && !(string(i) in banned_list)
        try
            Core.eval(Main, :($i = Interface.$(i)))
        catch e
        end
    end
end

mutable struct TestA
    x::Int
    y::Int
    z::Any
end
s = """
x = Vector{Union{Int, Float64, String}}()
y = x[1]
if y isa Int
    println(y + 1)
elseif y isa String
    println(y)
else
    println(y + 1.0)
end
"""

s = """
function inferAssign(eng::Engine, ctx::Context, ast::Assign)::InferResult
    rel = inferExpr(eng, ctx, ast.rhs)
    retctx = rel.ctx
    retnode = rel.node
    var = ast.lhs
    if hasvar(retctx, var.id)
        # this variable is already assigned before, we check type compatibility
        oldval = lookup(retctx, var.id)
        # storage type is unchanged, update current type 
        if !tryMergeFlowType(oldval.typ, retnode)
            error("Incompatble type is assigned")
        end
        newnode = makeAssignFlowNode(ast, retnode)
        # the primary assignment is unchanged
        val = ContextValue(oldval.typ, newnode)
    else
        newnode = makeAssignFlowNode(ast, retnode)
        val = ContextValue(newnode, newnode)
    end
    newctx = update(retctx, var.id, val)
    return InferResult(newctx, newnode)
end

function extractFunDef(ast::JuExpr)::Vector{FunDef}
    e = ast.val
    if e isa Block
        res = Vector{FunDef}()
        return res
    elseif e isa FunDef
        res = Vector{FunDef}()
        push!(res, e)
        return res
    else
        error("Not valid func def")
    end
end
"""
          
ast = Interface.typedConvertAST(Interface.parseJuAST(s))
Interface.testInferForFunction(Interface.extractFunDef(ast)[1], Interface.inferAssign, (Interface.Engine, Interface.Context, Interface.Assign))
Interface.testInferForFunction(Interface.extractFunDef(ast)[2], Interface.extractFunDef, (Interface.JuExpr,))

s2 = """
y = 1
for i in 1:19
    z = 3
    println(i)
end
a = 3
while i > 0
    println(i)
end
"""
ast = Interface.typedConvertAST(Interface.parseJuAST(s2))
rel = Interface.decideScopeVariable(Interface.Context(), ast)
end