const JuAST = Main.SimpleTypeChecker.Inference.TypedAST
const JuExpr = subtypes(MyTest.SimpleTypeChecker.Inference.TypedAST)
open("walk.jl", write = true) do f
    l = string.(map(x->split(x, '.')[end], string.(z)))
    for i in 1:length(l)
        body = String[]
        for name in fieldnames(z[i])
            t = fieldtype(z[i], name)
            if t == JuExpr
                push!(body, "  walkExpr(walker, ast.$(name))")
            elseif t == Vector{JuExpr}
                push!(body, "  for i in ast.$(name)\n    walkExpr(walker, i)\n  end")
            elseif t == Union{Nothing, JuExpr}
                nameval = "$(name)val"
                push!(body, "  $nameval = ast.$name\n  if $nameval isa Nothing\n  else\n    walkExpr(walker, $nameval)\n  end")
            end
        end
        tt = l[i]
        write(f, 
        """
        function walk$tt(walker::JuExprWalker, ex::JuExpr, ast::$tt)
        $(join(body,'\n'))
        end\n
        """)
    end
end
