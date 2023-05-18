
#=
Core.eval(MyTest.Wrapper,
quote
    const I = SimpleTypeChecker.Inference
    function refff(x::I.FlowNode)
        vv = x.typ
        if vv.isConst
            return "Const($(repr(vv.val)))"
        else
            return repr(vv.val)
        end
    end
    function ff(ast::I.JuAST, node)::String
        locstr = I.formatLocation(ast.loc)
        str = "<p>$locstr</p>"
        if !(node isa Nothing)
            str *= "<p>Inferred:</p><p>$(refff(node[1]))</p>"
        end
        return str
    end
    let
    ctx = I.GlobalContext()
    mi = which(sin, (Float64,)).specializations[1]
    mod = Base
    e = I.Engine(ctx, mi, mod)
    c = I.Context()
    str = """
    begin
        x = 1
        y = 2
        z = (x, y)
        x += 1
        l = 'a'
        k = :x
        o = quote a end
        p = Vector{Int}(undef, 1)
        sort(p;rev=true)
        w = Vector{Union{Int, Nothing}}()[1]
        if w isa Int
            print(w)
            k = 1
        else
            error()
        end
        p[1] += 1
        p[1] += "x"
        (x1, (x2, x3)) = (3,(4,'a'))
        x1 = 'a'
        x1 = 1.0
    end
    """
    ast = SimpleTypeChecker.SyntaxAdaptor.parseAndConstructHTML(str, "none", joinpath(@__DIR__, "../src/www/")).args[1]
    try
        r = I.inferExpr(e, c, ast)
        I.displayContext(stdout, r.ctx)
        f = SimpleTypeChecker.Server.QueryFunctor(ff, ast, e.flowMapping)
        SimpleTypeChecker.Server.handleQuery(f)
    catch e
        if e isa I.InferenceError
            println("────────────────────────────────────────────────────────────────")
            print(String(take!(ctx.errio.io)))
            println("────────────────────────────────────────────────────────────────")
        elseif e isa I.SyntaxError
            println(e.msg)
        else
            rethrow(e)
        end
    end
    
end
end
)

=#

#=
Core.eval(MyTest.Wrapper,
quote
    include("test.jl")
    ctx = SimpleTypeChecker.Inference.GlobalContext()
    SimpleTypeChecker.Inference.addFile!(ctx, TTest, abspath("test/test.jl"))
    SimpleTypeChecker.Inference.runCheck!(ctx)
    for (k,v) in ctx.hasChecked
        if v isa SimpleTypeChecker.Inference.InferenceError
            z = split(string(typeof(v)), '.')[end]
            x = z[length("InferenceError")+1:end]
            x = "displayError" * x
            f = getproperty(SimpleTypeChecker.Inference, Symbol(x))
            f(v)
        end
    end
    println(String(take!(ctx.errio.io)))
end
)
=#
#=
Core.eval(MyTest.Wrapper,
quote
    include("test.jl")
    ctx = SimpleTypeChecker.Inference.GlobalContext()
    SimpleTypeChecker.Inference.addFile!(ctx, TTest, abspath("test/test.jl"))
    for (meth, def) in ctx.methodDefs
        local sictx
        try
            sictx = SimpleTypeChecker.Inference.analyzeScopeVariable(ctx, def)
        catch e
            println(String(take!(ctx.errio.io)))
            rethrow(e)
        end
        println("For method : ", meth)
        for (_, info) in sort(collect(sictx.infos);by = x->x.second.parent.loc.span)
            println(info)
        end
        println("────────────────────────────────────────────────────────────────")
    end
    
    SimpleTypeChecker.Inference.runCheck!(ctx)
end
)
=#
module MyTest
    include("../src/SimpleTypeChecker.jl")
end
Core.eval(MyTest,
quote
    ctx = SimpleTypeChecker.Inference.GlobalContext()
    const path = abspath(joinpath(@__DIR__, ".."))
    SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/JSAdaptor.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/TreeQuery.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceDefinition.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/Inference.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceErrorUtility.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceError.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/ScopeChecking.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, SimpleTypeChecker.Server, joinpath(path, "src/server/SimpleHTTPServer.jl"))
SimpleTypeChecker.Inference.runCheck!(ctx)
#=
    for (meth, def) in ctx.methodDefs
        local sictx
        try
            sictx = SimpleTypeChecker.Inference.analyzeScopeVariable(ctx, def)
        catch e
            println(String(take!(ctx.errio.io)))
            rethrow(e)
        end
        println("For method : ", meth)
        for (_, info) in sort(collect(sictx.infos);by = x->x.second.parent.loc.span)
            println(info)
        end
        println("────────────────────────────────────────────────────────────────")
    end
    
    SimpleTypeChecker.Inference.runCheck!(ctx)
    =#
end
)


Core.eval(MyTest, quote
ctx = SimpleTypeChecker.Inference.GlobalContext()
const path = abspath(joinpath(@__DIR__, ".."))
include(joinpath(path, "test/case/scope.jl"))
SimpleTypeChecker.Inference.addFile!(ctx, ScopeTest, joinpath(path, "test/case/scope.jl"))
SimpleTypeChecker.Inference.runCheck!(ctx)
end)