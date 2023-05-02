module MyTest
include("../src/interface.jl")
function evalMods(parent::Core.Module, s::Vector{Symbol})
    base = parent
    for i in s
        base = getproperty(base, i)
    end
    return base
end

function collectArgs(mod, f, tts)
    result = Any[]
    for (name, tt) in tts
        v = Interface.fasteval(mod, tt)
        push!(result, v)
    end
    try
        return which(f, tuple(result...))
    catch e
        return e
    end
end

function runtest(s::String, parent::Core.Module = Main;filename)
    local l
    try
        l = Interface.parseJuAST(s;valid = false, filename=filename)
    catch e
        println(e[2])
        dump(e[3])
        error()
    end
    ast = Interface.convert2Expr(l)
    for (mod, def) in Interface.extractFunDef(ast, @__MODULE__)
        for i in def
            if parent != Main
                mmmmm = parent
            else
                mmmmm = evalMods(Main, mod)
            end
            ff = Core.eval(mmmmm, i.f)
            m = collectArgs(mmmmm, ff, i.args)
            if !(m isa Method)
                println(m)
                continue
            end
            z = m.sig
            if !(z <: Tuple)
                println(z)
                error("Not here")
            end
            try
                if z.parameters[1] <: Type
                    k = z.parameters[1].parameters[1]
                else
                    k = z.parameters[1].instance
                end
                println(k)
                rep = Interface.testInferForFunction(i, k, tuple(z.parameters[2:end]...))
                #Interface.displayReport(stdout, rep)
            catch e
                if e isa Interface.InferenceError
                    println("────────────────────────────────────────────────────────────────")
                else
                    rethrow(e)
                end
            end
        end
    end
end

include("../src/adaptor/toplevel.jl")
#=
s2 = open(joinpath(@__DIR__, "../src/adaptor/JuExprAdaptor.jl")) do f
    read(f, String)
end
runtest(s2, SimpleTypeChecker.Inference;filename="src/adaptor/JuExprAdaptor.jl")
s2 = open(joinpath(@__DIR__, "../src/adaptor/SyntaxAdaptor.jl")) do f
    read(f, String)
end
runtest(s2, SimpleTypeChecker.SyntaxAdaptor;filename="src/adaptor/SyntaxAdaptor.jl")

s2 = open(joinpath(@__DIR__, "../src/adaptor/SyntaxAdaptor.jl")) do f
    read(f, String)
end
ex = SimpleTypeChecker.Inference.parseJuExpr(s2, "src/adaptor/SyntaxAdaptor.jl")
SimpleTypeChecker.Inference.lookupToplevel(ex)
=#
s3 = open(joinpath(@__DIR__, "../src/adaptor/InferenceError.jl")) do f
    read(f, String)
end
runtest(s3, SimpleTypeChecker.Inference;filename="src/adaptor/InferenceError.jl")

#=
s5 = open(joinpath(@__DIR__, "../src/adaptor/InferenceDefinition.jl")) do f
    read(f, String)
end
runtest(s5, SimpleTypeChecker.Inference;filename="src/adaptor/InferenceDefinition.jl")
=#

s4 = open(joinpath(@__DIR__, "../src/adaptor/Inference.jl")) do f
    read(f, String)
end
runtest(s4, SimpleTypeChecker.Inference;filename="src/adaptor/Inference.jl")

end