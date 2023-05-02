import ..SimpleTypeChecker
import ..SimpleTypeChecker.Utility.@nocheck
const M = SimpleTypeChecker.Inference
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
        if SimpleTypeChecker.Utility.isNone(tt)
            v = Any
        else
            v = SimpleTypeChecker.Inference.fasteval(mod, SimpleTypeChecker.Utility.castJust(tt))
            push!(result, v)
        end
    end
    try
        return which(f, tuple(result...))
    catch e
        return e
    end
end

function runtest(mod::Core.Module, filename::String)
    if !isabspath(filename)
        error("$filename is not a absolute path")
    end
    str = open(filename) do f
        read(f, String)
    end
    globalctx = SimpleTypeChecker.Inference.GlobalContext()
    conv = M.parseJuExpr(str, filename)
    for (ex, mapping) in sort(collect(conv.map), by = x->x.first.ast.loc.span.first)
        mod = evalMods(mod, conv.modMap[ex])
        ast = ex.ast
        def = ex.val
        #println(mod, def.f[])
        if isdefined(mod, def.f[])
            ff = Core.eval(mod, def.f[])
        else
            error("$(def.f[]) is not defined in module $(mod)")
        end
        println("Checking ", def.f[], ":")
        m = collectArgs(mod, ff, def.args)
        if !(m isa Method)
            println("Not a valid method $m")
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
            rep = SimpleTypeChecker.Inference.testInferForFunction(globalctx, ex, mapping, k, tuple(z.parameters[2:end]...))
        catch e
            if e isa SimpleTypeChecker.Inference.InferenceError
                println("────────────────────────────────────────────────────────────────")
            else
                rethrow(e)
            end
        end
    end
end