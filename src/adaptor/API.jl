import ..SimpleTypeChecker
import ..SimpleTypeChecker.Utility.@nocheck
import ..SimpleTypeChecker.Inference.GlobalContext
export addFile!, runCheck!, GlobalContext

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
        end
        push!(result, v)
    end
    return which(f, tuple(result...))
end


function addFile!(ctx::SimpleTypeChecker.Inference.GlobalContext, mod::Core.Module, filename::String)
    if !isabspath(filename)
        error("$filename is not a absolute path")
    end
    str = open(filename) do f
        read(f, String)
    end
    conv = SimpleTypeChecker.Inference.parseJuExpr(str, filename)
    for (ex, mapping) in sort(collect(conv.map), by = x->x.first.ast.loc.span.first)
        mod = evalMods(mod, conv.modMap[ex])
        ast = ex.ast
        def = ex.val
        if isdefined(mod, def.f[])
            ff = Core.eval(mod, def.f[])
        else
            error("$(def.f[]) is not defined in module $(mod)")
        end
        meth = collectArgs(mod, ff, def.args)
        if !(meth isa Method)
            println("Not a valid method $meth")
            continue
        end
        # meth is the corresponding method definition for ex 
        z = meth.sig
        ctx.methodDefs[meth] = ex=>mapping
        if z isa UnionAll
            continue
        end
        if !(z <: Tuple)
            continue
        end
        # the logic here is not quite correct
        # we only check concrete function here
        iscon = true
        for i in 2:length(z.parameters)
            if !Base.isconcretetype(z.parameters[i])
                iscon = false
                break
            end
        end
        if iscon
            ci = Base.code_typed_by_type(z)[1].first
            if ci isa Core.CodeInfo
                mi = ci.parent
                push!(ctx.queue, mi)
            end
        end
    end
end

function runCheck!(ctx::SimpleTypeChecker.Inference.GlobalContext)
    while !isempty(ctx.queue)
        mi = popfirst!(ctx.queue)
        # this specialization is checked
        if haskey(ctx.hasChecked, mi)
            continue
        end
        meth = mi.def
        if !haskey(ctx.methodDefs, meth)
            continue
        end
        ex, mapping = ctx.methodDefs[meth]
        println("Checking $mi")
        tts = mi.specTypes
        # mark the specialization in check
        ctx.hasChecked[mi] = nothing
        try
            SimpleTypeChecker.Inference.testInferForFunction(ctx, ex, mapping, mi)
        catch e
            if e isa SimpleTypeChecker.Inference.InferenceError
                println("────────────────────────────────────────────────────────────────")
                ctx.hasChecked[mi] = e
            else
                rethrow(e)
            end
        end
    end
end
#=
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
=#