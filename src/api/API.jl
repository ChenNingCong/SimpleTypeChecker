import ..SimpleTypeChecker
import ..SimpleTypeChecker.Utility.@nocheck
import ..SimpleTypeChecker.Inference.GlobalContext
import ..SimpleTypeChecker.Inference.addFile!
import ..SimpleTypeChecker.Inference.runCheck!
import ..SimpleTypeChecker.Inference.KwFunCall

function check(ctx::SimpleTypeChecker.Inference.GlobalContext, f, tt)
    ci = Base.code_typed(f,tt)[1].first
    if ci isa Core.CodeInfo
        mi = ci.parent
        push!(ctx.queue, KwFunCall(mi))
        runCheck!(ctx)
    end
    return
end



function loadPackage(s)::GlobalContext
    name = Symbol(String(s))
    mod = Main
    for i in 1:2
        try
            Core.eval(Main,:(using $name))
            mod = Core.eval(Main, name)
        catch e
            println(e)
        end
    end
    if mod == Main
        error("Failed to include module")
    end
    path = dirname(abspath(pathof(mod)))
    files = String[]
    for (root, dir, files_) in collect(walkdir(path))
        for f in files_
            apath = joinpath(path, root, f)
            println("Scanning file from package $mod", apath)
            push!(files, apath)
        end
    end
    ctx = GlobalContext()
    for i in files
        full = joinpath(path, i)
        println(mod, " ", full)
        addFile!(ctx, mod, full)
    end
    return ctx
end

function checkFactory(ctx::GlobalContext)
    function helper(x...)
        check(ctx, x...)
    end
    return helper
end

macro code_checked(ctx, ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :(checkFactory($ctx)), ex0)
end

export addFile!, runCheck!, GlobalContext, @nocheck, check, loadPackage