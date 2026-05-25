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

# One-shot type-check of a loaded module's source directory.
#
# Usage:
#     using YourPackage, SimpleTypeChecker
#     SimpleTypeChecker.API.checkPackage(YourPackage)
#
# What it does:
#   - Resolves the module's source directory from `pathof(mod)`.
#   - Adds every `.jl` file in that directory (and its subdirectories) to a
#     fresh GlobalContext. The entry-point file (whose basename matches the
#     module name, e.g. `ClothSim.jl` for module `ClothSim`) is SKIPPED —
#     it declares `module Foo ... end` which trips the wrapper-name
#     collision in evalMods. Including files (which don't declare their
#     own module) work cleanly.
#   - Runs runCheck! and returns the GlobalContext for further inspection.
@nocheck function checkPackage(mod::Module)::GlobalContext
    p = pathof(mod)
    if p === nothing
        error("pathof($mod) returned nothing — module isn't loaded from a package source tree")
    end
    pkgdir = dirname(abspath(p))
    entry_basename = string(nameof(mod), ".jl")
    ctx = GlobalContext()
    for (root, _dirs, files) in walkdir(pkgdir)
        for f in files
            endswith(f, ".jl") || continue
            f == entry_basename && continue
            addFile!(ctx, mod, joinpath(root, f))
        end
    end
    runCheck!(ctx)
    return ctx
end

# Single-file variant. Pass the live module the file's contents will be
# attributed to (usually the parent module, NOT a module the file declares
# itself — see [[project-install-friction]] for why a wrapper-name match
# silently drops methods).
function checkFile(mod::Module, path::String)::GlobalContext
    ctx = GlobalContext()
    addFile!(ctx, mod, abspath(path))
    runCheck!(ctx)
    return ctx
end

macro code_checked(ctx, ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :(checkFactory($ctx)), ex0)
end

export addFile!, runCheck!, GlobalContext, @nocheck, check, loadPackage, checkPackage, checkFile