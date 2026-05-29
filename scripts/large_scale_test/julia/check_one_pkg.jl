#!/usr/bin/env julia
#
# Stage 5 (smoke) — runs SimpleTypeChecker.runCheck! on ONE installed
# package. Usage:
#   julia check_one_pkg.jl <PkgName>
#
# Expects the package to be in the active env already (Pkg.add'd by
# caller). Writes a brief JSON record to stdout-prefixed line "RESULT:".

const REPO_ROOT = abspath(joinpath(@__DIR__, "..", "..", ".."))
push!(LOAD_PATH, REPO_ROOT)

import SimpleTypeChecker
import SimpleTypeChecker.API: GlobalContext, addFile!, runCheck!

length(ARGS) >= 1 || error("usage: check_one_pkg.jl <PkgName>")
const PKG_NAME = ARGS[1]

function find_src_files(mod::Module)::Vector{String}
    # Use the module's own __module__/__file__ to find its src dir.
    p = pathof(mod)
    p === nothing && return String[]
    src_dir = dirname(p)
    files = String[]
    for (root, _dirs, fs) in walkdir(src_dir)
        for f in fs
            endswith(f, ".jl") || continue
            push!(files, joinpath(root, f))
        end
    end
    return files
end

function main()
    started = time()
    # Load the package — this happens in the caller's env.
    mod = try
        Base.invokelatest(Base.require, Base.PkgId(Base.UUID("00000000-0000-0000-0000-000000000000"), PKG_NAME))
    catch
        try
            Core.eval(Main, Meta.parse("using $PKG_NAME"))
            Base.invokelatest(getfield, Main, Symbol(PKG_NAME))
        catch e
            println(stderr, "[load_failed] ", first(string(e), 200))
            return Dict("status"=>"load_failed","err"=>first(string(e),200))
        end
    end
    src_files = find_src_files(mod)
    isempty(src_files) && return Dict("status"=>"no_src_files")

    ctx = GlobalContext()
    n_added = 0
    for f in src_files
        try
            Base.invokelatest(addFile!, ctx, mod, f)
            n_added += 1
        catch e
            # Parser blew up on a specific file — keep going.
            continue
        end
    end

    n_check_errors = 0
    err_class_counts = Dict{String,Int}()
    first_err = ""
    try
        open("/dev/null", "w") do dn
            redirect_stdout(dn) do
                Base.invokelatest(runCheck!, ctx)
            end
        end
    catch e
        return Dict("status"=>"check_threw","err"=>first(string(e),300),
                    "files_added"=>n_added,
                    "elapsed"=>time()-started)
    end
    for (_mi, entries) in ctx.hasChecked
        for (_kw, exc) in entries
            exc === nothing && continue
            n_check_errors += 1
            cls = string(typeof(exc).name.name)
            err_class_counts[cls] = get(err_class_counts, cls, 0) + 1
            if isempty(first_err)
                msg = hasfield(typeof(exc), :msg) ? getfield(exc, :msg) : string(exc)
                first_err = first(msg, 180)
            end
        end
    end
    return Dict(
        "status"          => "ok",
        "files_added"     => n_added,
        "n_method_checks" => length(ctx.hasChecked),
        "n_errors"        => n_check_errors,
        "err_classes"     => err_class_counts,
        "first_error"     => first_err,
        "elapsed"         => time() - started,
    )
end

result = main()
# Plain k=v lines instead of JSON — the test env doesn't carry JSON.jl and
# adding it would force a re-resolve of every package's compat.
println("RESULT_BEGIN")
println("package=", PKG_NAME)
for k in sort(collect(keys(result)))
    v = result[k]
    if v isa AbstractDict
        println(k, "=", join((string(k2, ":", v2) for (k2, v2) in v), ","))
    else
        println(k, "=", v)
    end
end
println("RESULT_END")
