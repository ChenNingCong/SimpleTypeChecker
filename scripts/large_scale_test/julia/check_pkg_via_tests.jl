#!/usr/bin/env julia
#
# Stage 5 (test-seeded) — Pelenitsyn-style entry-point harvesting.
#
#   1. `using PKG` to load it.
#   2. include the package's `test/runtests.jl` in this process so the
#      JIT populates MethodInstances for every test-exercised call.
#   3. Walk `methods(f)` for every function defined in PKG and pull
#      `Base.specializations(m)` — each yields a MethodInstance whose
#      `.specTypes` is a concrete Tuple-type seed.
#   4. Push each MI onto `ctx.queue` as a `KwFunCall(mi)` and run
#      `runCheck!`.
#   5. Print plain key=value lines so the caller can parse without
#      pulling JSON into the test env.
#
# Usage:
#   julia --project=<env> check_pkg_via_tests.jl <PkgName>

const REPO_ROOT = abspath(joinpath(@__DIR__, "..", "..", ".."))
push!(LOAD_PATH, REPO_ROOT)

import SimpleTypeChecker
import SimpleTypeChecker.API: GlobalContext, addFile!, runCheck!
import SimpleTypeChecker.Inference: KwFunCall

length(ARGS) >= 1 || error("usage: check_pkg_via_tests.jl <PkgName>")
const PKG_NAME = ARGS[1]

function find_runtests_path(mod::Module)::Union{String,Nothing}
    p = pathof(mod)
    p === nothing && return nothing
    rt = joinpath(dirname(dirname(p)), "test", "runtests.jl")
    return isfile(rt) ? rt : nothing
end

# Run tests in a sandbox module so they don't poison Main with stray bindings.
function run_tests_safely(runtests::String)
    sandbox = Module(:__PkgTestSandbox__)
    Core.eval(sandbox, :(using Test))
    # Runtime-created modules don't get `include` for free (only modules the
    # parser sees in a `module` block do). Many runtests.jl files do
    # `include("subfile.jl")` — without this they hit `UndefVarError: include`.
    Core.eval(sandbox, quote
        include(path::AbstractString) = Base.include(@__MODULE__, path)
    end)
    try
        Base.invokelatest(Base.include, sandbox, runtests)
        return (:ok, "")
    catch e
        return (:test_failed, first(string(e), 200))
    end
end

# Walk every function defined in `mod` and harvest its specialised
# MethodInstances. Filters: method must be defined IN `mod` (not in a
# stdlib we re-exported), and the spec types must be a concrete-ish Tuple.
function harvest_mis(mod::Module)
    out = Core.MethodInstance[]
    for name in names(mod; all=true, imported=false)
        isdefined(mod, name) || continue
        v = try
            getfield(mod, name)
        catch
            continue
        end
        # Functions, callable struct types, and constructor singletons all
        # have method tables we can iterate.
        ms = try
            methods(v)
        catch
            continue
        end
        for m in ms
            m.module === mod || continue
            for spec in Base.specializations(m)
                spec === nothing && continue
                push!(out, spec)
            end
        end
    end
    return out
end

function main()
    started = time()
    println(stderr, "[loading $PKG_NAME]")
    Core.eval(Main, Meta.parse("using $PKG_NAME"))
    mod = Base.invokelatest(getfield, Main, Symbol(PKG_NAME))

    rt = find_runtests_path(mod)
    if rt === nothing
        return Dict("status"=>"no_runtests_file")
    end
    println(stderr, "[running tests from $rt]")
    test_started = time()
    (test_status, test_err) = run_tests_safely(rt)
    test_elapsed = time() - test_started
    println(stderr, "[tests $test_status in $(round(test_elapsed; digits=2))s]")

    mis = harvest_mis(mod)
    println(stderr, "[harvested $(length(mis)) MIs]")

    # runCheck! drops queue items whose `method` isn't in `ctx.methodDefs`,
    # which is populated by addFile!. So even though we seed the queue
    # with concrete MIs from tests, the source ASTs still need to be
    # registered first.
    ctx = GlobalContext()
    pkg_src = dirname(pathof(mod))
    src_files_added = 0
    for (root, _dirs, fs) in walkdir(pkg_src)
        for f in fs
            endswith(f, ".jl") || continue
            try
                Base.invokelatest(addFile!, ctx, mod, joinpath(root, f))
                src_files_added += 1
            catch
                # Parser blew up on this file; the others may still work.
            end
        end
    end
    println(stderr, "[addFile! added $src_files_added files]")

    seeds_pushed = 0
    for mi in mis
        try
            push!(ctx.queue, KwFunCall(mi))
            seeds_pushed += 1
        catch
        end
    end
    println(stderr, "[seeded $seeds_pushed queue items]")

    check_started = time()
    check_failed = ""
    try
        open("/dev/null", "w") do dn
            redirect_stdout(dn) do
                runCheck!(ctx)
            end
        end
    catch e
        check_failed = first(string(e), 300)
    end
    check_elapsed = time() - check_started

    n_errors = 0
    err_classes = Dict{String,Int}()
    first_err = ""
    for (_mi, entries) in ctx.hasChecked
        for (_kw, exc) in entries
            exc === nothing && continue
            n_errors += 1
            cls = string(typeof(exc).name.name)
            err_classes[cls] = get(err_classes, cls, 0) + 1
            if isempty(first_err)
                msg = hasfield(typeof(exc), :msg) ? getfield(exc, :msg) : string(exc)
                first_err = first(msg, 180)
            end
        end
    end

    return Dict(
        "status"          => isempty(check_failed) ? "ok" : "check_threw",
        "check_threw"     => check_failed,
        "test_status"     => String(test_status),
        "test_err"        => test_err,
        "test_elapsed"    => round(test_elapsed; digits=3),
        "mis_harvested"   => length(mis),
        "n_method_checks" => length(ctx.hasChecked),
        "n_errors"        => n_errors,
        "err_classes"     => err_classes,
        "first_error"     => first_err,
        "check_elapsed"   => round(check_elapsed; digits=3),
        "total_elapsed"   => round(time() - started; digits=3),
    )
end

result = main()
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
