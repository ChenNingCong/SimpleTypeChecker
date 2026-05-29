#!/usr/bin/env julia
#
# Stage 5: per-package smoke check.
#
#   1. Set up a fresh env: Pkg.add(PKG) + install test deps from either
#      `<pkg>/test/Project.toml` (preferred, newer convention) OR the
#      legacy `[extras]` + `[targets].test` block in `<pkg>/Project.toml`.
#   2. `using PKG` to load the module.
#   3. include `test/runtests.jl` in a sandbox module — populates Julia's
#      JIT cache with MethodInstances for everything the tests actually
#      exercised.
#   4. Harvest MIs whose declaring method lives in PKG's module (drop
#      stdlib + dep noise).
#   5. addFile! every `src/**/*.jl` so the checker's `methodDefs` has
#      ASTs to resolve.
#   6. Push each harvested MI onto the queue, runCheck!.
#   7. Emit a structured one-line summary: package name, file/MI counts,
#      error class breakdown, and (optionally) the rendered diagnostics
#      themselves when --diag is on.
#
# Usage:
#   julia --project=<tmp_env> stage5_check.jl <PkgName> [--diag] [--permit-required-kwargs]
#
# This script is designed to be the stable entry point for stage 5 — it
# handles all the env / world-age / sandbox failure modes that broke the
# earlier `Base.include`-only approach.

using Pkg

const REPO_ROOT = abspath(joinpath(@__DIR__, "..", "..", ".."))
push!(LOAD_PATH, REPO_ROOT)

length(ARGS) >= 1 || error("usage: stage5_check.jl <PkgName> [--diag] [--permit-required-kwargs]")
const PKG_NAME = ARGS[1]
const SHOW_DIAG = "--diag" in ARGS
const PERMIT_REQUIRED_KWARGS = "--permit-required-kwargs" in ARGS

# ── Test-env setup ──────────────────────────────────────────────────────

# Reads test deps from one of:
#   (a) `<pkg_root>/test/Project.toml`  — modern convention (Julia 1.2+)
#   (b) `[extras]` + `[targets].test` in `<pkg_root>/Project.toml` — legacy
# Returns a Vector of PackageSpec the caller can hand to Pkg.add.
function discover_test_deps(pkg_root::AbstractString)
    new_layout = joinpath(pkg_root, "test", "Project.toml")
    if isfile(new_layout)
        tp = Pkg.TOML.parsefile(new_layout)
        deps = get(tp, "deps", Dict{String,Any}())
        return [Pkg.PackageSpec(name=String(k), uuid=Base.UUID(String(v))) for (k, v) in deps]
    end
    proj = joinpath(pkg_root, "Project.toml")
    if isfile(proj)
        tp = Pkg.TOML.parsefile(proj)
        extras = get(tp, "extras", Dict{String,Any}())
        targets = get(tp, "targets", Dict{String,Any}())
        testnames = String[]
        if haskey(targets, "test")
            append!(testnames, targets["test"])
        end
        out = Pkg.PackageSpec[]
        for n in testnames
            haskey(extras, n) || continue
            push!(out, Pkg.PackageSpec(name=n, uuid=Base.UUID(extras[n])))
        end
        return out
    end
    return Pkg.PackageSpec[]
end

# Returns (mod, test_outcome::Symbol, test_err::String, test_elapsed::Float64).
# test_outcome ∈ {:ok, :failed, :no_runtests}.
function install_and_run_tests(pkgname::String)
    Pkg.activate(mktempdir())
    println(stderr, "[adding $pkgname]")
    Pkg.add(pkgname)
    Core.eval(Main, Meta.parse("using $pkgname"))
    mod = Base.invokelatest(getfield, Main, Symbol(pkgname))

    pkg_root = dirname(dirname(pathof(mod)))
    test_deps = discover_test_deps(pkg_root)
    if !isempty(test_deps)
        println(stderr, "[adding $(length(test_deps)) test deps]")
        Pkg.add(test_deps)
    end

    rt = joinpath(pkg_root, "test", "runtests.jl")
    isfile(rt) || return (mod, :no_runtests, "", 0.0)

    sandbox = Module(Symbol("__$(pkgname)Tests__"))
    Core.eval(sandbox, :(using Test))
    # Pre-import the package into the sandbox so tests that do `using PKG`
    # find it in world. (Many test files do this explicitly anyway, but
    # belt-and-braces helps with order-of-operations across worlds.)
    Core.eval(sandbox, Meta.parse("using $pkgname"))
    # Inject an `include` that resolves relative paths against the file
    # currently being read. Modules created via `Module(:name)` don't
    # inherit `include` (that's only auto-defined in top-level Main /
    # `module` blocks the parser sees), so tests that do
    # `include("subfile.jl")` would otherwise fire `UndefVarError`.
    Core.eval(sandbox, quote
        include(path::AbstractString) = Base.include(@__MODULE__, path)
    end)

    t0 = time()
    outcome = :ok
    err = ""
    try
        Base.invokelatest(Base.include, sandbox, rt)
    catch e
        outcome = :failed
        err = first(string(e), 300)
    end
    return (mod, outcome, err, time() - t0)
end

# ── MI harvesting ──────────────────────────────────────────────────────

# Walks every binding exported or internal in `mod`, pulls each binding's
# method table (functions, callable structs, constructors), and for each
# method declared IN `mod`, pushes every cached specialization. Filtering
# at the `method.module === mod` level keeps stdlib / dep noise out.
function harvest_mis(mod::Module)::Vector{Core.MethodInstance}
    out = Core.MethodInstance[]
    for name in names(mod; all=true, imported=false)
        isdefined(mod, name) || continue
        v = try
            getfield(mod, name)
        catch
            continue
        end
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

# ── Checker invocation ────────────────────────────────────────────────

function run_checker(mod::Module, mis::Vector{Core.MethodInstance};
                    permit_required_kwargs::Bool=false,
                    show_diag::Bool=false)
    # Loaded lazily so caller doesn't pay the precompile bill on errors
    # in the test-env setup phase.
    import_stc()
    ctx = Base.invokelatest(SimpleTypeChecker.API.GlobalContext)
    ctx.permitRequiredKwargs = permit_required_kwargs

    src_dir = dirname(pathof(mod))
    n_files_added = 0
    for (root, _dirs, fs) in walkdir(src_dir), f in fs
        endswith(f, ".jl") || continue
        try
            Base.invokelatest(SimpleTypeChecker.API.addFile!, ctx, mod, joinpath(root, f))
            n_files_added += 1
        catch
            # parser blew up on this file; the others may still work
        end
    end

    for mi in mis
        try
            push!(ctx.queue, SimpleTypeChecker.Inference.KwFunCall(mi))
        catch
        end
    end

    diag_buf = IOBuffer()
    check_outcome = :ok
    check_err = ""
    t0 = time()
    try
        if show_diag
            redirect_stdout(diag_buf) do
                Base.invokelatest(SimpleTypeChecker.API.runCheck!, ctx)
            end
        else
            open("/dev/null", "w") do dn
                redirect_stdout(dn) do
                    Base.invokelatest(SimpleTypeChecker.API.runCheck!, ctx)
                end
            end
        end
    catch e
        check_outcome = :threw
        check_err = first(string(e), 300)
    end
    check_elapsed = time() - t0

    n_errors = 0
    err_classes = Dict{String,Int}()
    first_err_msg = ""
    for (_mi, entries) in ctx.hasChecked
        for (_kw, exc) in entries
            exc === nothing && continue
            n_errors += 1
            cls = string(typeof(exc).name.name)
            err_classes[cls] = get(err_classes, cls, 0) + 1
            if isempty(first_err_msg)
                msg = hasfield(typeof(exc), :msg) ? getfield(exc, :msg) : string(exc)
                first_err_msg = first(msg, 180)
            end
        end
    end
    return (n_files_added=n_files_added,
            n_methods_checked=length(ctx.hasChecked),
            n_errors=n_errors,
            err_classes=err_classes,
            first_err_msg=first_err_msg,
            check_outcome=check_outcome,
            check_err=check_err,
            check_elapsed=check_elapsed,
            rendered_diag=show_diag ? String(take!(diag_buf)) : "")
end

function import_stc()
    @eval Main begin
        if !isdefined(Main, :SimpleTypeChecker)
            import SimpleTypeChecker
        end
    end
end

# ── Main ───────────────────────────────────────────────────────────────

function main()
    total_t0 = time()
    (mod, test_outcome, test_err, test_elapsed) =
        install_and_run_tests(PKG_NAME)

    mis = harvest_mis(mod)
    println(stderr, "[harvested $(length(mis)) MIs from $PKG_NAME]")

    r = run_checker(mod, mis;
                    permit_required_kwargs=PERMIT_REQUIRED_KWARGS,
                    show_diag=SHOW_DIAG)
    total_elapsed = time() - total_t0

    # Print a stable single-block result. Key=value lines (no JSON dep).
    println("RESULT_BEGIN")
    println("package=", PKG_NAME)
    println("test_outcome=", test_outcome)
    println("test_elapsed=", round(test_elapsed; digits=2))
    if test_outcome == :failed
        println("test_err=", test_err)
    end
    println("mis_harvested=", length(mis))
    println("files_added=", r.n_files_added)
    println("methods_checked=", r.n_methods_checked)
    println("n_errors=", r.n_errors)
    println("check_outcome=", r.check_outcome)
    println("check_elapsed=", round(r.check_elapsed; digits=2))
    println("total_elapsed=", round(total_elapsed; digits=2))
    if !isempty(r.err_classes)
        for (cls, n) in sort(collect(r.err_classes); by = x -> -x[2])
            println("err.", cls, "=", n)
        end
    end
    if r.check_outcome == :threw
        println("check_threw_err=", r.check_err)
    end
    if !isempty(r.first_err_msg)
        println("first_err=", r.first_err_msg)
    end
    if SHOW_DIAG && !isempty(r.rendered_diag)
        println("RENDERED_DIAG_BEGIN")
        print(r.rendered_diag)
        endswith(r.rendered_diag, "\n") || println()
        println("RENDERED_DIAG_END")
    end
    println("RESULT_END")
    return 0
end

if abspath(PROGRAM_FILE) == @__FILE__
    exit(main())
end
