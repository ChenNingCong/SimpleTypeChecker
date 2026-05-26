#!/usr/bin/env julia
# Regenerate the diagnostic*.txt files in the repo root.
# Run from the repo root:  julia --project=test bin/gen_diagnostics.jl
#
# What this driver does:
#   1. Loads SimpleTypeChecker into a fresh Main.
#   2. Type-checks ClothSim (non-GPU), the checker's own source, and the
#      two origin-tracing fixture cases.
#   3. Captures STDOUT (where checkPackage/checkFile prints results) into
#      a String and writes each to its own diagnostics_*.txt file.
#   4. STDERR is redirected to /dev/null to suppress Julia's world-age
#      "WARNING: Detected access to binding ..." chatter, which is the
#      loader complaining about Revise-unfriendly Core.eval(...) — not a
#      type-checker finding.

using Pkg
Pkg.activate("typecheck"; shared = true)
push!(LOAD_PATH, joinpath(@__DIR__, ".."))

import SimpleTypeChecker
import SimpleTypeChecker.API: checkPackage, checkFile, GlobalContext, addFile!, runCheck!

const REPO_ROOT = abspath(joinpath(@__DIR__, ".."))

# Capture stdout of `f()` into a String. stderr → /dev/null so Julia's loader
# warnings don't contaminate the file.
#
# Write to a tempfile, not a Pipe: the kernel pipe buffer is ~64 KB on
# Linux, and the consuming `read` doesn't happen until after `f` returns.
# A long checker run (self-check, ClothSim) fills the buffer and writes
# block — looks exactly like the script is hung.
function capture_stdout(f)::String
    tmppath = tempname()
    err_devnull = open("/dev/null", "w")
    out_file = open(tmppath, "w")
    saved_out = stdout
    saved_err = stderr
    redirect_stdout(out_file)
    redirect_stderr(err_devnull)
    try
        f()
    finally
        redirect_stdout(saved_out)
        redirect_stderr(saved_err)
        close(out_file)
        close(err_devnull)
    end
    s = read(tmppath, String)
    rm(tmppath; force = true)
    return s
end

# Strip well-known absolute prefixes so paths display as
# `SimpleTypeChecker/src/...` rather than `/home/.../Code/SimpleTypeChecker/src/...`.
const PATH_STRIP_PREFIXES = String[
    "/home/chenningcong/Documents/Code/",
]

function relativize_paths(s::String)::String
    out = s
    for p in PATH_STRIP_PREFIXES
        out = replace(out, p => "")
    end
    return out
end

function write_diagnostic(path::String, header::String, body::String)
    body = relativize_paths(body)
    open(path, "w") do io
        println(io, header)
        println(io)
        print(io, body)
    end
    println(stderr, "wrote $path ($(count(==('\n'), body)) lines of body)")
end

# 1) ClothSim non-GPU
function gen_clothsim()
    body = capture_stdout() do
        # Pull ClothSim's non-GPU sources only; skip gpu_*.jl + the entry file.
        clothsim_root = "/home/chenningcong/Documents/Code/cloth_simulaton/ClothSim"
        clothsim_src = joinpath(clothsim_root, "src")
        if !isdir(clothsim_src)
            println(stderr, "ClothSim sources not found at $clothsim_src — skipping")
            return
        end
        # LOAD_PATH stacking: put ClothSim's project dir on LOAD_PATH so `using
        # ClothSim` resolves deps via ClothSim's own Manifest, without dumping
        # them into the shared @typecheck env.
        clothsim_root in LOAD_PATH || push!(LOAD_PATH, clothsim_root)
        # Lazily import ClothSim's module to get a Module handle for addFile!.
        try
            @eval Main using ClothSim
        catch e
            println(stderr, "could not load ClothSim — skipping")
            showerror(stderr, e)
            return
        end
        mod = Base.invokelatest(getfield, Main, :ClothSim)
        ctx = GlobalContext()
        for (root, _dirs, files) in walkdir(clothsim_src)
            for f in files
                endswith(f, ".jl") || continue
                startswith(f, "gpu_") && continue
                f == "ClothSim.jl" && continue
                addFile!(ctx, mod, joinpath(root, f))
            end
        end
        ctx.printDispatchTrace = true
        runCheck!(ctx)
    end
    write_diagnostic(joinpath(REPO_ROOT, "diagnostics_clothsim.txt"),
                     "# ClothSim non-GPU type-check", body)
end

# 2) Self-check of the checker's own sources.
#
# Exclusions: the driver/seam files (src/compiler/*, src/patch.jl) reach
# into Core.Compiler.* and JuliaSyntax internals — they're @nocheck-heavy
# by design and a self-check there mostly reports either expected skips
# or things we can't reasonably typecheck (heavy generic Core.Compiler
# types). precompile.jl / precompile_fixture.jl are meta-files for the
# precompile workflow, not part of the checker's logic. Skipping these
# keeps the self-check output focused on the inference / scope / syntax
# code, where errors are actionable.
# Map src/<folder>/<file>.jl → the submodule that includes it. Without
# this mapping, `addFile!(ctx, SimpleTypeChecker, ...)` looks up method
# definitions on the bare top-level module — but the actual functions
# live in `SimpleTypeChecker.Inference`, `.SyntaxAdaptor`, etc. — so
# every `mapAST2Method` reflection call fails with "Module field X not
# defined" and the method gets skipped as a CompilerError.
function _module_for_path(path::String)::Module
    if occursin("/utility/", path)
        return SimpleTypeChecker.Utility
    elseif occursin("/syntax/", path) && endswith(path, "SyntaxDefinition.jl")
        return SimpleTypeChecker.SyntaxDefinition
    elseif occursin("/syntax/", path)
        return SimpleTypeChecker.SyntaxAdaptor
    elseif occursin("/inference/", path)
        return SimpleTypeChecker.Inference
    elseif occursin("/server/", path)
        return SimpleTypeChecker.Server
    elseif occursin("/api/", path)
        return SimpleTypeChecker.API
    else
        return SimpleTypeChecker
    end
end

function gen_self_check()
    body = capture_stdout() do
        src_dir = joinpath(REPO_ROOT, "src")
        ctx = GlobalContext()
        for (root, _dirs, files) in walkdir(src_dir)
            occursin("/experimental", root) && continue
            occursin("/compiler", root) && continue
            for f in files
                endswith(f, ".jl") || continue
                f == "patch.jl" && continue
                f == "precompile.jl" && continue
                f == "precompile_fixture.jl" && continue
                full = joinpath(root, f)
                addFile!(ctx, _module_for_path(full), full)
            end
        end
        ctx.printDispatchTrace = true
        runCheck!(ctx)
    end
    write_diagnostic(joinpath(REPO_ROOT, "diagnostics_self_check.txt"),
                     "# SimpleTypeChecker self-check", body)
end

# 3) Origin-tracing fixtures. Each fixture declares `module X ... end` at
# top-level, so we first `include` it under Main (which defines `Main.X`),
# then point addFile! at Main as the parent module.
function gen_demo()
    cases_dir = joinpath(REPO_ROOT, "test/features/cases/syntax_coverage")
    body = capture_stdout() do
        for fname in ("origin_field.jl", "origin_chain.jl")
            path = joinpath(cases_dir, fname)
            isfile(path) || continue
            println("=== ", fname, " ===")
            Base.include(Main, path)
            ctx = GlobalContext()
            addFile!(ctx, Main, path)
            ctx.printDispatchTrace = true
            runCheck!(ctx)
            println()
        end
    end
    write_diagnostic(joinpath(REPO_ROOT, "diagnostics_demo.txt"),
                     "# Origin-tracing demos", body)
end

gen_demo()
gen_self_check()
gen_clothsim()
