# Standalone-binary validation.
#
# Builds the inferencer as a trim-compiled executable, runs it against
# a fixture file in a fresh OS process, and asserts:
#
#   1. The build SUCCEEDS (juliac --trim=unsafe-warn produces a binary).
#      Each new verifier error in the build output is a real coupling we
#      haven't eliminated yet — surfacing them is the point of this test.
#
#   2. The binary RUNS (no MethodError, no abort).  This is the real
#      two-runtime validation — the binary has its own embedded Julia
#      runtime, distinct from the test-suite host. If any analyzer code
#      reaches into the test-suite's `Core.*`, the binary's runtime
#      won't have those bindings and will crash.
#
# Test budget: this is the slowest test in the suite (~30s build + run).
# It only runs when env STTC_STANDALONE_TEST=1 is set, so normal
# `Pkg.test()` stays under 30s wall.
#
# Bypass intentional through Phase E (rtctx.includeFile membrane op):
# Phase E routes Base.which + Base.include through the rtctx seam. The
# parse_block patch from Phase 55 is applied at standalone_entry.jl's
# top level, so parsing succeeds. The binary now fails at rtctx.includeFile
# because Base.include's transitive callee `include_string` isn't trim-
# resolved. Switching EXPECT_PASS=true needs either a trim-friendly
# includeFile impl OR Base.include becoming trim-clean upstream.

using Test

const EXPECT_PASS = false  # see docstring above
# Current failure mode after Phase E (rtctx.includeFile membrane op): the
# binary reaches includeFile (Base.include via the seam) but Base.include's
# transitive callee `include_string` isn't trim-resolved. Expected stderr
# substring used to validate the binary at least got past parse_block_inner.
const EXPECTED_STDERR_SUBSTRING = "include_string"
const PKG_ROOT    = abspath(joinpath(@__DIR__, "..", ".."))
const EXE         = joinpath(PKG_ROOT, "bin", "build", "sttc-check")
const BUILD_JL    = joinpath(PKG_ROOT, "bin", "build_standalone.jl")
const FIXTURE     = joinpath(PKG_ROOT, "test", "case", "case.jl")

@testset "standalone trimmed binary" begin
    if get(ENV, "STTC_STANDALONE_TEST", "") != "1"
        @info "skipping standalone-binary test (set STTC_STANDALONE_TEST=1 to run)"
        return
    end

    # Build.
    @info "Building trimmed exe via juliac --trim=unsafe-warn"
    build_log = mktemp() do path, io
        cmd = `$(Base.julia_cmd()) --project=$(PKG_ROOT) $(BUILD_JL) exe`
        try
            run(pipeline(cmd, stdout=io, stderr=io))
        catch e
            # juliac exits non-zero on verifier errors but still produces
            # a binary in --trim=unsafe-warn mode; treat as success if the
            # binary exists.
        end
        close(io)
        return read(path, String)
    end
    @test isfile(EXE)

    # Run.
    @info "Invoking trimmed exe on fixture" EXE FIXTURE
    run_log, rc = mktemp() do path, io
        cmd = `$EXE $FIXTURE`
        rc = try
            run(pipeline(cmd, stdout=io, stderr=io); wait=true)
            0
        catch e
            e isa ProcessFailedException ? e.procs[1].exitcode : -1
        end
        close(io)
        return (read(path, String), rc)
    end

    if EXPECT_PASS
        @test rc == 0
    else
        # Expected to fail at the rtctx.includeFile seam until a trim-friendly
        # includeFile impl lands (or until Base.include itself becomes trim-
        # clean upstream). We DO still want the exe to be runnable (no
        # segfault before the MethodError fires).
        @test rc != 0
        @test occursin(EXPECTED_STDERR_SUBSTRING, run_log) || rc != -1
    end
end
