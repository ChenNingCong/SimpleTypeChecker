# Package-image smoke test.
#
# Validates the "host Julia loads the checker through normal `using`"
# deployment path — which uses Julia's package-image cache rather than
# a trim-compiled artifact. This is the supported deployment for the
# Julia-host case; trim --output-lib is forbidden here (sysimage
# identity conflict — see docs/isolation.md).
#
# Why a subprocess: this test is meaningful only when SimpleTypeChecker
# is loaded *from cold*, i.e. from a fresh process that hits the
# precompiled .so under ~/.julia/compiled/. The current test process
# already has the package loaded from source via `using SimpleTypeChecker`
# at the top of runtests.jl — exercising it again here would only test
# the in-memory image. The subprocess gives us the deployment scenario.
#
# What it catches:
#   - Precompile pass broke (a top-level expression no longer
#     trim/precompile-clean).
#   - __init__ wiring broke (something the loaded image needs at startup
#     no longer runs in the right order).
#   - The API surface used by external callers regressed.

using Test

const PKG_ROOT = abspath(joinpath(@__DIR__, "..", ".."))
const FIXTURE  = joinpath(PKG_ROOT, "test", "realworld", "numerical_sim.jl")

@testset "package image smoke" begin
    # Script the subprocess runs. Loads the package (triggering precompile
    # cache load) and exercises the checkFile API against a fixture we
    # already validate elsewhere (`runtests.jl` smoke testset). The
    # subprocess prints "OK" on success so we can verify it ran to
    # completion rather than silently exiting.
    script = """
    using SimpleTypeChecker
    smoke_mod = Module(:NumericalSimSmoke)
    Base.include(smoke_mod, raw"$(FIXTURE)")
    ctx = SimpleTypeChecker.Inference.GlobalContext()
    SimpleTypeChecker.API.addFile!(ctx, smoke_mod.NumericalSim, raw"$(FIXTURE)")
    SimpleTypeChecker.API.runCheck!(ctx)
    println("OK")
    """

    cmd = `$(Base.julia_cmd()) --startup-file=no --project=$(PKG_ROOT) -e $script`
    out, rc = mktemp() do path, io
        rc = try
            run(pipeline(cmd, stdout=io, stderr=io); wait=true)
            0
        catch e
            e isa ProcessFailedException ? e.procs[1].exitcode : -1
        end
        close(io)
        return (read(path, String), rc)
    end

    @test rc == 0
    @test occursin("OK", out)
end
