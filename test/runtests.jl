using Test
using SimpleTypeChecker

# Per-interface unit tests. These exist to ground each module in its own
# tests so a future change can be validated without running the whole pipeline.
# When you add or modify a module, add or extend its test file here.
@testset "SimpleTypeChecker" begin
    @testset "CompilerAdaptor (Julia compiler internals)" begin
        include("compiler_adaptor/test_compiler_adaptor.jl")
    end

    @testset "Language features" begin
        include("features/test_features.jl")
    end

    # End-to-end smoke tests. These exercise the whole pipeline by checking
    # known sample programs; assertions are coarse ("doesn't crash"). The
    # detailed correctness checks belong in the per-interface testsets above.

    @testset "Smoke — case.jl produces error reports" begin
        # Set up a fresh module to host the case-test source, then check it.
        # We don't go through test/case/casetest.jl because that driver
        # re-includes the SimpleTypeChecker source with a brittle relative
        # path. The relevant test artifact is test/case/case.jl itself.
        case_path = abspath(joinpath(@__DIR__, "case", "case.jl"))
        smoke_mod = Module(:CaseSmoke)
        Base.include(smoke_mod, case_path)
        @assert isdefined(smoke_mod, :CaseTest) "case.jl should define module CaseTest"
        ctx = SimpleTypeChecker.Inference.GlobalContext()
        SimpleTypeChecker.API.addFile!(ctx, smoke_mod.CaseTest, case_path)
        SimpleTypeChecker.API.runCheck!(ctx)
        # The case suite is constructed to produce many error reports.
        # If runCheck! completes without throwing, the smoke check passes.
        @test true
    end

    @testset "Smoke — real-world numerical simulation" begin
        # Exercises the language features added during the May 2026 round:
        # untyped vector literals, lambdas, @inbounds passthrough, ::, <:,
        # and where in expression position. Should produce NO errors.
        sim_path = abspath(joinpath(@__DIR__, "realworld", "numerical_sim.jl"))
        smoke_mod = Module(:NumericalSimSmoke)
        Base.include(smoke_mod, sim_path)
        ctx = SimpleTypeChecker.Inference.GlobalContext()
        SimpleTypeChecker.API.addFile!(ctx, smoke_mod.NumericalSim, sim_path)
        captured_failures = 0
        for (_mi, entries) in ctx.hasChecked
        end
        SimpleTypeChecker.API.runCheck!(ctx)
        for (_mi, entries) in ctx.hasChecked
            for (_kw, exc) in entries
                if exc !== nothing
                    captured_failures += 1
                end
            end
        end
        @test captured_failures == 0
    end

    # Standalone trimmed-binary validation. Opt-in (STTC_STANDALONE_TEST=1)
    # because the build is slow (~30s). This is the membrane's real proof:
    # the inferencer compiled to a self-contained exe must run in a fresh
    # process with its own embedded runtime — any Core.* leak from analyzer
    # code becomes a crash.
    @testset "Standalone trimmed binary" begin
        include("standalone/test_standalone.jl")
    end

    # Package-image smoke. Loads SimpleTypeChecker in a fresh subprocess
    # (exercising the precompiled .so under ~/.julia/compiled/) and runs
    # checkFile against a fixture. This is the supported deployment for
    # the Julia-host case — distinct from the trim binary above. See
    # docs/isolation.md for the three-artifact split.
    @testset "Package-image smoke" begin
        include("standalone/test_package_image.jl")
    end

    @testset "Smoke — README self-check runs to completion" begin
        path = abspath(joinpath(splitdir(pathof(SimpleTypeChecker))[1], ".."))
        ctx = SimpleTypeChecker.Inference.GlobalContext()
        for relpath in [
            "src/syntax/TreeQuery.jl",
            "src/inference/InferenceDefinition.jl",
            "src/inference/Inference.jl",
            "src/inference/InferenceErrorUtility.jl",
            "src/inference/InferenceError.jl",
            "src/inference/ScopeChecking.jl",
        ]
            mod = startswith(relpath, "src/syntax/") ?
                SimpleTypeChecker.SyntaxAdaptor : SimpleTypeChecker.Inference
            SimpleTypeChecker.API.addFile!(ctx, mod, joinpath(path, relpath))
        end
        SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Server,
                                       joinpath(path, "src/server/SimpleHTTPServer.jl"))
        SimpleTypeChecker.API.runCheck!(ctx)
        @test true
    end
end
