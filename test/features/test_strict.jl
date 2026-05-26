# Strict-mode overlay tests — proof that the membrane absorbs policy
# overlays without analyzer changes.
#
# Verifies:
#   1. Default rtctx silently approves push!(Float64[], 1) — the baseline gap.
#   2. Strict rtctx flags the same call with an InferenceErrorStrictReject.

using Test
using SimpleTypeChecker
using SimpleTypeChecker.Inference: GlobalContext, defaultRuntimeContext, strictRuntimeContext

const STRICT_FIXTURE = joinpath(@__DIR__, "cases", "strict", "push_eltype_mismatch.jl")

function check_with(rtctx)
    wrapper = Module(:StrictTestWrap)
    Base.invokelatest(Base.include, wrapper, STRICT_FIXTURE)
    target = Base.invokelatest(getfield, wrapper, :PushEltypeMismatch)
    ctx = GlobalContext(; rtctx)
    Base.invokelatest(SimpleTypeChecker.API.addFile!, ctx, target, STRICT_FIXTURE)
    open("/dev/null", "w") do devnull
        redirect_stdout(devnull) do
            Base.invokelatest(SimpleTypeChecker.API.runCheck!, ctx)
        end
    end
    n_errors = 0
    sawStrictReject = false
    for (_mi, entries) in ctx.hasChecked
        for (_kw, exc) in entries
            if exc !== nothing
                n_errors += 1
                if occursin("StrictReject", string(typeof(exc)))
                    sawStrictReject = true
                end
            end
        end
    end
    return (n_errors, sawStrictReject)
end

@testset "strict-mode overlay" begin
    # Baseline: default rtctx approves the convert-required push! silently.
    n_default, sawStrict_default = check_with(defaultRuntimeContext())
    @test n_default == 0
    @test !sawStrict_default

    # Strict overlay: the same call gets flagged via approveCallSite.
    n_strict, sawStrict_strict = check_with(strictRuntimeContext())
    @test n_strict >= 1
    @test sawStrict_strict
end
