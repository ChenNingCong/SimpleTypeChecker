# Shared helpers for per-feature test files.
#
# Each test case lives in its own readable .jl fixture file under
# `cases/<feature>/`. The helpers below load a fixture, run the checker on
# its declared module, and inspect what landed in `ctx.hasChecked`. The
# checker stores either an exception object or `nothing` per MethodInstance —
# more reliable than scraping stdout.

using Test
using SimpleTypeChecker
using SimpleTypeChecker.Inference: GlobalContext

const FEATURES_DIR = @__DIR__

# Run runCheck! on the fixture at `cases/<rel>` declaring module `module_name`.
# Returns a Vector{Pair{String, Any}}: MethodInstance → exception-or-nothing.
#
# IMPORTANT: the wrapper Module name MUST NOT match the declared inner module
# name. If they match, `evalMods(inner, [inner_name])` in runCheck! tries to
# dereference `inner.inner_name`, which doesn't exist, and the checker
# silently skips the method as a CompilerError.
function check_case(rel::String, module_name::Symbol; deep::Bool=false)
    path = joinpath(FEATURES_DIR, "cases", rel)
    wrapper = Module(:FeatureTestWrapper)
    # Julia 1.12 enforces strict world-age semantics for new bindings inside
    # function scope: a `Base.include` here creates bindings in a future
    # world that this function (running in the current world) can't see
    # without `invokelatest`. Without it, the checker queries methods on
    # the fixture module at our world and sees them as undefined / abstract,
    # producing spurious InferenceErrorFunCallArgs. Wrap everything that
    # touches the freshly-included module in `invokelatest`.
    Base.invokelatest(Base.include, wrapper, path)
    target_mod = Base.invokelatest(getfield, wrapper, module_name)
    ctx = GlobalContext()
    # `deep=true` opts into the recursive dyn-dispatch detection. Tests
    # for that detection set it; everything else uses the shallow default.
    ctx.deepDispatchCheck = deep
    Base.invokelatest(SimpleTypeChecker.API.addFile!, ctx, target_mod, path)
    open("/dev/null", "w") do devnull
        redirect_stdout(devnull) do
            Base.invokelatest(SimpleTypeChecker.API.runCheck!, ctx)
        end
    end
    results = Pair{String, Any}[]
    for (mi, entries) in ctx.hasChecked
        for (_kwargs, exc) in entries
            push!(results, string(mi) => exc)
        end
    end
    return results
end

# True iff any checked method instance produced an exception whose type name
# contains `pattern`, OR whose `.msg` field (if any) contains `pattern`.
function has_error_matching(results, pattern::String)
    for (_mi, exc) in results
        exc === nothing && continue
        if occursin(pattern, string(typeof(exc)))
            return true
        end
        if hasfield(typeof(exc), :msg) && occursin(pattern, getfield(exc, :msg))
            return true
        end
    end
    return false
end

function any_error(results)
    return any(((_mi, exc),) -> exc !== nothing, results)
end

# Convenience asserters — one line per case in the test files. The fixture's
# inline comment documents the expected outcome; the assertion enforces it.
# Pass `deep=true` for tests that depend on the recursive dyn-dispatch
# detection (off by default in fresh GlobalContexts).
function expect_clean(rel::String, module_name::Symbol; deep::Bool=false)
    @test !any_error(check_case(rel, module_name; deep))
end

function expect_error(rel::String, module_name::Symbol, pattern::String; deep::Bool=false)
    @test has_error_matching(check_case(rel, module_name; deep), pattern)
end
