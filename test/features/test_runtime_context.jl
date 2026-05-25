# Phase A backbone test for the runtime-context isolation seam.
#
# Verifies that a custom `RuntimeContext` passed to `GlobalContext` is
# (a) actually held on the context and (b) consulted by inference. The
# escape-hatch detection path is the first call site routed through
# `rtctx.methodsOf`; we replace `methodsOf` with a recording wrapper
# and confirm the recorder fires.

using Test
using SimpleTypeChecker
using SimpleTypeChecker.Inference: GlobalContext, RuntimeContext, MethodsOfFn,
                                   MethodDef, defaultRuntimeContext, methodsOf

include("helpers.jl")

@testset "Phase A — RuntimeContext injection seam" begin
    # ---------- (1) Constructor holds the supplied rtctx ----------
    @testset "GlobalContext kwarg ctor stores rtctx" begin
        rt = defaultRuntimeContext()
        ctx = GlobalContext(; rtctx = rt)
        @test ctx.rtctx === rt
    end

    # ---------- (2) Inference reads through rtctx (end-to-end) ----------
    @testset "rtctx.methodsOf is consulted by inference" begin
        call_count = Ref(0)
        recording_methodsOf = function (f)
            call_count[] += 1
            return methodsOf(f)
        end

        rt = RuntimeContext(MethodsOfFn(recording_methodsOf))

        # `show_abstract_arg.jl` has a typed (non-@nospecialize) caller
        # `bad()::Nothing` that calls `show(stdout, 42)`. `stdout::IO` is
        # abstract, so the call site is imprecise — inference reaches
        # `_escapeHatchMask`, which now reads `rtctx.methodsOf`.
        path = joinpath(FEATURES_DIR, "cases", "dispatch", "show_abstract_arg.jl")
        wrapper = Module(:RuntimeContextMWE)
        Base.invokelatest(Base.include, wrapper, path)
        target_mod = Base.invokelatest(getfield, wrapper, :DispatchShowAbstractArg)

        ctx = GlobalContext(; rtctx = rt)
        Base.invokelatest(SimpleTypeChecker.API.addFile!, ctx, target_mod, path)
        open("/dev/null", "w") do devnull
            redirect_stdout(devnull) do
                Base.invokelatest(SimpleTypeChecker.API.runCheck!, ctx)
            end
        end

        # If the seam is wired correctly, the recording wrapper fires at
        # least once during the escape-hatch check on `reraise(::Any)`.
        @test call_count[] > 0
    end
end
