# Workload-based precompile. Drives the real `addFile!` → `runCheck!`
# pipeline over a tiny inline fixture (precompile_fixture.jl) during
# package precompile, so Julia caches every MethodInstance it visits
# along the way. First-call JIT cost in a fresh process drops from
# seconds to near-zero.
#
# More robust than hand-curated `precompile(f, types)` lines — those
# silently no-op when a signature drifts, which is what happened to
# the previous list (e.g. `checkToplevelFunction`'s third arg moved
# from `MethodHandle` to `Dict{Symbol, CompileType}`).
#
# Why a real on-disk file rather than a tempfile: Julia stores the
# fixture path in the .ji cache as a source dependency. A tempfile
# vanishes after precompile and the cache then re-validates on every
# `using`, defeating the point.

# Dedicated submodule so the workload doesn't pollute Main and doesn't
# trip Julia's "unclosed module" warning during incremental compile.
module __SimpleTypeCheckerPrecompileTarget__
end

let
    path = joinpath(@__DIR__, "precompile_fixture.jl")
    try
        target = __SimpleTypeCheckerPrecompileTarget__
        Base.include(target, path)
        ctx = Inference.GlobalContext()
        Inference.addFile!(ctx, target, path)
        ctx.printDispatchTrace = true
        redirect_stdout(devnull) do
            Inference.runCheck!(ctx)
        end
    catch
        # Precompile workload must never break package load. If the
        # fixture or API drifts, we lose the cache — the package still
        # works, just slower on first call.
    end
end
