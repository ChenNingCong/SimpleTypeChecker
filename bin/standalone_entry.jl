# Standalone trimmed-library entry point.
#
# Goal: validate the RuntimeContext membrane in the "real" setting —
# compile the inferencer + its CompilerAdaptor driver into a shared
# binary via `juliac --trim`, then run it from a fresh process. If the
# membrane is truly tight, this build succeeds and the resulting binary
# runs end-to-end without any open-world dispatch beyond what the
# driver explicitly exposes.
#
# Build options (from bin/build_standalone.jl):
#   exe  -> ./bin/build/sttc-check <file>            # self-contained
#   lib  -> ./bin/build/libsimpletypechecker.so      # for C/Rust hosts
#
# The exe form is what we use to drive the test harness. The lib form
# is interesting for the "real" two-runtime story (a non-Julia host
# loads us by dlopen) but isn't loadable from a host Julia process —
# two libjulias would clash over TLS / GC / world state.

using SimpleTypeChecker

# Apply the JuliaSyntax patch HERE, at script-top-level, BEFORE the trim
# driver walks the call graph. This script is `include`d by juliac (not
# precompiled), so top-level statements run every build and the modified
# Base.JuliaSyntax method table is what trim's reachability analysis
# snapshots. Patching from SimpleTypeChecker's `__init__` would be too
# late (a trim binary never runs `__init__` before its method table is
# baked).
SimpleTypeChecker._apply_juliasyntax_patch!()

# Core check entry — given a path String, run the inferencer with the
# default (live-JIT) driver rtctx. Returns 0 on success or 1 on errors
# (the StringBuilder writers already wrote diagnostics to stderr).
#
# We DON'T catch here during initial validation: a swallowed exception
# would mask runtime-coupling issues we're trying to surface. Add a
# catch once the happy path is solid.
function check_path(path::String)::Cint
    ctx = SimpleTypeChecker.Inference.GlobalContext()
    SimpleTypeChecker.Inference.getGlobalExpander(ctx)
    SimpleTypeChecker.API.addFile!(ctx, Main, path)
    SimpleTypeChecker.API.runCheck!(ctx)
    return Cint(0)
end

# C-ABI entry for the lib form — used by a non-Julia host that dlopens
# the .so and calls `check_main(file_path_cstring)`.
Base.@ccallable function check_main(path_ptr::Cstring)::Cint
    return check_path(Base.unsafe_string(path_ptr))
end

# Executable entry (`@main` convention, Julia 1.12+). Invoked as
# `./bin/build/sttc-check <file>` from a shell. ARGS[1] is the path
# to check. Exit codes:
#   0 — success
#   1 — diagnostics fired (one or more type errors)
#   2 — usage error (missing argument)
function (@main)(args::Vector{String})::Cint
    if isempty(args)
        Core.println(Core.stderr, "usage: sttc-check <file_to_check>")
        return Cint(2)
    end
    return check_path(args[1])
end
