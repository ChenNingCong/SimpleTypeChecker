# Build SimpleTypeChecker as a trimmed shared library via `juliac`.
#
# Output: `bin/build/libsimpletypechecker.so` (Linux) — load via dlopen
# and call the `check_main` entry from any host.
#
# Trim mode: `unsafe-warn` is required today because of:
#   - patch.jl's runtime method-table rewrite (Base.JuliaSyntax.parse_block)
#     applied from __init__ — juliac's static verifier sees the
#     unpatched method bodies at compile time
#   - any residual abstract-dispatch sites still reported by self-check
#
# Each new "unsafe-warn" line in the build output is a real coupling
# we haven't eliminated yet. This script's job is to surface them so
# we can attack them one by one.

const PKG_ROOT = abspath(joinpath(@__DIR__, ".."))
const JULIAC = joinpath(Sys.BINDIR, "..", "share", "julia", "juliac", "juliac.jl")
const OUT_DIR = joinpath(PKG_ROOT, "bin", "build")
const OUT_LIB = joinpath(OUT_DIR, Sys.islinux() ? "libsimpletypechecker.so" :
                                  Sys.isapple() ? "libsimpletypechecker.dylib" :
                                                  "libsimpletypechecker.dll")
const OUT_EXE = joinpath(OUT_DIR, "sttc-check")
const ENTRY   = joinpath(PKG_ROOT, "bin", "standalone_entry.jl")

mkpath(OUT_DIR)

# `target` is "exe" (default) or "lib", controlled by ARGS[1]. The exe
# form is self-contained (its own libjulia init); the lib form is for
# loading from a C host.
target = length(ARGS) >= 1 ? ARGS[1] : "exe"

if target == "exe"
    cmd = `$(Base.julia_cmd()) --project=$(PKG_ROOT) $(JULIAC)
            --output-exe $(OUT_EXE)
            --experimental --trim=unsafe-warn
            --compile-ccallable
            --verbose
            $(ENTRY)`
    @info "Building standalone executable" out=OUT_EXE
elseif target == "lib"
    cmd = `$(Base.julia_cmd()) --project=$(PKG_ROOT) $(JULIAC)
            --output-lib $(OUT_LIB)
            --experimental --trim=unsafe-warn
            --compile-ccallable
            --verbose
            $(ENTRY)`
    @info "Building standalone library" out=OUT_LIB
else
    error("usage: julia bin/build_standalone.jl [exe|lib]  (got: $target)")
end

@info "Command" cmd
run(cmd)
@info "Build complete"
