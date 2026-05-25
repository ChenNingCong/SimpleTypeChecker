# Smoke test for the trimmed library — dlopens libsimpletypechecker
# and invokes `check_main` from a fresh Julia process.
#
# Usage:
#   julia bin/load_and_run.jl <file_to_check>
#
# Important: this script must NOT do `using SimpleTypeChecker`. The
# whole point is to validate the library is callable WITHOUT the
# package being available in the host process — proving the
# membrane is real and the inferencer is self-contained.

const LIB = joinpath(@__DIR__, "build",
    Sys.islinux()  ? "libsimpletypechecker.so" :
    Sys.isapple()  ? "libsimpletypechecker.dylib" :
                     "libsimpletypechecker.dll")

if !isfile(LIB)
    error("Library not built: $LIB\nRun bin/build_standalone.jl first.")
end

# Resolve user input
file_to_check = if length(ARGS) >= 1
    abspath(ARGS[1])
else
    # Default smoke target — a small fixture we know parses cleanly.
    abspath(joinpath(@__DIR__, "..", "test", "case", "case.jl"))
end

@info "Loading library + invoking check_main" LIB file_to_check

# Initialize Julia's runtime in the child — required when dlopen'ing
# a trim-built library so its __init__'s can fire.
# The library exports `init_<libname>` we should call first.
lib = Base.Libc.Libdl.dlopen(LIB)
init_sym = "init_libsimpletypechecker"
init_fp  = Base.Libc.Libdl.dlsym(lib, init_sym; throw_error = false)
if init_fp !== nothing
    ccall(init_fp, Cvoid, ())
    @info "Library __init__ chain fired"
end

check_fp = Base.Libc.Libdl.dlsym(lib, :check_main)
rc = ccall(check_fp, Cint, (Cstring,), file_to_check)
@info "check_main returned" rc

exit(Int(rc))
