// C-host validation for libsimpletypechecker.so.
//
// The .so was built with `juliac --trim --output-lib`. It links libjulia
// internally and exposes `check_main(const char*) -> int` via @ccallable.
//
// Trying to dlopen this .so from a host Julia process aborts (two libjulias
// clashing on TLS in jl_adopt_thread). A non-Julia host such as this one
// is the correct consumer — that is the "real-setting" two-runtime case.
//
// Build:
//   gcc -O2 bin/c_host.c -o bin/build/c_host \
//       -L bin/build -lsimpletypechecker \
//       -Wl,-rpath,'$ORIGIN'
//
// Run:
//   ./bin/build/c_host test/case/case.jl

#include <stdio.h>
#include <stdlib.h>

extern int check_main(const char *path);

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <file_to_check>\n", argv[0]);
        return 2;
    }
    fprintf(stderr, "[c_host] calling check_main(\"%s\")\n", argv[1]);
    int rc = check_main(argv[1]);
    fprintf(stderr, "[c_host] check_main returned %d\n", rc);
    return rc;
}
