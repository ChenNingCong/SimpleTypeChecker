# Combining with `juliac --trim` for data harvesting

`juliac --trim` is Julia's static-image builder. It walks reachable
code from a designated entry point and ahead-of-time compiles
everything it needs, emitting a self-contained binary (executable or
shared library). Anything not statically reachable is dropped —
hence the "trim".

This is interesting for our purposes for **two distinct reasons**:

1. **As a build target for the checker itself.** We ship
   `bin/build_standalone.jl` that uses `juliac --trim` to produce a
   `sttc-check <file>` binary. Useful for CI (no per-run Julia
   startup) and for embedding the checker in non-Julia hosts.

2. **As a data source for what's reachable in your package.**
   Trim's reachability walk visits every method instance the program
   could call. We can use that walk to harvest concrete MIs without
   running tests — the trim "discovery" phase is the harvest.

This document covers both. The user-facing one is the second; build
details are in [`isolation.md`](isolation.md).

## Trim as a method-instance discoverer

Pelenitsyn-style harvesting (see
[`harvesting_method_instances.md`](harvesting_method_instances.md))
needs a test suite to drive specialization. Trim is an alternative
when:

- The package doesn't have tests (or tests miss your code path).
- You want to know what specializations are reachable from a
  specific entry point — e.g. one `main` function — rather than
  everything the tests exercise.
- You want to verify the entry-point closure trims cleanly (no
  abstract-dispatch leaks) as part of the same pass.

### The flow

1. Write a tiny driver that calls your package's entry points with
   concrete arguments:

    ```julia
    # entry.jl
    using YourPackage
    Base.@ccallable function main()::Cint
        result = YourPackage.compute(Float64[1, 2, 3])
        @show result
        return Cint(0)
    end
    ```

2. Build with `juliac --trim`:

    ```bash
    julia --project=. $JULIA_SHARE/julia/juliac/juliac.jl \
        --output-exe out \
        --experimental --trim=safe \
        --compile-ccallable \
        --verbose \
        entry.jl 2>&1 | tee trim.log
    ```

   `--trim=safe` refuses the build if any reachable site is unsafe
   (open-world dispatch, eval, …). Use `--trim=unsafe-warn` to get
   warnings instead — each warning is one place where trim's static
   walk hit a non-resolvable site. Those are the same patterns the
   type-checker flags as `JTC-DISPATCH` / `JTC-DEEP-DISPATCH`.

3. The trim log lists every method instance that ended up in the
   image. With `--verbose`, you get one line per compiled MI.
   Capture them via the log, then feed back into the checker:

    ```bash
    # Extract MI specs from the trim log
    grep -oP 'MethodInstance for \K[^\n]+' trim.log > reachable_mis.txt
    ```

   The exact log shape varies by Julia version — parse the format
   your build actually produces. With Julia 1.12 `juliac` we get
   lines like `compiling MethodInstance for foo(::Float64)`.

4. From inside Julia, query `Base.specializations` on the same
   methods you saw in the log — the trim discovery walk already
   forced the JIT to materialize them in this session, so they're
   queryable:

    ```julia
    include("entry.jl")     # forces the same JIT path
    mis = harvest_concrete_mis(YourPackage)   # see harvesting doc
    ```

5. Filter to the ones trim said were reachable (intersect with
   `reachable_mis.txt`), and feed into the checker as in the
   harvesting doc.

### Why the intersection matters

`Base.specializations` returns every spec the compiler ever
synthesized in this session — including ones generated during
package loading, `__init__`, precompile hooks, internal helpers
that don't actually run from your entry point. If you care about
"what's reachable from my `main`", trim's reachability is the
ground truth.

For "what's reachable from anywhere I might call", just use the
test-driven harvest from
[`harvesting_method_instances.md`](harvesting_method_instances.md).

## Trim's warning list as a checker-equivalent signal

When trim runs in `unsafe-warn` mode, every warning is a site where
trim's static walk couldn't resolve to a single concrete callee. The
overlap with our `JTC-DISPATCH` / `JTC-DEEP-DISPATCH` is high — both
analyses are looking for "the runtime would need the method table
here." Comparing the two reports is a useful cross-check:

| Trim says | Checker says | Likely cause |
|---|---|---|
| Warning at X | `JTC-DISPATCH` at X | Real abstract-arg site — fix or `@nocheck` |
| Warning at X | No fire | Checker is being more permissive (e.g. `@nospecialize`-aware escape hatch) — investigate |
| No warning | `JTC-DISPATCH` at X | Site isn't actually reachable from trim's entry point, or trim resolved it via constprop |

The latter two cases are the interesting ones — they're where the
two analyses disagree, and usually one of them needs work.

See [`project_trim_is_dead_end.md`](../memory/project_trim_is_dead_end.md)
for why we don't rely on trim alone as a linter: it catches problems
too late in the pipeline (after the build attempt), and the
diagnostic location is buried in compiler internals rather than
pointing at your source. The static checker is faster and reports at
the source level; trim is a sound cross-check.

## Building the checker itself with trim

`bin/build_standalone.jl` does this. Two targets:

```bash
julia bin/build_standalone.jl exe     # ./bin/build/sttc-check
julia bin/build_standalone.jl lib     # ./bin/build/libsimpletypechecker.so
```

The exe form is self-contained — its own libjulia init, you run it
as `sttc-check <file>` from a shell. Useful for CI to avoid paying
~1.5 s of Julia startup per file:

```bash
for f in src/*.jl; do
    ./bin/build/sttc-check "$f"
done
```

The lib form is the C-ABI shared library. The `check_main(path)`
entry takes a `Cstring` and returns `Cint` (0 on success, 1 on
errors, 2 on usage error). Suitable for embedding the checker in a
non-Julia host (`dlopen` + lookup `check_main`). It is **not**
loadable from a separate Julia process — two libjulias would clash
over TLS and GC. See [`isolation.md`](isolation.md).

## Trim status: caveats

We build with `--trim=unsafe-warn`, not `--trim=safe`. The remaining
unsafe-warn lines come from:

- The runtime patch in `patch.jl` that rewrites
  `Base.JuliaSyntax.parse_block` — trim sees the unpatched method
  bodies at compile time. The patch is applied at the entry
  script's top-level via `_apply_juliasyntax_patch!()` so that
  trim's reachability walk picks it up. See
  [`feedback_trim_patches_at_script_top_level.md`](../memory/feedback_trim_patches_at_script_top_level.md).
- Residual abstract-dispatch sites the self-check still flags. The
  long-term goal is `--trim=safe` for the checker itself, but it's
  not there yet.

For data-harvesting purposes, `unsafe-warn` is fine — you only need
the reachability output, not a strict-mode build.
