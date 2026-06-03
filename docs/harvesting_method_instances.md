# Harvesting concrete `MethodInstance`s

For accurate analysis of parametric code, the checker should be fed
**concrete** `MethodInstance`s — ones whose signature parameters are
all leaf types (`Matrix{Float64}`, `Tuple{Int, String}`), not abstract
supertypes (`AbstractMatrix`, `Tuple`) or unbound type variables
(`Matrix{T} where T`).

The default `checkPackage(mod)` path is **declared-signature**
seeding: every method def is queued with its source-declared sig.
For a method written `f(x::Foo)` where `Foo{T}` is parametric, the
declared sig is `Tuple{typeof(f), Foo}` with `T` unbound. The
inferencer then can't resolve `x.field` (because `T` is unbound),
producing `getproperty on non-concrete type Foo{T} where T`
warnings that aren't real bugs.

The fix is to harvest **concrete** specializations from a real
execution and queue those instead. This document is the recipe.

## The Pelenitsyn pattern

From Pelenitsyn et al., OOPSLA 2021 ([prl-julia/julia-type-stability](https://github.com/prl-julia/julia-type-stability)) —
the standard technique for getting concrete specializations out of a
Julia package:

1. `Pkg.activate(path)` + `Pkg.instantiate()` — fresh environment per
   package.
2. `Pkg.test(pkg)` — runs the test suite. Tests cover the package's
   public API and exercise it on concrete types. The JIT populates
   the method-instance table along the way.
3. Walk every `Method` defined in `pkg`'s modules. For each, call
   `Base.specializations(method)` and iterate.
4. Each entry yields a `MethodInstance` whose `sig` is concrete.

```julia
using YourPackage

function harvest_concrete_mis(mod::Module)
    mis = Core.MethodInstance[]
    for name in names(mod; all = true)
        isdefined(mod, name) || continue
        v = getfield(mod, name)
        v isa Function || continue
        for m in methods(v)
            m.module === mod || continue       # skip methods added in other modules
            for mi in Base.specializations(m)
                mi isa Core.MethodInstance || continue
                push!(mis, mi)
            end
        end
    end
    return mis
end
```

You'd usually invoke `Pkg.test` first in the same process (or have
the package's own `__init__` precompile work do the JIT triggering).

## Filter: drop MIs with abstract type parameters

`Base.specializations` returns specializations *as the compiler stored
them*. That includes specializations the compiler synthesized over
abstract types (e.g. when `@nospecialize` was used, or when the
method's declared sig already had an abstract slot the compiler
didn't specialize beyond). For our purposes those are still
"non-concrete" and we want to drop them:

```julia
function _spec_is_analyzable(sig)
    sig isa DataType || return false
    sig <: Tuple || return false
    isempty(sig.parameters) && return false
    for p in sig.parameters
        # Abstract types, UnionAlls, Unions all fail this check
        Base.isconcretetype(p) || return false
    end
    return true
end

concrete = filter(mi -> _spec_is_analyzable(mi.specTypes), all_mis)
```

`Base.isconcretetype` already rejects abstract types, `UnionAll`,
and `Union` (except the trivial single-element case). The 23-package
stage-5 corpus confirmed this is the right cut — see
[`large_scale_test_methodology.md`](large_scale_test_methodology.md).

## Feed them to the checker

```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()
ctx.permitRequiredKwargs = true        # depends on your package
ctx.permitCustomGetproperty = true     # depends on your package

# 1. Add every source file from the package — gives the checker ASTs.
pkg_dir = dirname(pathof(YourPackage))
for (root, _dirs, files) in walkdir(pkg_dir)
    for f in files
        endswith(f, ".jl") || continue
        SimpleTypeChecker.API.addFile!(ctx, YourPackage, joinpath(root, f))
    end
end

# 2. Queue the harvested concrete MIs.
for mi in concrete_mis
    push!(ctx.queue, SimpleTypeChecker.Inference.KwFunCall(mi))
end

# 3. Run.
SimpleTypeChecker.API.runCheck!(ctx)
```

Note: do NOT also call `seedAllRegistered!(ctx)` — that re-seeds the
queue with declared (shape-only) sigs and brings back the false
positives you just avoided. See [`large_scale_test_methodology.md`](large_scale_test_methodology.md).

## Running per-package in a fresh process

Method-table state from one package can leak into the next if you
run them in one Julia session — e.g. a method added by package A
becomes visible when analyzing package B. The safe pattern is one
subprocess per package:

```bash
for pkg in YourPkg1 YourPkg2 YourPkg3; do
    timeout -s INT -k 10 90 julia --project=. \
        bin/check_concrete.jl "$pkg" > "diags/$pkg.txt" 2>&1
done
```

Notes from running 23 packages this way:

- Use `-s INT` (SIGINT) — Julia ignores SIGTERM during precompile / GC.
  See [`feedback_30s_check_timeout`](../memory/feedback_30s_check_timeout.md).
- Add internal task-level timeouts on `using YourPkg` (40 s) and
  `Pkg.test` (30 s) — some packages stall on precompile or have flaky
  tests. The external `timeout` is a safety net, not the primary cut.
- Never `2>&1 | tail` a long-running checker — the pipe buffer fills
  and the producer blocks; looks like a hang. Redirect to a tempfile
  and `tail -f` separately. See [`feedback_long_process_no_pipe`](../memory/feedback_long_process_no_pipe.md).
- Cap parallelism at 5 (`xargs -P 5`) — JIT contention dominates above that.

## When tests aren't available

If the package has no test suite, or tests don't reach the code path
you care about, fall back to writing a small driver that calls each
top-level entry point with concrete arguments:

```julia
# spec_driver.jl — a hand-rolled "test" that drives JIT specialization
using YourPackage
YourPackage.entry_point_1(Float64[1, 2, 3])
YourPackage.entry_point_2(Matrix{Float64}(undef, 3, 3))
# … etc
```

Then harvest specializations the same way after this driver finishes.

The `seedAllRegistered!` helper (in
`src/compiler/CompilerAdaptor.jl`) is still available as an escape
hatch when neither tests nor a driver are practical. Be aware that
it reproduces the declared-sig shape-seeding behavior and you'll see
the false positives described above. Use only when no concrete
seeding option exists.

## Why this matters

Round-1 on the 8-package numerical corpus saw ~85% of fires
(179/211) be shape-seeding artifacts — see
[`checker_limitations.md`](checker_limitations.md). Switching to
concrete-MI seeding eliminated essentially all of them and surfaced
4 real bugs across 23 packages (`docs/package_bugs.md`).

Concrete seeding is the difference between "this checker fires on
everything" and "this checker fires on real problems."
