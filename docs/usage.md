# Usage guide

Practical how-to for running SimpleTypeChecker against your own code.
For interpreting the diagnostics it emits, see
[`error_codes.md`](error_codes.md).

## Installation

```julia
pkg> add https://github.com/ChenNingCong/SimpleTypeChecker
```

Julia 1.12+ is required (we use post-1.12 `Core.Compiler` entry points
and the new `typeinf` signature — see [`project_julia_compat`](../memory/project_julia_compat.md)).

If you don't want to add the checker as a dependency of every project
you check, use the shared `@typecheck` environment pattern:

```julia
# In ~/.julia/environments/typecheck/Project.toml, add SimpleTypeChecker.
# Then prepend that env to LOAD_PATH so it's visible from any project:
julia> push!(LOAD_PATH, "@typecheck")
julia> using SimpleTypeChecker
```

This keeps the checker out of your target package's `Project.toml`.

## The public API

```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()           # fresh context
SimpleTypeChecker.API.addFile!(ctx, mod, filepath)          # queue a file
SimpleTypeChecker.API.runCheck!(ctx; checkSyntax=true)      # run analysis
SimpleTypeChecker.API.@nocheck fun                          # opt out one method
SimpleTypeChecker.API.check(ctx, f, tt)                     # check one specialization
SimpleTypeChecker.API.checkPackage(mod)                     # one-shot a whole package
SimpleTypeChecker.API.checkFile(mod, path)                  # one-shot a single file
```

### Three entry-point shapes

The checker has three useful entry shapes depending on what you have.

**Shape A — whole package, no MI harvest.** Quickest. Walks every
`.jl` file in `pathof(mod)`'s directory and analyzes the declared
method definitions:

```julia
using YourPackage
using SimpleTypeChecker
SimpleTypeChecker.API.checkPackage(YourPackage)
```

Limitation: methods written `f(x::Foo)` where `Foo{T}` is parametric
get analyzed with `T` unbound, which produces false positives on
field accesses (`getproperty on non-concrete type Foo{T} where T`).
For accurate analysis of parametric methods, use shape C.

**Shape B — single file, against an existing module.**

```julia
include(joinpath(pkgdir(YourPackage), "src", "YourPackage.jl"))
ctx = SimpleTypeChecker.API.checkFile(YourPackage, "src/utils.jl")
```

The `mod` argument is the module the file's top-level definitions
belong to — usually the package's main module, NOT a module the file
itself declares with `module Foo … end`. See the wrapper-name caveat
in [`project_install_friction`](../memory/project_install_friction.md).

**Shape C — concrete `MethodInstance` queue from a real test run.**
This is the most accurate path. Requires harvesting MIs first — see
[`harvesting_method_instances.md`](harvesting_method_instances.md).
Briefly:

```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()
# 1. Add source files so the checker has ASTs.
for f in walk_jl(pkgdir(YourPackage))
    SimpleTypeChecker.API.addFile!(ctx, YourPackage, f)
end
# 2. Queue concrete MIs harvested from `Pkg.test` (filtered to drop
#    MIs whose params include abstract types — see harvesting doc).
for mi in concrete_mis
    push!(ctx.queue, SimpleTypeChecker.Inference.KwFunCall(mi))
end
# 3. Run.
SimpleTypeChecker.API.runCheck!(ctx)
```

### Per-specialization spot-check

For `code_warntype`-style one-off checks:

```julia
SimpleTypeChecker.API.check(ctx, sin, (Float64,))
SimpleTypeChecker.API.@code_checked ctx sum([1.0, 2.0])
```

Both compute a `MethodInstance` via `Base.code_typed(f, tt)` and push
it onto the queue, then run.

## Tuning what fires — GlobalContext flags

Set these on `ctx` before calling `runCheck!`.

| Flag | Default | What it does |
|---|---|---|
| `checkSyntax` (arg to `runCheck!`) | `true` | Reports `JTC-UNSUPPORTED` / `JTC-SYNTAX` for AST shapes the inferencer can't model. Turn off if you only care about type errors. |
| `permitRequiredKwargs` | `false` | Required keyword args (no default value) — silenced when set. Idiomatic in many APIs, false-positive without this flag. |
| `permitCustomGetproperty` | `false` | Allow types that override `Base.getproperty` (virtual properties — e.g. `PDMats.PDiagMat.dim`). Otherwise the checker insists property names match `fieldnames`. |
| `deepDispatchCheck` | `false` | Report transitive dynamic-dispatch sites (inside callees too, not just the top-level method body). Heavy; produces many fires on real code. |
| `printDispatchTrace` | `false` | When `deepDispatchCheck` fires, include a `· <method> @ <file>:<line> — <callee>(...)` list of the offending transitive calls. |

Example: a permissive run that ignores syntax limitations and accepts
abstract-arg keyword forwarding:

```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()
ctx.permitRequiredKwargs = true
ctx.permitCustomGetproperty = true
SimpleTypeChecker.API.addFile!(ctx, YourPackage, file)
SimpleTypeChecker.API.runCheck!(ctx; checkSyntax = false)
```

## Opting out a method — `@nocheck`

```julia
SimpleTypeChecker.API.@nocheck function highly_dynamic(x)
    # body uses splat, generators, eval(...) — checker can't handle it
end
```

The method is skipped entirely. Any built-in macro at the function
head (`@inline`, `@inbounds`) has the same effect, because the
checker doesn't recurse through macrocall heads. That makes
`@nocheck` interchangeable with `@inline` for the "skip me" purpose if
you'd rather not depend on SimpleTypeChecker at all.

See [`nocheck_inventory.md`](nocheck_inventory.md) for the patterns
this is currently used for in the checker's own source.

## Output format

Diagnostics use the rustc shape — primary span, source line with a
caret underline, secondary `note:` blocks and `help:` lines. Sample:

```
error[JTC-ASSIGN]: reassigned type Float64 is incompatible
   --> .../LowRankArithmetic/src/utils.jl:37:9
   |
37 |         s += σ^2
   |         ^^^^^^^
   |
note: previous type is Int64, from .../utils.jl:34:5
   |
34 |     s = 0
   |     ^^^^^
   |
help: change s = 0 to s = zero(eltype(S)) (or 0.0 if S is Float64-typed)
```

The error code (the `JTC-…` part) is stable across versions — your
tooling can pattern-match on it. The headline text is human-readable
and may drift. See [`error_codes.md`](error_codes.md) for the full
list.

## CI integration

`runCheck!` writes diagnostics to `ctx.errio` (defaults to `stderr`).
Process exit code is not affected — you need to check whether anything
fired yourself:

```julia
ctx = SimpleTypeChecker.API.checkPackage(YourPackage)
exit(ctx.errCount > 0 ? 1 : 0)
```

For more elaborate gating (allow N existing errors, fail on new ones),
parse the diagnostic stream and compare error codes against a
checked-in baseline. Codes are line-stable; locations drift, so don't
key the baseline on file:line:col.

## Standalone binary

The checker can also be built as a trimmed shared library / executable
via `juliac --trim` and run from a fresh process — useful if you want
to drive it from a non-Julia host or avoid paying Julia startup per
file. See [`trim_integration.md`](trim_integration.md).
