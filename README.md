# SimpleTypeChecker

A static type checker for Julia. Designed to enforce a C-style
"concrete-types-everywhere" discipline on new code. Best when your
codebase has not yet accumulated heavy dynamic dispatch — it isn't a
retrofit tool for messy code.

## Install

```julia
pkg> add https://github.com/ChenNingCong/SimpleTypeChecker
```

Julia 1.12+ is required.

## Quick start

```julia
using YourPackage
using SimpleTypeChecker
SimpleTypeChecker.API.checkPackage(YourPackage)
```

That's it. Diagnostics print to stderr in a rustc-shape format:

```
error[JTC-ASSIGN]: reassigned type Float64 is incompatible
   --> .../utils.jl:37:9
   |
37 |         s += σ^2
   |         ^^^^^^^
   |
note: previous type is Int64, from .../utils.jl:34:5
```

For one-off spot-checks (à la `code_warntype`):

```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, YourPackage, "/path/to/file.jl")
SimpleTypeChecker.API.runCheck!(ctx)
```

### What gets auto-checked

`checkPackage` / `addFile!` only **auto-queues** method definitions
whose declared signature is fully concrete — every argument type is a
leaf type, with no `where T` (`UnionAll`) and no abstract supertype
(`AbstractArray`, `Number`, `Union{…}`). The "C-style signature" set.
A method like:

```julia
f(x::Vector{Float64}, n::Int) = ...     # ✓ auto-checked
g(x::AbstractArray) = ...               # ✗ skipped (abstract)
h(x::T) where T<:Number = ...           # ✗ skipped (UnionAll)
```

For methods that aren't auto-checked, queue specific specializations
explicitly with `check(ctx, f, tt)` (where `tt` is a tuple of types,
NOT a `Tuple{…}` type):

```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, YourPackage, "src/utils.jl")
SimpleTypeChecker.API.check(ctx, YourPackage.g, (Vector{Float64},))
SimpleTypeChecker.API.check(ctx, YourPackage.h, (Float64,))
```

Or write a `main`-function driver that calls every entry point with
concrete arguments — the checker recurses through the call sites
inside, so each call inside `main` seeds checking of that
specialization:

```julia
function main()
    YourPackage.g(rand(10))
    YourPackage.h(3.14)
end
SimpleTypeChecker.API.check(ctx, main, ())
```

For batch harvesting from a real test run (the recommended path for
analyzing parametric packages), see
[`docs/harvesting_method_instances.md`](docs/harvesting_method_instances.md).

### Opting out a method

To skip a method the checker can't model:

```julia
SimpleTypeChecker.API.@nocheck function highly_dynamic(x)
    # …body uses splat/eval/generators — checker won't recurse here
end
```

## Documentation

- [`docs/usage.md`](docs/usage.md) — full usage guide, including
  context flags (`permitRequiredKwargs`, `permitCustomGetproperty`,
  `deepDispatchCheck`), the three entry-point shapes, and CI
  integration.
- [`docs/error_codes.md`](docs/error_codes.md) — what each `JTC-…`
  code means, when it's a real bug vs a checker limitation, and how
  to suppress.
- [`docs/syntax.md`](docs/syntax.md) — supported / unsupported AST
  shapes. Read this before opening a "false positive" issue — it's
  probably an unsupported form with a known workaround.
- [`docs/harvesting_method_instances.md`](docs/harvesting_method_instances.md) — for
  accurate analysis of parametric code: harvest concrete
  `MethodInstance`s from a real test run (Pelenitsyn-style) and feed
  them in.
- [`docs/trim_integration.md`](docs/trim_integration.md) — combine
  with `juliac --trim` either as a build target (ship a standalone
  `sttc-check` binary) or as an alternative MI-harvesting source.
- [`docs/checker_limitations.md`](docs/checker_limitations.md) —
  known false-positive classes and their fix status.
- [`docs/package_bugs.md`](docs/package_bugs.md) — real bugs the
  checker has surfaced in published packages (treat as a sanity check
  for whether the checker is finding what it should).
- [`docs/isolation.md`](docs/isolation.md) — the membrane design
  separating analyzer / driver. Read this if you want to embed the
  checker.
- [`docs/ts_checker_algorithm.md`](docs/ts_checker_algorithm.md) —
  the inference algorithm itself.

## How it works (briefly)

1. SimpleTypeChecker reads each method definition from the source files
   via JuliaSyntax and builds a lossless AST.
2. For each method, it walks the AST and tracks the type of every
   local variable / expression. When it hits a function call into code
   it has no AST for (e.g. `Base`), it asks Julia's own type inferencer
   for the return type.
3. Diagnostics fire when the static type analysis sees something that
   doesn't fit — incompatible reassignment, abstract-arg dispatch,
   property access on a non-concrete type, etc.

Inference for recursive functions is generally undecidable, so we
deliberately do not recurse into bodies — Julia's type inferencer is
the oracle at the boundary.

## Limitations (the permanent ones)

These are by design — they're the price for clean, source-located
diagnostics. The README's older revisions discussed each of these in
depth; the short version:

- **Abstract-arg calls are unconditionally rejected.** Wrap abstract
  values in a struct, manipulate via interface methods.
- **No heap analysis, no complex narrowing.** Single `isa` /
  `=== nothing` narrowing only. Do calculations in short
  `@nocheck` functions if you need more.
- **No implicit `convert` modeling.** `push!([1,2,3], "1")` is a
  legitimate Julia call — we don't try to flag it.
- **`if`-block scope is unified.** `if y; x = 1 else x = "1" end`
  fires `JTC-IF` (type enlarged). Use a typed slot:
  `local x::Union{Int, String}` to make the union explicit.

The full reasoning lives in the older READMEs at `git log -- README.md`
if you're curious about the design history.
