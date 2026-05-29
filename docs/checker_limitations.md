# Why the checker fires on real packages — notes from the 8-pkg run

Scope: 8 numerical packages from the parse_ok corpus
(AffineMaps, PDMatsSingular, PenaltyFunctions, LinearMapsAA,
PSDMatrices, LowRankArithmetic, FastRationals, KroneckerProducts).

## Fixed (since this analysis was written)

| Item | Commit |
|---|---|
| Parser N-dim concat (`ncat`/`typed_ncat`) flag bits | `0c02a1f` |
| `permitRequiredKwargs` flag for required keyword args | `0c02a1f` |
| Builtin string-macro result types (`@r_str`, `@artifact_str`, `@v_str`, `@b_str`, `@raw_str`, `@cmd`, `@big_str`, ...) | `0c02a1f` |
| `=== nothing` type narrowing | `0c02a1f` |
| `runCheck!` defensive per-MI exception handling (#4c) | `0c02a1f` |
| `checkPackage` single-file-package path (#5) | `0c02a1f` |
| `seedAllRegistered!` removed from `checkPackage` default — concrete MIs only | `0c02a1f` |
| `permitCustomGetproperty` flag for virtual properties (PDMats's `PDiagMat.dim`) | `29ef426` |
| Scope analyzer recurses into macrocall args (was early-exit; `@inbounds for` crashed) | `7ddd319` |
| JTC-INTERNAL renders stacktrace (filtered to checker frames) for debuggability | `7ddd319` |
| `iterateCheck` soften bare `error()` → Any-typed fallback for non-standard iterators | `1ad67cc` |

With the concrete-MI flow now in place, the original 211-fire breakdown
no longer represents reality. See "post-fix re-run" further down.

> **Important caveat about this run.** The results below came from a
> default `checkPackage(mod)` flow that internally seeded the work
> queue via `seedAllRegistered!` — i.e. it queued every non-parametric
> method def using the method's **declared** signature. For methods
> written `function f(x::Foo)` where `Foo{M}` is parametric, the
> declared sig is `Tuple{typeof(f), Foo}` with `M` unbound. That
> caused a wave of `FIELD: getproperty on non-concrete Foo{M} where M`
> diagnostics that aren't real — they're an artifact of shape-only
> seeding.
>
> **`seedAllRegistered!` is no longer wired into `checkPackage`**. It
> remains in the code base as an escape-hatch helper, but the
> recommended entry point for stage 5 is to feed concrete
> `MethodInstance`s harvested from a real test run (Pelenitsyn-style:
> `Pkg.test(pkg)` + `Base.specializations(method)`) via the existing
> `check(ctx, f, tt)` API. With concrete seeds (`Foo{Matrix{Float64}}`
> instead of `Foo`), the FIELD warnings here would not fire.
>
> The 37 % "shape-only seeding" share in the table below was a
> work-around symptom, not real checker behavior. Keep that in mind
> when reading the rest.

The per-package writeups live in
`scripts/large_scale_test/data/pkg_analysis/*.md`. This file is the
cross-package summary.

## Recorded fire breakdown (211 fires across 8 packages)

| Source category | Count | % | Real bug? | What to do |
|---|---:|---:|---|---|
| Shape-seeding artifact — FIELD `getproperty on T{...} where ...` | 79 | 37 % | no | re-run with concrete MIs |
| Shape-seeding artifact — DISPATCH `call has abstract arg` (downstream of unbound types in the seed) | 100 | 47 % | no | re-run with concrete MIs |
| Parser gap (SYNTAX `; kwargs...` slurp) | 24 | 11 % | no, **checker FP** | parser whitelist (#1) |
| Inferencer gap (splat, default-expr `T(1)`, parametric union isa) | 4 | 2 % | no, **checker limitation** | implement (#2) |
| Other (`fieldnames`, `more than one matching method`) | 4 | 2 % | downstream of seeding | follows from concrete seeds |
| Crashes (parser unicode, inference typeassert, exception escape) | 3 | — | yes, **checker bugs** | targeted fixes (#3) |
| Single-file `checkPackage` skip (PSDMatrices: 0 methods analyzed) | n/a | — | yes, **API design gap** | `checkPackage` (#4) |

**~85 % of fires (179/211) are shape-seeding artifacts** — they
disappear by switching the entry path from `seedAllRegistered!` to
concrete test-harvested MIs:
- The 79 FIELD fires come from `x.field` accesses where `x`'s type is
  parametric and `M` is unbound.
- The 100 DISPATCH fires come from calls inside method bodies where the
  argument's static type is the bare abstract supertype the method
  declared (`a::AbstractMatrix` seeded literally as `AbstractMatrix`,
  not `Matrix{Float64}` from a real call). With a concrete seed,
  arguments resolve to concrete types and downstream calls dispatch
  cleanly.

Per-package volume:

| Package | Fires | Dominated by |
|---|---:|---|
| LinearMapsAA | 78 | 5-param shape seeding (30) + abstract dispatch (28) + kwargs slurp (16) |
| PenaltyFunctions | 47 | abstract `AbstractArray` dispatch (34) |
| AffineMaps | 30 | abstract dispatch (14) + Mul/Add shape (10) + kwargs slurp (8) |
| FastRationals | 24 | shape seeding on `FastRational{T}` (~14) + crash |
| PDMatsSingular | 22 | `Covariance{T,TM}` shape (9) + abstract sparse dispatch (10) |
| LowRankArithmetic | 8 (then crash) | shape seeding + unicode-byte parser crash |
| KroneckerProducts | 2 (then crash) | `LU{T,S,P}`/`Cholesky{T,S}` shape + iterate-mismatch exception escape |
| PSDMatrices | 0 | `checkPackage` skipped the single source file |

---

## What's actually checker work (everything below excludes the shape-seeding row)

### #1 — Conservative `AbstractArray`-class abstract-dispatch warning (~100 fires)

**Pattern**: `error[JTC-DISPATCH]: call has an argument of abstract type`
on a method declared over `AbstractMatrix`, `AbstractVector`,
`AbstractArray`, etc.

The package legitimately declares the function over an abstract supertype
— it's the intentional polymorphism. The checker correctly observes
that calls inside the body don't dispatch to a single concrete method,
but the package author wrote it that way on purpose.

**Examples**:
- AffineMaps: `_bc_muladd(a::AbstractMatrix, b::AbstractVector, c::AbstractVector) = muladd(a, b, c)`
- PenaltyFunctions: `value(p::ElementPenalty, θ::AbstractArray)` operating elementwise
- PDMatsSingular: `logdet(cov::Covariance{T, <:AbstractSparseMatrix{T}}) where T`

**Why a checker should care**: this is the single biggest noise source
at 47 % of all fires. It needs a clean opt-out story:

- `@nocheck` already exists per-method but is heavy-handed (silences everything).
- Better: a per-arg `@anydispatch` annotation that says "I know this
  arg is abstract, trust the caller".
- Or smarter analysis: if every concrete subtype implementation has the
  same return type, the abstract dispatch is type-safe at the level
  of return-typing — don't warn.

### #2 — Parser gap: `; kwargs...` slurp in kwargs slot (24 fires)

**Pattern**: `error[JTC-SYNTAX]: Invalid argument definition` anchored at
the `kwargs...` slurp marker.

**Example** (AffineMaps + LinearMapsAA, 24 sites):
```julia
Base.isapprox(f::Mul, g::Mul; kwargs...) = isapprox(f.A, g.A; kwargs...)
                              ^^^^^^^^^   ← parser fires here
```

`convert2JuAST!`'s flag whitelist (`src/syntax/SyntaxAdaptor.jl:108-118`)
doesn't accept the slurp flag combination inside the kwargs slot of a
method definition. This is idiomatic Julia — every package that forwards
kwargs uses this pattern.

**Fix**: add the slurp flag to the recognised mask. Likely a one-line
whitelist extension.

### #3 — Inferencer gap: splat `...` AST not modeled (3 fires)

**Pattern**: `error[JTC-UNSUPPORTED]: Unsupported AST type ...`.

**Examples**:
- PenaltyFunctions: `SCADPenalty(promote(a, γ)...)`
- PenaltyFunctions: `>(size(A)...) ? ...`
- LinearMapsAA: similar splat at call site

`inferExpr` has no branch for the `:...` splat node. The parser accepts
it; the inferencer doesn't model it.

**Fix**: medium. Need to widen the argument list to the splatted
collection's element type and re-attempt method match.

### #3a — Inferencer gap: default-expr `T(1)` with unbound parametric T (1 fire)

**Pattern**: `error[JTC-METHOD]: no matching method for Any(::Int64)`
on a default value that calls the parametric T as a constructor.

**Example** (PenaltyFunctions):
```julia
SCADPenalty(a::T = 3.7, γ::T = T(1)) where {T <: Number} = SCADPenalty{T}(a, γ)
                                ^^^^   ← `T(1)` analyzed with T = Any
```

**Fix**: small. Substitute `T` with its upper bound (`Number`) when
analyzing default expressions inside the method head.

### Unary `<:T` shorthand inside `Type{<:T}` (#3c) — NEW

**Pattern**: `error[JTC-SYNTAX]: <: requires exactly two arguments` at
`T::Type{<:Exception}` (ArgCheck — 17 sites) or `f(x::Vector{<:Real})`
(typical numerical code).

**Why**: Julia parses `<:Exception` standalone as `Expr(:<:, Exception)`
with one child. Our `inferSubtypeOp` only handles the binary form (the
boolean operator `T1 <: T2`).

**What this actually is**: a bounded TypeVar shorthand. `Type{<:Exception}`
is `Type{T} where T<:Exception` — a UnionAll over a bounded TypeVar. The
unary `<:` doesn't have a standalone CompileType — its only meaningful
context is inside a curly call or `where` clause, where the OUTER
construct should lift the inner bound into a UnionAll.

**Why a quick "collapse to Any" doesn't work**: `Type{T}` is the
SINGLETON type of the value `T` (so `Type{ArgumentError}` has exactly
one member: the value `ArgumentError`). `Type{Any}` therefore only
accepts the value `Any` itself, not other concrete Exception types.
The runtime check `ArgumentError isa Type{<:Exception}` works via
UnionAll semantics, not via `Type{Any}` widening.

**Fix path**: medium-to-large. Requires
(1) extending `makeTypeVarOf` to accept lower/upper bounds (or new op
    `makeBoundedTypeVarOf`),
(2) in `inferCurlyCall`, when an arg's AST is unary `:<:`, build a
    fresh bounded TypeVar and wrap the curly result in a UnionAll
    over that TypeVar.

For now: open — these fire as `JTC-SYNTAX` and stop the method check.

### #3b — Inferencer gap: parametric Union isa-narrowing (1 fire)

**Pattern**: `error[JTC-ISA]: rhs Union{LinearMapAM, LinearMapAO} is not
a valid splitted target`.

**Example** (LinearMapsAA):
```julia
if A isa Union{LinearMapAM, LinearMapAO}
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ← parametric union RHS rejected
```

Our `isa`-narrowing splits `Union{T, S}` cleanly for concrete `T`, `S`,
but rejects unions over parametric types with unbound parameters.

**Fix**: medium. Extend the union-split machinery; related to seeding
because concrete seeds collapse the union's parameter wildcards.

### #4 — Crashes

#### #4a — Parser: unicode byte-index mismatch (LowRankArithmetic)

`StringIndexError` from inside `addFile!` after 8 successful
diagnostics. Triggered by docstrings/identifiers using unicode math
symbols (`ℝⁿˣʳ`). Some code path uses byte offsets that don't fall on
UTF-8 codepoint boundaries.

**Fix**: small. Find the offending `String[i:j]` or `nextind` and use
codepoint-safe operations. Reproduce with a fixture: triple-quoted
docstring containing `ℝ`.

#### #4b — Inference: `Type{T}` constructor `::MethodHandle` typeassert (FastRationals)

`TypeError(:typeassert, "", DataType, Tuple{typeof(basetype), Type{FastRational{T}}} where T)`
after 24 successful diagnostics. Triggered by constructor methods like
`basetype(::Type{FastRational{T}})`. `Core.Compiler.specialize_method`
returns the unspecialized sig (a Tuple type) rather than a
`MethodInstance` when the spec_types include unbound `Type{T}`. The
`::MethodHandle` typeassert in `inferReturnType` then fails.

**Fix**: small. Replace the typeassert with a guarded check — if the
return isn't a `MethodInstance`, skip the match.

#### #4c — Harness: inference error re-thrown instead of collected (KroneckerProducts)

`ErrorException("iterate fails to match at ... over OrdinalRange{T,S} where {T,S}")`
after 2 diagnostics. The inferencer ran into an unresolvable `iterate`
dispatch and threw rather than collecting an `InferenceErrorMethod`
into `hasChecked`.

**Fix**: small. Wrap the per-MI loop in `runCheck!` with a catch that
converts thrown inference errors into diagnostics and continues with
the next queued MI.

### #5 — `checkPackage` design gap: single-file packages

PSDMatrices: 0 methods checked, 0 errors, 0 noise — `checkPackage`
walks `src/` and skips the file whose basename matches the module name
(e.g. `PSDMatrices.jl`) to avoid the `module Foo ... end` wrapper
collision. For multi-file packages that's fine; for single-file
packages it leaves `methodDefs` empty.

**Fix**: special-case single-file packages (add the entry file anyway,
or strip the outer `module Foo ... end` wrapper before parsing).

---

## Fix priority

Ranked by leverage (errors-removed / effort), excluding the shape-only
seeding row since that vanishes by switching to test-driven concrete
MIs:

| # | Fix | Effort | Eliminates |
|---|---|---|---|
| #2  | Parser `; kwargs...` slurp whitelist | small | 24 fires (~11 %) |
| #4a | Parser unicode byte-index | small | one crash, opens LowRankArithmetic |
| #4b | Inference typeassert guard for `Type{T}` constructors | small | one crash, opens FastRationals |
| #4c | Per-MI exception handling in `runCheck!` | small | one crash, opens KroneckerProducts |
| #5  | Single-file `checkPackage` path | small | opens PSDMatrices |
| #3a | Default-expr T-substitution | small | 1 fire, principle matters |
| #3  | Splat `...` AST | medium | 3 fires + every real package |
| #3b | Parametric Union isa-narrowing | medium | 1 fire (would compound with #3) |
| #1  | Opt-out for intentional abstract dispatch | medium | ~100 fires (~47 %), needs API design |
