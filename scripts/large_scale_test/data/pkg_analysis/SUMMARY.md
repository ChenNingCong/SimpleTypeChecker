# Stage-5 concrete-MI runs — consolidated summary

This is the rolling summary from running `checkPackage` against 23
packages using the concrete-MI flow (`Pkg.test` → `Base.specializations`
harvest → `runCheck!`, with `permitRequiredKwargs` and
`permitCustomGetproperty` flags on). Each row links to that package's
raw diagnostic dump.

Updated after every fix-and-rerun cycle; per-package iteration files
(e.g. `pf3.diag.txt`) get cleaned up periodically.

## Roster (23 packages)

| Package | Tests | MIs | Methods | Errors | Top class | Notes |
|---|---|---:|---:|---:|---|---|
| [ArgCheck](ArgCheck.diag.txt) | ok | 163 | 68 | 54 | SyntaxError(21) | `T::Type{<:Exception}` — see #3c (unary `<:`) |
| [BitFlags](BitFlags.diag.txt) | failed | 34 | 26 | 15 | FunCallArgs(6) | `function f end` forward decls |
| [ColorTypes](ColorTypes.diag.txt) | — | — | — | — | — | run hung previously; needs re-test |
| [CommonSolve](CommonSolve.diag.txt) | ok | 3 | 0 | 0 | — | empty harvest — methods all forward decls |
| [Compat](Compat.diag.txt) | failed | 8 | 5 | 3 | mixed | small surface |
| [CompositionsBase](CompositionsBase.diag.txt) | ok | 54 | 13 | 13 | SyntaxError(12) | `function ∘ end`-style forward decls |
| **[Crayons](Crayons.diag.txt)** | ok | 99 | 69 | 36 | FunCallArgs(12) | **real type-stability bug found — see `docs/package_bugs.md`** |
| [DataStructures](DataStructures.diag.txt) | failed | 70 | 86 | 19 | FunCallArgs(6) | GC.@preserve unreachable-fallthrough FP |
| [FastRationals](FastRationals.diag.txt) | ok | 63 | 35 | 5 | FailedToDestruct(3) | clean run, no crashes after 4c |
| [FixedPointNumbers](FixedPointNumbers.diag.txt) | — | — | — | — | — | hangs at `using` — skip |
| [Glob](Glob.diag.txt) | failed | 43 | 23 | 11 | FunCallArgs(4) | InterConditional residue; `findnext` Union returns |
| [IfElse](IfElse.diag.txt) | ok | 0 | 0 | 0 | — | trivial package, no MIs reached us |
| [KroneckerProducts](KroneckerProducts.diag.txt) | failed | 494 | 149 | 100 | SyntaxError(83) | dominated by vararg / parametric splice patterns |
| [LinearMapsAA](LinearMapsAA.diag.txt) | ok | 815 | 167 | 73 | FunCallArgs(29) | many MismatchedSplit (Union narrowing) |
| [LowRankArithmetic](LowRankArithmetic.diag.txt) | ok | 71 | 49 | 17 | FailedToDestruct(9) | SVD/QR-result destructuring fires |
| [PDMatsSingular](PDMatsSingular.diag.txt) | ok | 15 | 11 | 2 | NoField(1) | PDiagMat virtual property fired pre-fix |
| [PenaltyFunctions](PenaltyFunctions.diag.txt) | ok | 349 | 21 | 7 | SyntaxError(5) | down from 78 + crashes pre-fix |
| [ProgressMeter](ProgressMeter.diag.txt) | timeout | 72 | 59 | 53 | FunCallArgs(13) | tests hang; harvested-from-partial-run |
| [PSDMatrices](PSDMatrices.diag.txt) | ok | 37 | 34 | 9 | NoField(6) | 0→34 after single-file `checkPackage` path |
| [Reexport](Reexport.diag.txt) | failed | 3 | 3 | 1 | TopLevelDispatch(1) | single fire; generator-scope fix recovered |
| [SnoopPrecompile](SnoopPrecompile.diag.txt) | failed | 0 | 0 | 0 | — | **compiler-internal package, skip** |
| [StableTasks](StableTasks.diag.txt) | ok | 2 | 1 | 0 | — | clean! |
| [Tables](Tables.diag.txt) | — | — | — | — | — | needs re-run |

## Key findings

### Real type-stability bugs found in user code

**1** — see `docs/package_bugs.md`. **Crayons** `_torgb` body produces
`Tuple{UInt32,UInt32,UInt32}` for a function declared
`::NTuple{3, UInt8}`. The auto-`convert` at the return annotation
masks it.

### Recurring checker-side patterns post-fix

| Pattern | Where | Status |
|---|---|---|
| `function f end` forward declarations | DataStructures, BitFlags, CompositionsBase | open — needs parser |
| Tuple destructuring `f((a, b)::Tuple)` in arg position | DataStructures, Glob | open — needs parser |
| `GC.@preserve x begin return … end` reads as fallthrough | DataStructures `_slotget`, `_slotset!` | FP from macro-body unreachable analysis |
| `findnext`-style `Union{Int, Nothing}` returns | Glob:101 | needs declared-`Union` acceptance, not a real bug |
| Unary `<:T` shorthand inside `Type{<:T}` | ArgCheck (×17) | open — see #3c in `checker_limitations.md` |
| InterConditional propagation through non-makeType paths | DataStructures `&(::InterConditional)` | partial fix in `<latest>` |
| AbstractArray-class abstract dispatch (the perennial ~40% share) | every numerical package | needs `@nocheck` / `@anydispatch` API |

### Skip-list categories

- **Compiler-internal packages** (per user): SnoopPrecompile, Cassette-like — these probe `Core.Compiler.*` and produce noise. Skip at corpus-build time, not at run time.
- **Hangs during `using`**: FixedPointNumbers consistently exceeds the 40 s `using` budget. Likely heavy precompile chain in our shared env. Skip for now.
- **0-MI harvests**: CommonSolve, IfElse, StableTasks — tests don't exercise any non-stdlib method paths. Not a bug, just nothing for the checker to chew on.

## Per-package data files

All raw checker diagnostic dumps are in this directory as
`{PackageName}.diag.txt`. Each one ends with a `[STATS]` block listing
`test_outcome`, `mis_harvested`, `methods_checked`, `errors`,
`elapsed_sec`, and per-class fire counts.

## How to re-run

```bash
echo -e "Pkg1\nPkg2" | /tmp/run_pkg_batch.sh 5
```

Parallel = 5 max per user's instruction; each per-package wall-clock
budget = 90 s with SIGINT-then-SIGKILL escalation.
