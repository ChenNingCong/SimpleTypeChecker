# Sparse Backward Value Flow — interactive demo

A working demonstration of the demand-driven type-checking algorithm
described in `../ts_checker_algorithm.md`, implemented for a tiny
imperative language. Browse a step-by-step trace of how the analyzer
proves a return type, with branches and while loops handled correctly
and with each step annotated by the rule that fires.

## Why this exists

The TypeScript paper-style description ("interprocedural sparse
backward value flow") is hard to internalize from prose alone. The
crucial bit — *why* it is sparse, and how branches/loops compose with
sparse traversal — only becomes obvious once you can watch a specific
query land on a specific subset of the AST and skip the rest.

The demo shows three things visually:

- **Sparseness.** For each query, the UI lists every AST node and
  marks which ones the query actually touched. Most of them are
  inert. Branches whose path the query doesn't depend on are skipped
  entirely.
- **Backwardness.** The step-by-step trace shows the query starting at
  the use site and recursing toward defs — not in source-code order
  but along def-use edges.
- **Branches and while loops.** PHI-JOIN handles branch merges by
  taking the union of incoming defs. While loops form cycles in the
  def-use graph; the analyzer breaks them with a Bottom (⊥)
  placeholder, then unions with the placeholder yielding the same
  result as a fixpoint iteration would.

## Running

```
cd docs/sparse_flow_demo
pip install flask
python app.py
# open http://127.0.0.1:5000
```

No build step. Single Python file. Plain HTML + a few lines of vanilla
JS. The intent is that you can read every line.

## The language

```
fn main() {
  let x = 1
  let y = "hi"
  if x > 0 {
    y = 42
  } else {
    y = "world"
  }
  return y               // Union[Int, Str]
}
```

- Types: `Int`, `Str`, `Bool`, `Any`, `⊥`, and `Union[...]`.
- Statements: `let`, assignment, `if/else`, `while`, `return`, call.
- Expressions: literals, variables, binops (`+ - * /`,
  `< > <= >= == !=`), function calls, type predicates `e is Int|Str|Bool`.

## Algorithm at a glance

The analyzer runs in two phases.

**Phase 1 — Build reaching defs (forward, dense).**
A single AST walk computes, for every `Var` use, the set of `Let`/
`Assign` nodes whose value may flow in. At branch merges the sets are
unioned. At loop heads the union iterates until stable. This is
classical reaching-definitions analysis — the *only* eager forward
pass in the demo, and it produces nothing more than def-use edges.

**Phase 2 — Type query (backward, sparse).**
Starting from a target node (the `return` expression of `main`), the
analyzer recursively computes types. Every node carries a cache slot;
re-querying is free. Rules:

| Rule | When it applies | What it does |
|------|------|------|
| `LITERAL-{INT,STR,BOOL}` | At a literal | Maps token to its primitive type |
| `VAR-SINGLE-DEF` | At a `Var` with exactly one reaching def | Recurses into the def |
| `PHI-JOIN` | At a `Var` with multiple reaching defs | Recurses into each, unions the results |
| `BINOP-{op}` | At a binary expression | Recurses into operands, applies the operator's type signature |
| `ASSIGN-TYPE` | At a `Let`/`Assign` | The node's type is its RHS's type |
| `PARAM` | At a parameter def | Returns `Any` (no parameter inference in this demo) |
| `CALL` | At a function call | Queries the callee's signature (memoized) |
| `SIG-COMPUTE` | First time a callee's signature is needed | Scans the callee body for `return` statements, unions their types |
| `CACHE-HIT` | Re-querying the same node | Returns the stored result instantly |
| `CYCLE-BREAK` | Re-entering a node still being computed | Returns `⊥`; the outer `union` widens past it |
| `NARROW-INPUT` | A Var sits inside the arm of `if v is T` | Marks the predicate as visited — the query now depends on the condition |
| `NARROW-THEN` / `NARROW-ELSE` | After computing the base type | Refines via `base ∩ T` (then-arm) or `base − T` (else-arm) |

## How sparseness shows up

When you run example `02_branch`, observe the visited-node tally in
the right pane. The query `return y` walks back to the two
assignments to `y` inside the `if`/`else` arms. It does **not** visit
`let x = 1`, `let y = "hi"`, or even the `if` statement itself —
those nodes are not on a def-use chain leading to the queried `y`.

This is the property we want to inherit. Forward abstract
interpretation would touch every one of them at least once.

## How branches show up

Example `02_branch`. The reaching-defs phase records that `y`'s value
at the `return` point is one of: the `y = 42` assignment OR the
`y = "world"` assignment. The query then queries each, getting `Int`
and `Str`, and unions them.

Note what is *not* needed: the condition `x > 0` is irrelevant to the
query because we're not narrowing on it. Type checking under
union-typed assumptions is straightforward; *narrowing* (refining
`Union[Int, Str]` to `Int` when the condition is true) is a separate
layer not implemented here.

## How while loops show up

Example `03_while`. The reaching-defs phase iterates the env merge at
the loop head until stable. Once stable, the use `return x` has
reaching defs = the initial `let x = 0` AND the in-loop `x = x + 1`
assignment. Query:

```
get_type(Var x):
  PHI-JOIN on 2 defs
  ├ get_type(Let x = 0)
  │   ASSIGN-TYPE → get_type(IntLit 0) → Int
  └ get_type(Assign x = x + 1)
      ASSIGN-TYPE → get_type(Binop +)
        ├ get_type(Var x):                          ← cycle entered here
        │   PHI-JOIN on 2 defs
        │   ├ get_type(Let x = 0)        → CACHE-HIT → Int
        │   └ get_type(Assign x = x + 1) → CYCLE-BREAK → ⊥
        │   union(Int, ⊥) = Int                     ← cycle widens correctly
        └ get_type(IntLit 1) → Int
      Int + Int → Int
  union(Int, Int) = Int
```

This is the equivalent of a fixpoint iteration that converges in one
pass because `⊥` is the identity for `union`. The same structure
handles more interesting loops where the body widens the type:

- `while ... { x = "hi" }` — the second def is `Str`, so the union
  becomes `Union[Int, Str]`.
- `while ... { x = "hi"; x = x + 1 }` — the third def's RHS computes
  `Str + Int → Any`, and the final union is `Union[Int, Str, Any] =
  Any`.

## How type narrowing shows up

Examples `41_narrow_then`, `42_narrow_else`, `43_narrow_flow_join` are the
warm-up cases. The substantial showcases are:

- `44_narrow_both_arms` — narrowing fires in *both* arms; downstream
  `result` ends up as `Union[Int, Str]` instead of `Any`.
- `45_narrow_in_callee` — narrowing happens inside a callee body, and
  `SIG-COMPUTE` aggregates the narrowed returns into the callee's
  signature.
- `46_narrow_after_loop` — a `while` loop builds `Union[Int, Str]` over
  multiple iterations; a post-loop `if x is Int` collapses it back to
  `Int`.
- `47_narrow_reassign_guard` — same `v` appears twice in the narrowed
  arm; the second use comes *after* a reassignment, so the
  snapshot-equality guard correctly drops the tag. The trace shows
  narrowing on the first use and **no** narrowing on the second.
- `48_narrow_two_vars` — nested ifs narrow two different variables;
  the inner narrowing decides one return path while the outer
  narrowing decides another.


Adding `e is T` as a value form, and recognizing it as an `if`
condition, lets us refine a union along each branch:

```
fn main() {
  let v = box()            // box returns Union[Int, Str]
  if v is Int {
    return v + 100         // here v is Int; v + 100 is Int
  } else {
    return 0
  }
}
```

Without narrowing, `v + 100` would infer `Any` (since `Int + Int = Int`
but `Str + Int = Any`), and the final return type would be `Any`. With
narrowing the then-arm types `v` as `Int`, so `v + 100` is `Int`, and
the program's return type is `Int`.

What is interesting about this from a sparseness perspective is that
**the query now depends on the condition.** Every other rule rides
def-use edges only; the condition `x > 0` of an existing if-statement
is invisible to the type query. The `is` predicate is the first thing
in the demo that breaks that pattern. The trace makes this explicit:
when a narrowed Var is queried, the analyzer emits a `NARROW-INPUT`
step that highlights the predicate and adds it to the visited set,
followed by a `NARROW-THEN` / `NARROW-ELSE` step that does the actual
refinement.

The implementation has two halves:

- **Phase 1 (forward).** When an `if` cond is exactly `Var is T`, we
  snapshot the variable's reaching defs at branch entry, then walk
  each arm and tag every matching `Var` use with `(mode, T, pred)` —
  but *only* if the use's reaching defs still equal the snapshot.
  That equality check is what suppresses narrowing inside arms that
  re-assign the variable before the use.
- **Phase 2 (backward).** `_compute_var` first computes the base
  type the normal way (`VAR-SINGLE-DEF` / `PHI-JOIN`), then applies
  the tag: `base ∩ T` for the then-arm, `base − T` for the else-arm.

Limits of the simple version: only `Var is Type` triggers narrowing;
chained conditions (`v is Int && ...`), narrowing into nested ifs,
and narrowing through aliases are out of scope. The point is to show
the *shape* of how a condition becomes a sparseness input, not to
ship a TypeScript-grade flow analyzer.

## How function calls show up

Example `04_function`. The query `return x` in `main` walks back to
`let x = add(1, 2)`, then through `ASSIGN-TYPE` and `CALL` to
`add`'s signature. First time a signature is requested, the analyzer
runs a one-shot computation over the callee's body (scanning for
return statements, unioning their types) and caches the result on the
function symbol. Subsequent calls to `add` hit the cache. This is the
same memoization shape TypeScript uses — keyed on the function symbol,
not on the call site.

## What this demo deliberately omits

- **Rich control-flow narrowing.** We have the simplest form
  (`if v is Int`). Real checkers narrow on richer predicates
  (`typeof`, `instanceof`, equality with a literal, discriminant
  checks on tagged unions, conjunctions/disjunctions), and they
  thread narrowings across aliasing and rebinding. The demo's
  narrowing is the minimum viable shape — enough to make the
  condition show up as an input to the sparse query.
- **Generic / parameter type inference.** Parameters are `Any`. A
  real implementation infers them from the caller's argument types
  (contextual typing).
- **Incremental rechecking.** Cache invalidation on source edits
  would require a dependency graph recorded during the query
  traversal.
- **Real error reporting.** The demo only computes types; it doesn't
  emit a single diagnostic. This keeps the focus on the value-flow
  engine that diagnostics would ultimately sit on top of.

## Mapping to SimpleTypeChecker

See `../ts_checker_algorithm.md`, "What this implies for
SimpleTypeChecker" — same table of equivalences. The thing we're
missing in production today is the analogue of `inferReturnTypeFromBody`
that walks our own body rather than delegating to Core.Compiler. The
demo's `get_function_return_type` is the toy version of exactly that.
