# Error code reference

Every diagnostic the checker emits has a stable `JTC-…` code in the
header. The code is the part you should key tooling / baseline files
on; the headline text is informative and may drift across versions.

Diagnostics fall into three buckets:

1. **Real bugs** — code that won't behave as written (`JTC-UNDEFINED`,
   most `JTC-RETURN`-enlarged cases).
2. **Type-instability candidates** — likely real, sometimes intended
   (`JTC-ASSIGN`, `JTC-FIELD`, `JTC-DISPATCH`).
3. **Checker limitations** — patterns the checker can't model
   (`JTC-UNSUPPORTED`, `JTC-SYNTAX`, `JTC-INTERNAL`).

The headlines below use `$x` placeholders for runtime-substituted
content (a type name, a variable name, an argument index).

## Real-bug codes

These almost always point to a real problem in the analyzed code.

### `JTC-UNDEFINED` — variable potentially unbound

```
UndefinedError: variable $id is potentially unitialized here
UndefinedError: variable $id is conditionally defined here
UndefinedError: keyword $sym is required but not inputed
```

The variable is read on a path where it might not have been assigned.
Most common shape is an `if`/`elseif` chain with no `else`:

```julia
if cond1
    x = 1
elseif cond2
    x = 2
end
# x read here — UndefinedError if neither branch fired
println(x)
```

Real-world example: `ProgressMeter.jl:963` where `outerassignidx` is
assigned only under three specific `expr.head` values and then read
unconditionally — see [`package_bugs.md`](package_bugs.md).

**Fix**: add an `else` (with an explicit `error("…")` if the missing
case shouldn't be reachable), or initialize the variable before the
branch.

### `JTC-UNDEFVAR` — global variable not defined

```
UndefVarError: In module $mod, variable $p is not defined
```

A reference resolves to a global that doesn't exist in the module's
namespace. Usually a typo or missing import.

### `JTC-RETURN` — return type doesn't match annotation or earlier path

```
ReturnError: return type is enlarged
```

A function with a declared `::T` return (or an earlier return path
that established a type) returns a different type on this path. The
checker prevents the implicit `convert` that Julia would otherwise
silently apply. Most common shape: a body that nearly-but-not-quite
matches the annotation.

Real-world example: `Crayons._torgb` declared `::NTuple{3, UInt8}`
but the body returns `Tuple{UInt32, UInt32, UInt32}`. Runtime
`convert` truncates each value to 8 bits — works, but the body's
natural type is wrong and downstream destructuring picks up `UInt32`
instead of `UInt8`. See [`package_bugs.md`](package_bugs.md).

**Fix**: make the truncation explicit (e.g. `% UInt8`) so the body's
natural type matches the annotation.

## Type-instability codes

These point at type instability — sometimes the bug, sometimes
intentional polymorphism.

### `JTC-ASSIGN` — variable reassigned with incompatible type

```
AssignError: reassigned type $T is incompatible
AssignError: rhs is a bottom value
AssignError: update assign evaluates to a bottom value
AssignError: initializer's type $T1 is incompatible with asserted type of $T2
AssignError: Failed to destruct rhs to lhs : $msg
```

The checker tracks each local variable's type within a method. If a
later assignment narrows or widens incompatibly, this fires.

Two shapes are common:

```julia
# Shape 1: accumulator drift (real bug)
s = 0                  # Int
for x in xs
    s += x^2           # x is Float64 — s becomes Union{Int, Float64}
end

# Shape 2: stage-then-rewrite (often the bug, sometimes the API)
value = compute_value(r, g, b)   # Float64
value = round(Int, value / 50)   # Int — reassignment changes type
```

Real-world example: `LowRankArithmetic/utils.jl:37` (`s = 0` →
`s += σ^2`). Trivial fix: `s = zero(eltype(S))`.

**Fix**: use a new variable name for the rewritten value, or
initialize with the right type via `zero(eltype(…))` /
`oneunit(T)`.

### `JTC-FIELD` — getproperty / setproperty! issue

```
FieldError: calling $accesstype on non-concrete type $T
FieldError: type $T has no field $p
FieldError: assigning value of type $V to $T 's field $p, which has type $FT
```

The type whose field is being accessed is not concrete (so the
checker can't tell which field type to expect), or the field doesn't
exist, or the assigned value's type doesn't match the field's
declared type.

For parametric types with virtual properties (e.g. PDMats's
`PDiagMat.dim` is computed, not stored), set
`ctx.permitCustomGetproperty = true` — see [`usage.md`](usage.md).

### `JTC-DISPATCH` — call has an argument of abstract type

```
(method dispatch trace)
```

A call site can't resolve to a single concrete method because at
least one argument has an abstract static type. This is the most
common false-positive class on idiomatic Julia code that uses
abstract supertypes deliberately (`AbstractArray`, `AbstractDict`,
`Number`, …). For those cases, mark the method with `@nocheck` or
use the `@nospecialize`-aware escape hatch.

### `JTC-DEEP-DISPATCH` — transitive dispatch site (only when `deepDispatchCheck` is on)

The callee, or a callee of the callee, contains a dynamic dispatch
site. Used to flag dispatch chains that escape the analyzed surface.
Off by default.

### `JTC-METHOD` — method-resolution failure

```
MethodError: no matching $m for $(sig)
MethodError: more than one matching $m for $(sig)
```

The call doesn't dispatch to exactly one method. "No matching" is
usually a real bug (wrong argument types). "More than one matching"
is ambiguity that the runtime would resolve via dispatch order but
the checker refuses to.

### `JTC-CONSTRUCTOR` / `JTC-BROADCAST` — call returns `Union{}`

```
ConstructorError: constructor $str returns Union{}
BroadcastError: broadcast $str returns Union{}
```

The call site's inferred return is `Union{}` — i.e. it would throw at
runtime according to Julia's own inferencer. Real bug.

### `JTC-TYPE` — type-application or assertion problem

```
TypeError: $x-th paramter of type application is not a constant
TypeError: fail to construct type $str
TypeError: non-boolean type $T used as condition
TypeError: rhs of isa is not a constant
TypeError: the typed used for type assertion is not a constant
```

The thing in a type-application slot (`Vector{x}` where `x` is a
local variable, not a `const`) or assertion (`y isa f(x)`) isn't a
constant. Julia would accept these at runtime via reflection; the
checker requires constant types so it can reason statically.

### `JTC-ISA` — isa-narrowing problem

```
IsaError: can't split type $T to $RHS
IsaError: rhs $V is not a valid splitted target
```

The argument of `isa` isn't a type the checker can use for narrowing
the value's static type.

### `JTC-BOTTOM` — argument evaluated to `Union{}`

A subexpression has type `Union{}` (i.e. it definitely throws). The
expression containing it is unreachable.

```
BottomError: try to calling a bottom value
BottomError: $i-th parameter of function call is an bottom value
BottomError: try to index a bottom value
BottomError: try to intepolate a bottom into a string
BottomError: keyword argument $sym is assigned with bottom
…
```

### `JTC-IF` — branch return-type mismatch

```
IfError: if type is enlarged
```

The two branches of an `if`/`else` produce incompatible types and
the merged type is wider. The checker rejects this even when the
result would be valid — it's a deliberate design choice (see
[`README.md`'s scope-of-variable section](../README.md)). Use a
typed `local x::Union{T1, T2}` slot to make the union explicit.

### `JTC-UNREACHABLE` — code after a terminal expression

```
UnreachableError: unreachable code exists after this expression
```

There's a statement after `return` / `throw` / a call that returns
`Union{}` (like `error("…")`).

## Checker-limitation codes

These point at the checker, not your code.

### `JTC-UNSUPPORTED` — AST shape not implemented

The shape is valid Julia but the checker hasn't implemented it. The
emitter attaches a `help:` line explaining that this is a checker
limitation and suggesting either a rewrite or `@nocheck`.

Past examples that have since been implemented: splat `…` at call
sites, juxtapose `2x` / `A'B`, `@generated` in expression position.

### `JTC-SYNTAX` — checker refused the AST outright

Similar to `JTC-UNSUPPORTED` but stronger — the shape is rejected
rather than queued for "implement later". Look at the source span;
usually you can rewrite to a supported form.

### `JTC-MISC` — uncategorized

Catch-all for diagnostics whose headlines don't fit the
`SomeError: …` shape. If you see one of these from your run, it's
probably worth filing a bug to either categorize it properly or
re-word the headline.

### `JTC-INTERNAL` — checker bug

The checker itself threw inside `runCheck!`. The renderer attaches
a checker-internal stacktrace (filtered to checker frames). File a
bug. The per-MI exception handler will move on to the next
`MethodInstance` rather than crash the run — see
`runCheck!` in `src/inference/Inference.jl`.

## Suppression

| Tool | Scope | Use when |
|---|---|---|
| `@nocheck` macro | Whole method | The method legitimately uses dynamic features the checker can't model |
| `@nospecialize` on a slot | One arg of one method | The arg's abstract type is intentional and the body doesn't rely on the concrete type |
| `permitRequiredKwargs` flag | Whole context | Your codebase uses required keyword args (no default) |
| `permitCustomGetproperty` flag | Whole context | Your codebase uses virtual `getproperty` overrides |
| `checkSyntax = false` | Whole run | You only care about type errors, not parser-gap reports |

See [`usage.md`](usage.md#tuning-what-fires--globalcontext-flags) for the flags.

## Cross-references

- [`checker_limitations.md`](checker_limitations.md) — fix index + the
  full enumeration of known false-positive classes
- [`package_bugs.md`](package_bugs.md) — real bugs the checker found
  in published packages, with patterns
- [`nocheck_inventory.md`](nocheck_inventory.md) — patterns where
  `@nocheck` is the right tool
