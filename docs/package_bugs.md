# Real bugs found in user packages

Real type bugs and latent issues the checker surfaced during stage-5
runs. Distinct from checker limitations / false positives (those live
in `checker_limitations.md`).

These are NOT every fire — they're the ones I read carefully and
confirmed against the source.

---

## 1. LowRankArithmetic v0.1.4 — `utils.jl:37` — Int → Float64 accumulator

**Severity**: Real type-instability bug. One-line fix.

```julia
function truncate_to_tolerance(S, tol)
    s = 0                # ← Int64 literal
    r = length(S)
    for σ in reverse(S)
        s += σ^2         # ← Float64 (σ from svd().S)
        if s > tol^2      #    s is now Union{Int64, Float64}
            break
        end
        r -= 1
    end
    return r
end
```

**Diagnostic**:
```
error[JTC-ASSIGN]: reassigned type Float64 is incompatible
   --> .../utils.jl:37:9
Previous type is Int64, .../utils.jl:34:5
```

**Fix**: `s = zero(eltype(S))` (or `s = 0.0` if S is known Float64-typed).

---

## 2. ProgressMeter v1.x — `ProgressMeter.jl:963` — variable may be unbound

**Severity**: Real correctness bug. Runtime `UndefVarError` on any
unexpected expression head.

```julia
if expr.head == :for
    outerassignidx = 1
    loopbodyidx = lastindex(expr.args)
elseif expr.head == :comprehension
    outerassignidx = lastindex(expr.args)
    loopbodyidx = 1
elseif expr.head == :typed_comprehension
    outerassignidx = lastindex(expr.args)
    loopbodyidx = 2
end
# ← no else / no @assert
if expr.head !== :for
    @assert length(expr.args) == loopbodyidx
    expr = expr.args[outerassignidx] = copy(expr.args[outerassignidx])
    #                ^^^^^^^^^^^^^^   ← read here without prior assignment
```

**Diagnostic**:
```
error[JTC-UNDEFINED]: variable outerassignidx is potentially unitialized here
   --> .../ProgressMeter.jl:963:59
```

**Why real**: the if-elseif chain has no `else`, so if `expr.head` is
anything other than the three listed, both `outerassignidx` and
`loopbodyidx` are unbound. The code goes on to access them
unconditionally. Runtime would throw `UndefVarError`.

This is `@progress`'s expansion-time error path — if a user writes
`@progress while …` instead of `@progress for …`, they get a confusing
UndefVarError instead of a useful message.

**Fix**: add an `else` branch that throws `ArgumentError("@progress
requires :for, :comprehension, or :typed_comprehension")`, or guard the
subsequent block with `expr.head in (:for, :comprehension,
:typed_comprehension) || error(...)`.

---

## 3. Crayons v4.x — `crayon.jl:135` — `_torgb` body and declared return disagree

**Severity**: Latent type instability, masked by implicit `convert`
at return annotation. Fragile but currently working.

```julia
function _torgb(hex::UInt32)::NTuple{3, UInt8}
    (hex << 8 >> 24, hex << 16 >> 24, hex << 24 >> 24)
end
```

The bit-shift expressions are `UInt32 → UInt32`, so the body produces
`Tuple{UInt32, UInt32, UInt32}`. The `::NTuple{3, UInt8}` annotation
triggers `convert(NTuple{3, UInt8}, …)` which truncates each value to
8 bits.

**Diagnostic**:
```
error[JTC-RETURN]: return type is enlarged
   --> .../Crayons/u3AH8/src/crayon.jl:135:47
Previous type is Tuple{UInt8, UInt8, UInt8}, ...:135:1
Current type is Tuple{UInt32, UInt32, UInt32}, ...:135:47
```

**Knock-on effect at `crayon.jl:142`** — `r, g, b = _torgb(c)` — the
destructure picks up `UInt32` for each variable (from the body), but
elsewhere in the same scope these variables are typed as `Int64` /
`UInt8` from other code paths. The checker fires 5 specialisations of
"reassigned type Int64 is incompatible" at that destructure — all
downstream of this root cause.

**Fix**: make the truncation explicit so the body's natural type
matches the annotation:

```julia
function _torgb(hex::UInt32)::NTuple{3, UInt8}
    ((hex >> 16) % UInt8, (hex >> 8) % UInt8, hex % UInt8)
end
```

---

## 5. Crayons v4.x — `downcasts.jl:85` — `value` reassignment likely changes type

**Severity**: Probably real (need to verify `compute_value`'s return
type).

```julia
function to_system_colors(color::ANSIColor)
    @assert color.style == COLORS_24BIT
    r, g, b = color.r, color.g, color.b
    value = compute_value(r, g, b)        # ← inferred type X
    value = round(Int, value / 50)        # ← reassigned to Int
    ...
```

**Diagnostic**:
```
error[JTC-ASSIGN]: reassigned type Int64 is incompatible
   --> .../Crayons/u3AH8/src/downcasts.jl:85:13
```

The checker reports the second `value = round(Int, …)` reassigns the
variable with `Int` while the previous `value = compute_value(...)`
inferred to something else. Without reading `compute_value`, I'd guess
it returns `Float` (since `value / 50` only makes sense for non-Int).
If so, `value` walks `Float → Int` — type instability.

**Suggested fix** (subject to verifying compute_value): use a new
variable name for the rounded result:

```julia
v_idx = round(Int, value / 50)
```

---

## Probably NOT real (false-positive triage)

- **Crayons `crayon_stack.jl:73`** — `pc = Crayon(...)` says "Previous
  type Bool" but the source has `pc = cs.crayons[end]` which is
  `Crayon`. Doesn't match the source — checker bug.
- **Crayons `downcasts.jl:4`, `:27`, `:39`** — "Previous type
  Array{Any, 1}" doesn't match the visible source. Checker bug.
- **Crayons `crayon.jl:183` destruct length-mismatch** — multi-return
  from `_torgb` propagation, likely false positive on the same
  `_torgb` root cause.
- **DataStructures `_slotget` / `_slotset!`** — `return` is inside
  `GC.@preserve x begin … end` block. Checker treats the block end
  as fallthrough returning `Nothing`. Checker bug (we don't model
  `GC.@preserve` body as always-completing).
- **ColorTypes `traits.jl:251` (×26)** — parametric function
  constructing different parametric Type results. Legitimate
  polymorphism, not a bug.
- **Glob `findnext`-style returns** — `findnext` API contract is
  `Union{Int, Nothing}`. Not a bug.

---

## Methodology note (for my own discipline next round)

For every JTC-ASSIGN with "reassigned", actually READ both the
"Previous type X" line and the source. The previous-type often
identifies the wrong line if my fixes haven't all landed, in which
case it's a checker artifact. When the previous-type *does* match the
source, that's a real type-instability fire worth surfacing.

For every JTC-UNDEFINED, the source pattern is almost always a
guard-by-invariant that the author forgot to make explicit — these
are very high-signal real bugs.

For every JTC-RETURN "type is enlarged", check whether the function is
parametric or has multiple return paths returning different types.
Parametric polymorphism is fine; mixed-return is the bug.
