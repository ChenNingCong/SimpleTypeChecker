# Real bugs found in user packages

This file tracks **real type bugs in package source code** that the
checker found during stage-5 corpus runs — distinct from checker
limitations / false positives, which live in `checker_limitations.md`.

A bug counts as "real" if the package's own code would fail at runtime
under the inputs it accepts, OR if it has a non-typecheckable usage
pattern that the maintainer would want to fix.

## LowRankArithmetic v0.1.4 — accumulator type changes mid-loop

**File**: `src/utils.jl:34-43` (`truncate_to_tolerance`)

```julia
function truncate_to_tolerance(S, tol)
    s = 0                    # ← s starts as Int64 (literal `0`)
    r = length(S)
    for σ in reverse(S)
        s += σ^2             # ← σ^2 is Float64 (typical SVD singular values)
        if s > tol^2          #   so s becomes Float64 after iter 1
            break
        end
        r -= 1
    end
    return r
end
```

**Diagnostic** the checker correctly produced:
```
error[JTC-ASSIGN]: reassigned type Float64 is incompatible
   --> .../LowRankArithmetic/.../src/utils.jl:37:9
 37 |         s += σ^2
    |         ^
Previous type is Int64, ...:34:5
```

**Why it's a real bug**:

The textbook Julia type-instability pattern. The variable `s` is
initialised with the Int literal `0`, then accumulated with `σ^2`
which is `Float64` for typical inputs (singular values from
`svd(A).S`). After the first iteration `s` is `Float64` — meaning
`s`'s type across the loop is `Union{Int64, Float64}`.

Consequences:
- The compiler can't specialise the `s += σ^2` arithmetic optimally
  on the first iteration (must dispatch on Union).
- `s > tol^2` comparison has to handle both Int and Float64 cases.
- Inlining and SIMD opportunities lost.

This is exactly the case `code_warntype` flags with red ink, and
exactly the case `JTC-ASSIGN: reassigned type X is incompatible` is
designed to catch.

**Suggested fix** — match the accumulator's initial type to the
elements:

```julia
function truncate_to_tolerance(S, tol)
    s = zero(eltype(S))     # respects element type, works for any Number
    r = length(S)
    for σ in reverse(S)
        s += σ^2
        s > tol^2 && break
        r -= 1
    end
    return r
end
```

Or, if S is known to be Float64-typed: `s = 0.0`.

**Verdict**: real, clean, single-line fix. Should be a one-PR
submission to LowRankArithmetic.

---

## Crayons v4.x — `_torgb` body type doesn't match declared return

**File**: `src/crayon.jl:135`

```julia
function _torgb(hex::UInt32)::NTuple{3, UInt8}
    (hex << 8 >> 24, hex << 16 >> 24, hex << 24 >> 24)
end
```

**Diagnostic** (the checker correctly identifies this):
```
error[JTC-RETURN]: return type is enlarged
   --> .../Crayons/.../src/crayon.jl:135:47
Previous type is Tuple{UInt8, UInt8, UInt8}, ...:135:1
Current type is Tuple{UInt32, UInt32, UInt32}, ...:135:47
```

**Why it's a real bug**:

`hex` is `UInt32`. Each subexpression `hex << N >> 24` follows
`UInt32 << Int → UInt32`, `UInt32 >> Int → UInt32`, so the tuple
naturally has type `Tuple{UInt32, UInt32, UInt32}` — three 32-bit
values, NOT three 8-bit values as declared.

The function works at runtime only because Julia's return-type
annotation injects `convert(NTuple{3, UInt8}, …)` at the return point.
The truncation happens to preserve all data (each `>> 24` leaves at
most 8 significant bits), so the silent convert is information-lossless.

But: **the body is not type-stable** with respect to the declared
signature. Two consequences:

1. Removing the `::NTuple{3, UInt8}` annotation would suddenly change
   callers' inferred types from `UInt8` to `UInt32` — a silent
   API-breaking change waiting to happen.
2. The body promises `UInt8` semantics (look at `_torgb`'s name) but
   produces `UInt32` operands. If anyone copies these expressions into
   another context — e.g. into a SIMD builder or memory layout
   computation — they'd get the wrong width without knowing.

**Suggested fix** — make the truncation explicit, then the body's
natural type matches the declared signature:

```julia
function _torgb(hex::UInt32)::NTuple{3, UInt8}
    ((hex >> 16) % UInt8, (hex >> 8) % UInt8, hex % UInt8)
end
```

or equivalently:

```julia
function _torgb(hex::UInt32)::NTuple{3, UInt8}
    (UInt8(hex << 8 >> 24), UInt8(hex << 16 >> 24), UInt8(hex << 24 >> 24))
end
```

(The `% UInt8` form is the idiomatic truncation and is what Julia's
`convert(UInt8, ::UInt32)` does internally when the value fits.)

**Verdict**: real latent type-stability bug, surfaced exactly the way
the checker is designed to surface them — "the body's inferred type
disagrees with the declared signature". The implicit `convert` at the
annotation is masking it. Worth a PR to Crayons.

---

