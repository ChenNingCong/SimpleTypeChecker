# Supported syntax

SimpleTypeChecker walks Julia's AST directly (via JuliaSyntax) rather
than the lowered IR. This means it implements one branch per AST
shape — every supported form is one we've written a handler for.

When the checker sees an AST shape it can't model, it emits
`JTC-UNSUPPORTED` with a `help:` line explaining that this is a
checker limitation. Pass `runCheck!(ctx; checkSyntax = false)` to
silence those (you may still get downstream type errors from the
abandoned analysis).

## Definitely supported

| Construct | Example | Notes |
|---|---|---|
| Variable assignment | `x = 1+1` | Including `x::Int = 1` and update-ops `x += 1` |
| Tuple destructure | `(x, (y[4], z.l)) = (3, (4, 5))` | No update-ops on destruct (`(x, y) += 1` rejected) |
| Tuple destructure in args | `f((a, b)::Tuple{X, Y}) = …` | Supported since `06ac12c` |
| Array indexing | `x[i, j, k]` | Read and write, with update-ops |
| `getproperty` / `setproperty!` | `x.k`, `x.k = 1` | Set `permitCustomGetproperty = true` for virtual properties |
| Function call | `f(x, y; z = 1)` | Kwargs require explicit `;` separator |
| `if` / `while` / `for` / `return` / `break` / `continue` / ternary `? :` | | `for` accepts destructure: `for (x, (y, z)) in iter` |
| `let` blocks | `let x = 1, y, z::Int; … end` | Single declaration per var; no destructure in `let` |
| `local` / `global` | `local x = 1, y::Int` | Single declaration per var |
| `do`-block calls | `map(xs) do x; … end` | Supported since `#65` |
| Lambda / `->` | `x -> x*2` | Supported since `#20` |
| Anonymous block | `begin a; b; c end` | |
| Tuple literal | `(y, z)` | |
| Type assertion (expr) | `x::Int` | As both annotation and cast |
| `<:` / `where` in expr position | `T <: Number`, `Vector{T} where T<:Real` | Binary `<:`; unary `<:T` shorthand has known gaps — see [`checker_limitations.md`](checker_limitations.md) |
| String macros | `r"…"`, `b"…"`, `v"…"`, `@artifact_str` | Result types stamped from `src/compiler/CommonMacros.jl` |
| `@generated` in expr position | `(@generated) ? gen : runtime` | Modeled as `Bool` (since `36a1cb5`) |
| Splat `…` at call sites | `f(args...)`, `SCADPenalty(promote(a, γ)...)` | Result widens to `Any` (since `2387820`) |
| Vararg slurp in method args | `f(args::Pair...)` | Since `c9749c5` |
| Juxtapose | `2x`, `A'B` | Lowered to `*(args...)` via `inferReturnType` (since `d0f1729`) |
| `=== nothing` / `!== nothing` narrowing | `x === nothing ? a : b` | Splits the union; the false branch sees `x` minus `Nothing` |
| `function f end` forward declarations | | Skipped cleanly (since `6fd400d`) |
| Nested `where` | `f(x::T) where K where V<:T` | Multi-level type-param chains (since `6fd400d`) |
| Generators (minimal) | `[i+1 for i in 1:10]` | Scope handling recurses into args (since `b425a53`); full support is incomplete |
| Typed comprehensions | `T[expr for x in xs]` | Minimum viable (since `#85`) |
| Custom macros | `@my_macro x` | Skipped — body not analyzed, treated like `@nocheck`. Add result-type modeling in `CommonMacros.jl` if you want different behavior |

## Definitely not supported

| Construct | Workaround |
|---|---|
| Inline quote | `:(x + y)` — use `quote ... end`, or `@nocheck` |
| Named tuple literals | `(x = 1, y = 3)` — wrap in a struct |
| Multiple `for` in `for x in a, y in b` | Split into nested loops |
| `vect` literal | `[1, 2, 3]` — prefer typed `Int[1, 2, 3]` |
| `outer` keyword | No workaround; avoid |
| Broadcast assignment | `x .= x .+ 1` — write the loop explicitly |
| Nested function defs inside another method | Hoist to top-level |
| Single-call destructuring with mixed types into the same name | Use distinct local names |

## Partially supported (with gaps)

- **Unary `<:T` inside `Type{<:T}`** — `f(::Type{<:Exception})` fires
  `JTC-SYNTAX`. Needs proper bounded-TypeVar plumbing. See `#3c` in
  [`checker_limitations.md`](checker_limitations.md).
- **Default-expr `T(1)` in `where T<:Number`** — `f(γ::T = T(1))` fires
  because `T` is analyzed with bound `Any`. Workaround: hoist the
  default into a `oneunit(T)`-shaped helper. See `#3a`.
- **Full generator scope** — minimal support; complex comprehensions
  with nested filters may misbehave.

## Escape hatch — `@nocheck`

When a method legitimately uses a pattern the checker can't model:

```julia
SimpleTypeChecker.API.@nocheck function dynamic_thing(x)
    # body uses things the checker doesn't support
end
```

Any built-in macro at the function head (`@inline`, `@inbounds`) has
the same effect, because the checker doesn't recurse through macro
heads. That makes `@nocheck` interchangeable with `@inline` for the
"skip me" purpose if you don't want to depend on SimpleTypeChecker.

## When you hit an unsupported form

You have three options:

1. **Rewrite** to a supported form. The table above covers most
   common shapes.
2. **Isolate** the unsupported expression in a small helper marked
   `@inline` / `@nocheck`. The caller stays checked; the helper is
   trusted to be type-stable by the human who wrote it.
3. **Ignore syntax errors** for the whole run with
   `runCheck!(ctx; checkSyntax = false)`. Type errors still fire.

For the design rationale behind some of the "permanent" restrictions
(scoped if-blocks, abstract-arg call rejection), see the README's
`# Limitation` section.
