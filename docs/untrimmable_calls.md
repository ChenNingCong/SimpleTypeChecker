# Untrimmable function calls — inventory & analysis

A catalog of every dynamic-dispatch site that the deep-mode self-check
flags or that an `@nocheck` marker currently hides. For each site we
record the offending call, what dispatch is actually happening, and an
assessment of how hard it would be to eliminate.

Methodology:
- Section 1 is the **observed** list: ran `runCheck!` on the checker's
  own source with `ctx.deepDispatchCheck = true`. Every `DispatchError`
  line is one row.
- Section 2 is the **hidden** list: every `@nocheck function` in `src/`.
  These functions are not analyzed by the checker — the dynamic
  dispatch lives in the body, behind the marker.

Difficulty key:

| Level | Meaning |
| --- | --- |
| **EASY** | Solvable with a single-method `@nospecialize` escape hatch, a `===`/`isa` rewrite, or a `writelog!` routing. Mechanical. |
| **MEDIUM** | Needs a small refactor: narrow a `Union`, hoist abstract storage into a parametric struct, replace a `string(::Any)` chain with a typed builder. |
| **HARD** | Touches Julia internals (`Core.Compiler.*`, `apply_type` with splat, `getproperty`, `eval`). Would require a substantial new abstraction or a Julia-level fix. |
| **BY-DESIGN** | The site IS the escape hatch — making it static would defeat its purpose (e.g. `_to_string_fallback` for non-Type values, the runtime/intern adaptor seam). |

Last updated: 2026-05-25. Deep self-check fires: **17 → 0**.

---

## Recent checker improvements (false positives suppressed)

Three changes in the checker proper, none in the inspected codebase:

1. **Builtin-callee suppression**. When `fargs[1]` is a `Core.Builtin`
   or `Core.IntrinsicFunction` (`isa`, `===`, `typeof`, `getfield`,
   `tuple`, …), the abstract-arg check no longer fires for user args.
   These lower to intrinsics — no method-table lookup happens at
   runtime even when the slot is `Any`.

2. **Per-arity escape-hatch matching**. `_escapeHatchMask` previously
   required the callee to have exactly one method total. Now it
   accepts the callee if exactly one of its methods matches the call's
   arity (and that method has `@nospecialize`). Recognizes
   `Base.rethrow` (one zero-arg + one 1-arg `@nospecialize` method).

3. **`printDispatchTrace` flag**. Set
   `ctx.printDispatchTrace = true` (in addition to
   `deepDispatchCheck`) to have every `InferenceErrorTopLevelDispatch`
   include the transitive `:call` survivors as `· <method> @
   <file>:<line> — <callee>(...)` lines. Implemented in
   `_codeinfo_collect_dyncalls!` and surfaced via
   `dispatchTraceForCall` → `displayErrorTopLevelDispatch`.

## Code changes that eliminated the remaining fires

The trace flag made the deep causes visible. Each row was fixed with a
small targeted change:

| Original fire | Fix |
| --- | --- |
| `isaAny(x) -> x.typ == Any` | `==` → `===` (Builtin); same for `isaBool`. |
| `tryMergeFlowNode -> v1 != v2` | `!=` → `!(v1 === v2)`. |
| `splitUnion -> collectUnion(::Any)` | Added `@nospecialize` to `collectUnion`'s recursive arg. |
| `convert2JuAST!:111 println(tree, head)` | Deleted (leftover debug `println`). |
| `inferReturn` debug `println(newnode.typ)` | Deleted. |
| `collectToplevelFunction! catch println` | Replaced with `writelog!/writelogln!` on `ctx.errio`. |
| `formatLocation -> String * Char * String * Char * String` | Switched to typed IOBuffer `write`s. |
| `getFirstParameter -> x.typ.parameters[1]` | Routed through `_firstTypeParam(@nospecialize)` with `(t::DataType).parameters[1]` typeassert. |
| `methodHandleSpecTypesLength/Param` | Same pattern: `_specTypesLength` / `_specTypesParam` escape hatches. |
| `evalGetPropertyChain -> getproperty(::Module, ::Symbol)` | `getproperty` (multi-method) → `getglobal` (Builtin); added isa-Module narrowing in loop. |
| `fasteval -> getproperty(::Module, ::Symbol)` and `getproperty(::Any, ::Symbol)` | Same `getproperty` → `getglobal` substitution, plus `parent isa Module` narrowing. |
| `inferExpand -> e.expander(eng, ctx, ast)` (abstract callable) | Added `FunctionWrappers.jl` dep; `MacroExpander.expander` is now `FunctionWrapper{Any, Tuple{Any,Any,Any}}` — a concrete type so the call statically dispatches. |

Final state: **deep self-check fires = 0**, tests **78 / 78** passing.

---

## Section 1 — observed deep self-check fires (all cleared)

All 17 fires originally captured by running `runCheck!` over the
checker source with `ctx.deepDispatchCheck = true` have been
eliminated. See the "Code changes that eliminated the remaining
fires" table above for the per-site fix.

| Class | Original count | Now |
| --- | --- | --- |
| A — abstract slot at call site (callee or arg is `Any` *here*) | 6 | 0 |
| B — abstract deeper in the transitive call tree | 11 | 0 |

Verification: `ctx.deepDispatchCheck = true` over the full src/ set
emits no `DispatchError`. The same run with `printDispatchTrace = true`
also emits no `·` trace lines, confirming the inferencer no longer
finds any post-inference `:call` survivors across the checker's own
code.

---

## Section 2 — hidden dispatch (`@nocheck function` markers, 34 sites)

These are intentionally excluded from analysis. The body of each
contains dynamic dispatch that we've deliberately accepted. Grouped by
where the dispatch comes from.

### 2.1 — Adaptor to `Core.Compiler.*` / `Method` / `MethodInstance` internals (HARD)

File: `src/compiler/CompilerAdaptor.jl`

| Function | Body shape | Why dyn-dispatch | Elimination |
| --- | --- | --- | --- |
| `isInPureContext()` | `ccall(:jl_is_in_pure_context, Bool, ())` | None at runtime — the `@nocheck` is purely because the checker doesn't model bare `ccall` returns | **EASY** (low value): remove `@nocheck`; might just work, low risk. |
| `currentWorld()` | `Core.Compiler.get_world_counter()` | Returns `UInt`; concrete | **EASY**: remove `@nocheck`. |
| `inferReturnType(ctx, sig, f, types; world)` | `_methods_by_ftype` + `specialize_method` + `InferenceState` + `typeinf` | Every Compiler API takes/returns `Any` shapes | **HARD**: this IS the adaptor. |
| `_callee_inferred_type(f, ci)` | `ci.ssavaluetypes[f.id]` | Reads untyped Core.CodeInfo vector | **HARD**. |
| `_is_static_callee(f, ci)` | isa-dispatches f over `IntrinsicFunction`/`Builtin`/`GlobalRef`/`QuoteNode` | Argument `f` is `Any` | **MEDIUM**: add `@nospecialize f`; body uses isa-dispatch already. |
| `_codeinfo_has_dyncall(ci)` | walks `ci.code::Vector{Any}` | Statements are `Any` | **HARD**: `ci.code` element type is `Any` by design. |
| `_codegen_has_dyncall(interp)` | iterates `interp.codegen::IdDict{Any,Any}` | Untyped Dict | **HARD**. |
| `inferKeywordMethodHandles(ctx, sig)` | same shape as `inferReturnType` | Compiler internals | **HARD**. |
| `methodDefCodeInfo(meth)` | `Base.code_typed_by_type(meth.sig)[1].first` | Returns `Any` | **HARD**. |
| `methodsOf(@nospecialize f)` | `for m in methods(f); push!(result, m::MethodDef)` | `methods(f)` returns `Base.MethodList`, abstract | **MEDIUM**: already has `@nospecialize`; single method on function `methodsOf` → eligible as escape hatch. Could be removed if the checker's escape-hatch path covers this signature. |

The MethodHandle/MethodDef accessor block (lines ~248–354) has already
been *de-`@nocheck`'d* (visible in git: `methodDefSig`,
`methodHandleSpecTypes`, …). Remaining ones inside this section touch
`.sig.parameters`, `.specTypes.parameters`, etc., which are `Any`
parametric tuples — eliminating them would mean teaching the checker
about parametric `Tuple{...}` field access.

### 2.2 — Untrimmable IO / display (MEDIUM)

| Function | File | Body shape | Difficulty |
| --- | --- | --- | --- |
| `Base.write(log::ErrorLogger, x::UInt8)` | `InferenceDefinition.jl:424` | `write(log.io, x)` — IOBuffer write returns `Int` but `Base.write` expects `Int64` and Julia dispatches it | **EASY**: typed enough already; the `@nocheck` is legacy. Try removing. |
| `Base.show(io::IO, info::ScopeInfo)` | `InferenceDefinition.jl:610` | `println(io, "...", prettySymbols(...))` × 4 | **MEDIUM**: `io::IO` is abstract by interface contract. Could specialize on `::ErrorLogger`, but breaks `show(::IOContext, …)`. |
| `displayContext(io::IO, ctx::Context)` | `InferenceDefinition.jl:761` | `println(io, "...")` and `for (k,v) in ...` with `repr(v.curtyp.typ.val)` | **MEDIUM**: same `io::IO`. Plus `repr(::Any)`. |
| `Base.show(io::IO, ast::JuAST)` | `SyntaxDefinition.jl:108` | `println(io, ...)` + `displayJuASTHelper` | **MEDIUM**: same. |
| `displayJuASTHelper(io, ast, id)` | `SyntaxDefinition.jl:113` | `println(io, ' '^id, vstr, ' ', Int(s.first), ':', Int(s.second))` | **MEDIUM**: same plus heterogeneous `println` varargs (always dyn-dispatches). |

These are debug/REPL helpers. None of them participate in the trimmed
binary's call graph (no entry point reaches `Base.show` for a JuAST or
ScopeInfo). Lower priority for elimination.

### 2.3 — Runtime intrinsics (BY-DESIGN)

| Function | File | Why dynamic | Eliminable? |
| --- | --- | --- | --- |
| `fasteval(mod, x, params, implicit)` | `Inference.jl:2497` | `getproperty(mod, sym)`, `Core.eval`, recursive AST eval | **BY-DESIGN**: the whole point is to evaluate user-supplied type expressions at runtime. |
| `evalGetPropertyChain(parent, s)` | `Inference.jl:2792` | `getproperty(base, i)` in a loop | **BY-DESIGN**. |
| `mapAST2Method(mod, fundef)` | `Inference.jl:2813` | Walks an AST and looks up methods in a module at runtime | **BY-DESIGN**. |
| `runCheck!(ctx; checkSyntax)` | `Inference.jl:3004` | `eval(Symbol(name))` for displayError dispatch | **MEDIUM**: the `eval` is for displayError; could lift to a dispatch table over a closed set of error kinds. |
| `addFile!(ctx, mod, filename)` | `Inference.jl:2970` | Single legacy site — removing it currently trips the README self-check via a KeyError | **MEDIUM**: needs investigation of the file→AST map. Listed as LEGACY-?? in the inventory. |

### 2.4 — Escape-hatch helpers themselves (BY-DESIGN)

| Function | File | Why | Eliminable? |
| --- | --- | --- | --- |
| `_isEscapeHatchMethod(meth)` | `Inference.jl:485` | `getfield(mod, name)` + `length(methods(fval))` — both return `Any`-typed values | **BY-DESIGN**: the function exists to ask runtime questions. |
| `_escapeHatchMask(fargs)` | `Inference.jl:516` | same family | **BY-DESIGN**. |

### 2.5 — `apply_type` splat with abstract elements (HARD)

| Function | File | Body | Difficulty |
| --- | --- | --- | --- |
| `makeNameTupleType(args)` | `UtilityUnType.jl:53` | `NamedTuple{tuple([i.first for i in args]...), Tuple{[i.second.typ for i in args]...}}` | **HARD**: splatting a `Vector{CompileType}` into a parametric Tuple is exactly the case Julia's type system can't infer statically. |
| `makeTupleType(tt)` | `UtilityUnType.jl:263` | `Tuple{[i.typ.typ for i in tt]...}` | **HARD**: same. |
| `castJuASTVal(@nospecialize typ, val)` | `Inference.jl:61` | `val.val isa typ` — `typ` is a runtime type | **EASY** (already rewritten to drop the `where T` form). The remaining `@nocheck` may be stale — verify and remove. |

### 2.6 — JuliaSyntax / SyntaxAdaptor seams (HARD)

| Function | File | Why | Difficulty |
| --- | --- | --- | --- |
| `getData(tree)` | `SyntaxAdaptor.jl:57` | `tree.data` — `tree::JuliaSyntax.TreeNode` field has Union type with Nothing | **MEDIUM**: type the arg as `JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}` and the `data` field becomes typed. |
| `getNodeVal(tree)` | `SyntaxAdaptor.jl:65` | `tree.data.val` is `Any` | **EASY**: `makeConstJuASTVal` has `@nospecialize val` — escape hatch already covers the seam. |
| `untokenize(head)` | `SyntaxAdaptor.jl:69` | `JuliaSyntax.untokenize(head;…)` — return is `Any` | **HARD**: JuliaSyntax API returns `Any`. |
| `getChildren(tree)` | `SyntaxAdaptor.jl:132` | `tree.children` — Union type | **MEDIUM**: same as `getData`. |
| `parseJuAST(str, filename)` | `SyntaxAdaptor.jl:273` | `JuliaSyntax.parseall(...)` returns abstract `SyntaxNode` | **HARD**: external API. |
| `getNodeVal(tree::JuliaSyntax.TreeNode{...})` | `SyntaxDefinition.jl:37` | `tree.data.val::Any` | **EASY**: same as `SyntaxAdaptor.jl:65`. |
| `Base.show(io::IO, ast::JuAST)` | `SyntaxDefinition.jl:108` | see §2.2 | — |
| `displayJuASTHelper` | `SyntaxDefinition.jl:113` | see §2.2 | — |

### 2.7 — Server / HTML (BY-DESIGN, untrimmable)

| Function | File | Why | Difficulty |
| --- | --- | --- | --- |
| `query(f, method, body)` | `server/SimpleHTTPServer.jl:46` | HTTP.jl I/O is fundamentally abstract IO | **BY-DESIGN**: the server is never trimmed; it's developer-facing. |
| `handleQuery(f)` | `server/SimpleHTTPServer.jl:66` | same | **BY-DESIGN**. |
| `parseAndConstructHTML` | `syntax/JSAdaptor.jl:10` | String-building over `Any` AST shape | **BY-DESIGN**. |

---

## Summary

| Difficulty | Section 1 (observed) | Section 2 (`@nocheck` hidden) | Total |
| --- | --- | --- | --- |
| EASY | 0 (was 3) | ~7 | ~7 |
| MEDIUM | 0 (was 6) | ~10 | ~10 |
| HARD | 0 (was 2) | ~13 | ~13 |
| BY-DESIGN | 0 (was 1) | ~6 | ~6 |
| Total | **0** (was 17) | **34** | **34** |

### What's left

All remaining dispatch sites are inside `@nocheck` markers (Section 2)
— bodies the checker doesn't analyze. They split into the same three
buckets as before:

- **§2.1 Compiler adaptor seams** — `Core.Compiler.*`, raw
  `MethodInstance` / `Method` field access. The adaptor is the
  designated dynamism container; removing more `@nocheck`s here means
  rewriting `Core.Compiler` surfaces.

- **§2.2 Debug/REPL show methods** — `Base.show(::IO, ::JuAST)`,
  `displayContext`, `displayJuASTHelper`. Abstract `::IO` is the
  interface contract. Not in any trimmed entry's call graph.

- **§2.3 Runtime intrinsics** — `fasteval` (the 4-arg one), the
  `mapAST2Method` AST→Method walker, `addFile!`'s file→AST scaffold,
  `runCheck!`'s `eval(Symbol(name))` displayError dispatch. By design.

- **§2.5 `apply_type` splat over `Vector{Any}`** — `makeNameTupleType`,
  `makeTupleType`. Splatting abstract Vectors into parametric Tuple
  constructors is exactly the case Julia's type system can't infer.

- **§2.6 JuliaSyntax internals** — wrappers around abstract
  `TreeNode.data`/`children` and `parseall` return value. External API.

- **§2.7 Server / HTML** — never trimmed; developer-facing only.

### Patterns that worked

For future similar work in this codebase:

1. **`==` / `!=` between two `Any` slots → `===` / `!(===)`.** `===` is
   a Builtin, statically resolved even with `Any` args; `==` is a
   regular function that requires method-table lookup. Same effect for
   `!==`.

2. **`getproperty(::Module, ::Symbol)` → `getglobal`.** `getproperty`
   has multiple methods; `getglobal` is a Builtin that walks the same
   module-binding table. For a Module path with isa-narrowing in the
   loop, this is a drop-in replacement.

3. **`x.typ::Any` field access from a typed struct →
   `_helper(@nospecialize(t))` with `(t::DataType).parameters[i]`.**
   The typeassert is `Core.typeassert` (Builtin), narrows the dynamic
   value; the field then resolves statically. Single-method
   `@nospecialize` makes the call site an escape hatch.

4. **Heterogeneous callable-field dispatch table →
   `FunctionWrapper{R, A}` from `FunctionWrappers.jl`.** The wrapper
   struct is concrete, so the call statically dispatches; the ccall
   trampoline preserves runtime-heterogeneous behavior. Use `Any`
   type-parameters if the concrete types are defined later in the
   file (forward-reference cycle).

5. **Leftover debug `println(x, y)`** — just delete it. Always
   dyn-dispatches via abstract `IO`, and the user-facing path already
   has `writelog!`/`writelogln!` on `ErrorLogger`.

6. **String formatting via `*` and `string(::Int)`** → IOBuffer with
   typed `write(::String)` / `write(::Int)`. The `*` operator
   eventually goes through `string` varargs which dyn-dispatches; the
   typed `write` methods don't.
