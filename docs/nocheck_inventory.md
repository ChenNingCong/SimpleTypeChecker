# `@nocheck` inventory

A living inventory of every `@nocheck function` site in `src/`, with a
category and a removal note. Goal: shrink this list over time. Update
this file when adding or removing a `@nocheck`.

Categories:

- **GENUINE** — touches Julia internals (`Core.Compiler.*`, raw `ccall`
  to runtime, untyped `mi.def` / `meth.sig` field access) or an external
  API where the typed surface would be a big refactor. Keep.
- **BOUNDARY** — deliberately crosses an `Any`-typed value. Mostly
  resolved by `@nospecialize` + single-method escape hatch (so the
  checker now accepts them without `@nocheck`); a handful still need
  the marker because they splat into `apply_type` or use parametric
  `where T`.
- **LEGACY** — author hadn't typed the body. Removal target. Most of
  these are now done.

Last updated: 2026-05-25 (after the JuliaSyntax wrapper review +
try/catch-merge + Builtin / per-arity-escape-hatch / FunctionWrapper
pass).
Count: `grep -c "@nocheck function" -r src` → **27**, down from 78.

Breakdown of the remaining 27:
- `src/compiler/CompilerAdaptor.jl`: **12** — adaptor seam to
  `Core.Compiler.*`, `MethodInstance`/`Method` field access, trace
  helpers. Kept by design (core-compiler component to be trimmed
  separately).
- Application-level (15 across the rest):
  - **abstract IO interface contract** (4): `Base.show(::IO, ::JuAST)`,
    `displayJuASTHelper`, `Base.show(::IO, ::ScopeInfo)`,
    `displayContext(::IO, ::Context)`. `io::IO` is the public
    `Base.show` contract — can't tighten without breaking the
    interface.
  - **`apply_type` splat over `Vector{Any}`** (3):
    `makeNameTupleType`, `dispatchTraceForCall`, `makeTupleType`.
    Splatting a `Vector{Any}` into a parametric Tuple/NamedTuple is
    exactly the case Julia's type system can't infer statically.
  - **by-design runtime evaluation** (4): `fasteval`,
    `mapAST2Method`, `addFile!`, `runCheck!` — `Core.eval`, AST
    walking, file I/O.
  - **HTTP server** (2): `query`, `handleQuery`. Abstract IO; never
    trimmed.
  - **walkdir** (1): `checkPackage` — abstract iterator from
    `walkdir`. Could be eliminated with a small refactor.
  - **HTML debug helper** (1): `parseAndConstructHTML`.

### JuliaSyntax wrapper review (2026-05-25)

Reviewed `untokenize` and `parseJuAST` empirically with
`juliac --trim=safe`:

- `JuliaSyntax.untokenize(::SyntaxHead; include_flag_suff=true)`
  trim-compiles **cleanly** (no verifier errors). Our checker also
  accepts the wrapper once `::String` is asserted on the return.
  `@nocheck` removed.
- `JuliaSyntax.parseall(SyntaxNode, str; filename)` with the
  **registered package version** produced 8 verifier errors, all
  in JuliaSyntax error-emission paths. With the **in-tree version
  (`Base.JuliaSyntax`)**, that drops to **2 errors, 0 warnings**
  — both for the same `parse_block_inner` call (`::Function`
  callback). The error-path issues were fixed in-tree:
  - `const _token_error_descriptions = Dict{Kind, String}(...)` —
    one-word `const` pins the binding type so `getindex(::Dict{Kind,String}, ::Kind)::String` resolves statically.
  - `internal_error(strs::Vararg{String, N}) where {N}` — typed
    Vararg with concrete element type and length parameter, so
    each call site specializes and the splat unrolls statically.

Migrated to the in-tree `Base.JuliaSyntax` (Julia 1.12+). Three
shape differences also needed handling:
- `module` lost the leading boolean-bareness child.
- `try` is now `(try block (catch var body) (else body) (finally body))`
  with optional clause sub-nodes instead of a fixed 5-tuple.
- `for` wraps the binding clause(s) in a new `:iteration` node;
  binding uses `:in` instead of `:=`.
- `a.b` is `(. a b)` (bare identifier) instead of `(. a (quote b))`.
- `GreenNode.children` (was `args`) is `Union{Nothing, Vector}`
  instead of `Union{Tuple{}, Vector}`.
- `_kind_names::Vector{String}` is now `_kind_int_to_str::Dict{UInt16, String}`.

Conclusion (validated empirically): "JuliaSyntax returns Any in
places" is independent of whether our wrapper is trimmable from
our checker's standpoint. Don't add `@nocheck` defensively —
only when our checker actually fires. The in-tree version closes
most of the trim gap for free.

## Removed this pass (strike-throughs preserved for history)

The following moved from `@nocheck` to checked, sometimes with a
`@nospecialize` annotation on an Any arg so the escape-hatch
detector (`_escapeHatchMask` + `_isEscapeHatchMethod` in
`Inference.jl`) accepts the typed-to-Any seam. Tests stay green (76/76)
and the deep-mode self-check fire count is unchanged at 1.

### src/api/API.jl
- ~~`checkFile(mod::Module, path::String)::GlobalContext`~~ — typeable as is.

### src/inference/InferenceDefinition.jl
- ~~`makeConstVal(val::Any)`~~ — added `@nospecialize val`.
- ~~`makeType(val::Any)`~~ — added `@nospecialize val`.
- ~~`removeConst(typ::CompileType)::CompileType`~~ — single-method, body
  routes through `@nospecialize`'d `makeType`.
- ~~`lift(v::CompileType)::CompileType`~~ — same.
- ~~`convert2ConstVal(val::JuASTVal)::CompileType`~~ — same.
- ~~`isBottomType(t::CompileType)::Bool`~~ — `t.typ === Union{}` uses
  the `===` Builtin instead of `==` so it stays checkable.
- ~~`inferExpand(eng, ctx, e::MacroExpander, ast)`~~ — single-method,
  body's `e.expander(eng, ctx, ast)` dispatches through a typed callable.

### src/inference/InferenceError.jl
- ~~`printErrorHead(eng, ast, errkind)`~~ — converted to typed
  `writelog!` primitives.
- ~~`printWarningHead(eng, ast, errkind)`~~ — same; now buffers
  through `eng.errio` instead of writing to stdout.
- ~~`explainAbstractType(typ::CompileType)::String`~~ — routes
  type-to-string through `_type_to_string` (escape hatch).
- ~~`isBuiltinEqual(tt::CompileType)::Bool`~~ — predicate using `===`
  Builtin.

### src/inference/InferenceErrorUtility.jl
- ~~`toString(typ::CompileType)::String`~~ — body now routes through
  `_type_to_string` (escape hatch). Caller's abstract-arg check still
  passes because the seam is at `_type_to_string`'s `@nospecialize` arg.
- ~~`_to_string_fallback(t::Any)::String`~~ — added `@nospecialize t`;
  body delegates to `Base.string`.
- ~~`printSignature(io::IOBuffer, f::CompileType, ...)`~~ — uses
  `_type_to_string` and concrete IOBuffer `write`s.
- ~~`getTypeSignature(args::Vector{FlowNode})::String`~~ — same.
- ~~`toIndex(v::Vector{Int})::String`~~ — rewrote with explicit for-loop
  instead of `map`/`join` lambdas.

### src/inference/Inference.jl
- ~~`isTypeValue(x::Any)::Bool`~~ — added `@nospecialize x`.
- ~~`subtypeOf(a::Any, b::Any)::Bool`~~ — added `@nospecialize` on both.
- ~~`makeUnionAll(tv::TypeVar, base::Any)`~~ — added `@nospecialize base`.
- ~~`isaJuASTVal(val::JuASTVal, typ)::Bool`~~ — added `@nospecialize typ`.
- ~~`isPassthroughMacroName(name::Symbol)::Bool`~~ — checkable.
- ~~`macroIdentifierName(ast::JuAST)::Symbol`~~ — checkable.
- ~~`inferMacroArgsForSideEffects!(...)`~~ — checkable.
- ~~`inferMacroCall(eng, ctx, ast)`~~ — checkable.
- ~~`inferSubtypeOp(...)`~~ — checkable.
- ~~`inferTypeAssertExpr(...)`~~ — checkable.
- ~~`inferLambdaExpr(...)`~~ — checkable.
- ~~`inferWhereType(...)`~~ — checkable.
- ~~`inferCCall(...)`~~ — checkable.
- ~~`tryDestructTuple(...)`~~ — checkable.
- ~~`makeMatrixType(typ::CompileType)::CompileType`~~ — body routes
  through `@nospecialize`'d `makeType(Matrix{typ.typ})`.
- ~~`isCallableMethodDef(ast::JuAST)::Bool`~~ — predicate; checkable.
- ~~`collectToplevelFunction!(...)`~~ — checkable.

### src/syntax/SyntaxDefinition.jl
- ~~`makeConstJuASTVal(val::Any)`~~ — added `@nospecialize val`.

### src/syntax/SyntaxAdaptor.jl
- ~~`checkField(ast::JuAST, field::Symbol)::Bool`~~ — checkable.

## Remaining @nocheck (53)

### src/compiler/CompilerAdaptor.jl — GENUINE (keep)

Everything in this file is the adaptor seam to `Core.Compiler.*`, raw
`Method` / `MethodInstance` field access, and `ccall`s to the Julia
runtime. `@nocheck` is the contract here, not a sign of legacy laziness.

| line | function |
| --- | --- |
| 17  | `isInPureContext()` |
| 21  | `currentWorld()` |
| 27  | `inferReturnType(ctx, sig, f, types; world)` |
| 121 | `_callee_inferred_type(f, ci)` |
| 138 | `_is_static_callee(f, ci)` |
| 170 | `_codeinfo_has_dyncall(ci)` |
| 203 | `_codegen_has_dyncall(interp)` |
| 236 | `inferKeywordMethodHandles(ctx, sig)` |
| 248–354 | MethodHandle / MethodDef accessors, including the new escape-hatch helpers (`methodDefNospecializeMask`, `methodDefNargs`, `methodDefIsVarArg`, `methodDefNKw`, `methodsOf`) |

### src/inference/InferenceDefinition.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 424 | `Base.write(log::ErrorLogger, x::UInt8)` | GENUINE | Base method extension. |
| 610 | `Base.show(io::IO, info::ScopeInfo)` | GENUINE | abstract IO. |
| 761 | `displayContext(io::IO, ctx::Context)` | GENUINE | abstract IO. |

Removed in this pass: `displayReport` — converted `println(io, ...)`
to typed `writelogln!` + `string()` interpolation.

### src/inference/InferenceError.jl

Removed in this pass: `reportErrorFunCall` — checkable as is.

### src/inference/Inference.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 58  | `castJuASTVal(typ::Type{T}, val::JuASTVal)::T where T` | BOUNDARY | parametric `where T`; can't easily eliminate. |
| 479 | `_isEscapeHatchMethod(meth::MethodDef)` | GENUINE | introspects Method + Function. |
| 510 | `_escapeHatchMask(fargs)` | GENUINE | same. |
| 2491 | `fasteval(mod, x, params, implicit)` | GENUINE | `Core.eval`. |
| 2786 | `evalGetPropertyChain(parent, s)` | GENUINE | runtime `getproperty`. |
| 2807 | `mapAST2Method(mod, fundef)` | GENUINE | bridges AST → Method. |
| 2964 | `addFile!(ctx, mod, filename)` | LEGACY-?? | removing trips the README self-check via a KeyError in the file→AST map. Keep for now. |
| 2998 | `runCheck!(ctx; checkSyntax)` | GENUINE | `eval(Symbol(name))` for the displayError dispatch. |

### src/inference/UtilityUnType.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 53  | `makeNameTupleType(args)` | BOUNDARY | splats into `NamedTuple{...}` with abstract elements. |
| 263 | `makeTupleType(tt)` | BOUNDARY | splats into `Tuple{...}` with abstract elements. |

### src/server/SimpleHTTPServer.jl — GENUINE (HTTP, abstract IO)

| line | function |
| --- | --- |
| 46  | `query(f::QueryFunctor, method, body)::String` |
| 66  | `handleQuery(f::QueryFunctor)` |

### src/syntax/JSAdaptor.jl — GENUINE (HTML emission)

| line | function |
| --- | --- |
| 10  | `parseAndConstructHTML(raw, filename, outdir)` |

### src/syntax/SyntaxAdaptor.jl — GENUINE (JuliaSyntax internals)

| line | function |
| --- | --- |
| 57  | `getData(tree)::JuliaSyntax.SyntaxData` |
| 65  | `getNodeVal(tree)::JuASTVal` |
| 69  | `untokenize(head::JuliaSyntax.SyntaxHead)` |
| 132 | `getChildren(tree)` |
| 273 | `parseJuAST(str, filename)::JuAST` |

### src/syntax/SyntaxDefinition.jl — GENUINE (Base.show + JuliaSyntax)

| line | function |
| --- | --- |
| 37  | `getNodeVal(tree::JuliaSyntax.TreeNode{...})` |
| 108 | `Base.show(io::IO, ast::JuAST)` |
| 113 | `displayJuASTHelper(io, ast, id)` |

## Next removal candidates

Just one LEGACY-?? left:

1. `addFile!(ctx, mod, filename)` — removing it trips the README
   self-check via a KeyError in the file→AST map. Needs a closer
   look to figure out which body call confuses the checker.

Beyond that, the remaining 51 entries are GENUINE adaptor seams or
BOUNDARY shapes (parametric `where T`, splatted `apply_type`) that
can't be cleaned up without changing the algorithm.

Each removal should come with a test exercising the function path.
