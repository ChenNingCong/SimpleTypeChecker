# `@nocheck` inventory

A living inventory of every `@nocheck function` site in `src/`, with a
category and a removal note. Goal: shrink this list over time. Update
this file when adding or removing a `@nocheck`.

Categories:

- **GENUINE** — touches Julia internals (`Core.Compiler.*`, raw `ccall`
  to runtime, untyped `mi.def` / `meth.sig` field access) or an external
  API where the typed surface would be a big refactor. Keep.
- **BOUNDARY** — deliberately crosses an `Any`-typed value. The function
  is a serializer / coercion at a typed-to-Any seam. `@nocheck` is a
  documented escape hatch; keep but revisit if a typed alternative
  appears (e.g. a new `jl_*` runtime symbol).
- **LEGACY** — the original author didn't get around to typing the body.
  Should be checkable after a small refactor. Removal is the next step.
- **DONE** — already cleaned up (kept here for history during the
  reduction pass).

Last updated: 2026-05-25. Count at this snapshot: **78** `@nocheck
function` declarations. Recent removals: `printErrorHead`,
`printWarningHead`, `toIndex`. Use `grep -c "@nocheck function"
src -r` to keep this number honest.

## src/api/API.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 74 | `checkPackage(mod::Module)::GlobalContext` | LEGACY | top-level wrapper; reads paths and calls `addFile!` per file. Should typecheck after small cleanup. |
| 97 | `checkFile(mod::Module, path::String)::GlobalContext` | LEGACY | same pattern as `checkPackage`. |

## src/compiler/CompilerAdaptor.jl

Everything in this file is **GENUINE** by design — the adaptor owns the
seam to `Core.Compiler.*`, raw `Method` / `MethodInstance` field access,
and `ccall`s to the Julia runtime. `@nocheck` is the contract here, not
a sign of legacy laziness.

| line | function |
| --- | --- |
| 17  | `isInPureContext()` (ccall) |
| 21  | `currentWorld()` (ccall via Core.Compiler) |
| 27  | `inferReturnType(ctx, sig, f, types; world)` (NativeInterpreter / typeinf) |
| 121 | `_callee_inferred_type(f, ci)` (ssavaluetypes/slottypes via Any) |
| 138 | `_is_static_callee(f, ci)` (introspects Core.* objects) |
| 170 | `_codeinfo_has_dyncall(ci)` (walks CodeInfo.code) |
| 203 | `_codegen_has_dyncall(interp)` (iterates interp.codegen) |
| 236 | `inferKeywordMethodHandles(ctx, sig)` (\_methods\_by\_ftype) |
| 248–317 | MethodHandle / MethodDef accessors (raw field reads) |

## src/inference/InferenceDefinition.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 16  | `isBottomType(t::CompileType)::Bool` | BOUNDARY | reads `t.typ::Any`. Could check via the `_type_to_string`-style isa dispatch. |
| 24  | `makeType(val::Any)` | BOUNDARY | builds a CompileType from arbitrary value. |
| 40  | `removeConst(typ::CompileType)` | BOUNDARY | strips Const wrapper, returns Any. |
| 48  | `lift(v::CompileType)` | BOUNDARY | type lift, touches val::Any. |
| 56  | `convert2ConstVal(val::JuASTVal)::CompileType` | LEGACY | JuASTVal → CompileType conversion. Probably checkable. |
| 424 | `Base.write(log::ErrorLogger, x::UInt8)` | GENUINE | Base method extension. |
| 610 | `Base.show(io::IO, info::ScopeInfo)` | GENUINE | Base method extension over abstract IO. |
| 698 | `inferExpand(eng, ctx, e, ast)` | LEGACY | macro expansion via `Base.macroexpand` — could perhaps stay GENUINE if we touch macro objects. |
| 761 | `displayContext(io::IO, ctx::Context)` | GENUINE | abstract IO. |
| 812 | `displayReport(io::ErrorLogger, r::InferReport)` | LEGACY | could use typed writelog! primitives. |

## src/inference/InferenceError.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 134 | `reportErrorFunCall(eng, ast, ms, num, isDotcall)` | LEGACY | builds an error message; touches Symbol → String via interpolation. Should be checkable with typed primitives. |
| 469 | `explainAbstractType(typ::CompileType)::String` | BOUNDARY | inspects `typ.typ::Any` and returns human-readable text. Same seam as `_type_to_string`. |
| 481 | `isBuiltinEqual(tt::CompileType)::Bool` | LEGACY | predicate; probably checkable. |

## src/inference/InferenceErrorUtility.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 17  | `_to_string_fallback(t::Any)::String` | BOUNDARY | last-resort `string(t)` for non-type values. |
| 70  | `toString(typ::CompileType)::String` | BOUNDARY | thin wrapper around `_type_to_string`. |
| 95  | `printSignature(io::IOBuffer, f, typ, kwtyp)` | BOUNDARY | uses `_type_to_string`. |
| 152 | `getTypeSignature(args::Vector{FlowNode})::String` | BOUNDARY | same. |

`_type_to_string` itself is **CHECKED** (uses `@nospecialize` to keep one
MI; the body's isa-dispatch is statically resolvable).

## src/inference/Inference.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 51  | `isaJuASTVal(val, typ)::Bool` | BOUNDARY | predicate on val::JuASTVal with abstract typ. |
| 58  | `castJuASTVal(typ::Type{T}, val)::T where T` | BOUNDARY | unsafe cast. |
| 129 | `isPassthroughMacroName(name::Symbol)::Bool` | LEGACY | simple list of names; trivial check. |
| 143 | `macroIdentifierName(ast::JuAST)::Symbol` | LEGACY | extracts a symbol from AST. |
| 165 | `inferMacroArgsForSideEffects!(eng, ctx, ast)` | LEGACY | walks macro args. |
| 188 | `inferMacroCall(eng, ctx, ast)` | LEGACY / GENUINE — borderline; macro expansion is dynamic. |
| 281 | `isTypeValue(x::Any)::Bool` | BOUNDARY | predicate on Any. |
| 285 | `subtypeOf(a::Any, b::Any)::Bool` | BOUNDARY | subtype op on Any pair. |
| 289 | `makeUnionAll(tv::TypeVar, base::Any)::Any` | BOUNDARY | builds UnionAll. |
| 297 | `inferSubtypeOp(...)` | LEGACY | structural inference logic. |
| 325 | `inferTypeAssertExpr(...)` | LEGACY | structural. |
| 384 | `inferLambdaExpr(...)` | LEGACY / GENUINE — uses module-level eval (touches `eval`), borderline. |
| 406 | `inferWhereType(...)` | LEGACY | structural. |
| 446 | `inferCCall(...)` | LEGACY | special-cases ccall expressions. |
| 1279 | `tryDestructTuple(...)` | LEGACY | tuple-destructuring helper. |
| 1545 | `makeMatrixType(typ::CompileType)::CompileType` | BOUNDARY | builds a parametric matrix type from typ.typ::Any. |
| 2404 | `fasteval(...)` | GENUINE | calls `Core.eval` directly. |
| 2699 | `evalGetPropertyChain(parent::Module, s::Vector{Symbol})` | GENUINE | runtime getproperty chain. |
| 2720 | `mapAST2Method(mod, fundef)::Vector{MethodDef}` | GENUINE | bridges AST to runtime Methods. |
| 2781 | `isCallableMethodDef(ast::JuAST)::Bool` | LEGACY | predicate on AST shape. |
| 2808 | `collectToplevelFunction!(...)` | LEGACY | top-level walker; mostly structural. |
| 2877 | `addFile!(ctx, mod, filename)` | LEGACY | reads file + parses; trivial. |
| 2911 | `runCheck!(ctx; checkSyntax)` | GENUINE | dispatches `displayError*` via `eval(Symbol(name))` — that's the actual runtime indirection. |

## src/inference/UtilityUnType.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 53  | `makeNameTupleType(args)::CompileType` | BOUNDARY | builds NamedTuple type from heterogeneous typ.typ values. |
| 263 | `makeTupleType(tt::Vector{FlowNode})::CompileType` | BOUNDARY | builds Tuple type. |

## src/server/SimpleHTTPServer.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 46  | `query(f::QueryFunctor, method, body)::String` | GENUINE | HTTP query dispatch. |
| 66  | `handleQuery(f::QueryFunctor)` | GENUINE | server loop with abstract IO. |

## src/syntax/JSAdaptor.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 10  | `parseAndConstructHTML(raw, filename, outdir)` | GENUINE | HTML emission, lots of `print(io, ...)`. |

## src/syntax/SyntaxAdaptor.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 57  | `getData(tree)::JuliaSyntax.SyntaxData` | GENUINE | reaches into JuliaSyntax tree. |
| 65  | `getNodeVal(tree)::JuASTVal` | GENUINE | same. |
| 69  | `untokenize(head::JuliaSyntax.SyntaxHead)` | GENUINE | JuliaSyntax internals. |
| 132 | `getChildren(tree)` | GENUINE | JuliaSyntax internals. |
| 236 | `checkField(ast::JuAST, field::Symbol)::Bool` | LEGACY | predicate; checkable. |
| 273 | `parseJuAST(str, filename)::JuAST` | GENUINE | bridges JuliaSyntax parse. |

## src/syntax/SyntaxDefinition.jl

| line | function | category | note |
| --- | --- | --- | --- |
| 29  | `makeConstJuASTVal(val::Any)` | BOUNDARY | wraps arbitrary value as JuASTVal. |
| 37  | `getNodeVal(tree::JuliaSyntax.TreeNode{...})::JuASTVal` | GENUINE | JuliaSyntax internals. |
| 108 | `Base.show(io::IO, ast::JuAST)` | GENUINE | Base method on abstract IO. |
| 113 | `displayJuASTHelper(io, ast, id)` | GENUINE | recursive show helper on abstract IO. |

## Removed in this pass (kept here as a log)

- `toString(typ::CompileType)` (kept `@nocheck` but rewrote body via `_type_to_string`).
- `printErrorHead(eng, ast, errkind)` — converted to typed `writelog!`, removed `@nocheck`.
- `printWarningHead(eng, ast, errkind)` — same.
- `toIndex(v::Vector{Int})::String` — checkable, removed `@nocheck`.

## Next removal candidates

Highest payoff (LEGACY entries with small bodies, low blast radius):

1. `isPassthroughMacroName(name::Symbol)::Bool` — passthrough list check.
2. `macroIdentifierName(ast::JuAST)::Symbol` — AST field read.
3. `checkField(ast::JuAST, field::Symbol)::Bool` — same family.
4. `isCallableMethodDef(ast::JuAST)::Bool` — predicate.
5. `isBuiltinEqual(tt::CompileType)::Bool` — predicate.
6. `addFile!(ctx, mod, filename)` — reads file then parses.
7. `convert2ConstVal(val::JuASTVal)::CompileType` — coercion.

Each removal should come with a test exercising the function path.
