# Legacy experiment files

There are four source files in `src/adaptor/` that are NOT loaded by `toplevel.jl` and therefore not part of the active build. They are **not dead code** — they are from an earlier design generation that may be brought back. This document records what each was exploring, so the work can be picked up later without re-reverse-engineering it.

If you are doing a folder reorg, **preserve these files** (e.g. under `src/experimental/` or `src/archive/`) rather than deleting them.

## The earlier design — at a glance

The active codebase represents code as a single uniform `JuAST` node (head-symbol + untyped args, see `src/adaptor/SyntaxDefinition.jl`), and the inference algorithm walks that AST directly. The legacy files together sketch a **different approach**: parse into a *typed* AST where each construct has its own struct (`Assign`, `IfStmt`, `FunCall`, ...), and run inference over that.

Pros of the typed-AST direction (why it might come back):
- Precise structural positions — useful for IDE/LSP features like hover, jump-to-definition, "what slot of the parent does this expression occupy" (this is exactly what `DerivedExpr.jl` enables).
- Type-safe pattern matching in the inferencer instead of head-symbol dispatch.

Cons (why it was set aside):
- Two parallel representations (`JuAST` + typed AST) require a faithful conversion + validation pass (`JuExprValidator.jl`).
- More boilerplate per AST node kind.

## Per-file summary

### `src/adaptor/type.jl` — typed-AST core (279 lines)

The earlier representation. Defines:

- `JuVal` — wrapper for any Julia value (`val::Any`, `isPoison::Bool`, `isConst::Bool`). Distinguishes constant vs non-constant vs poison values.
- `JuType` — wrapper for any Julia type with poison flag.
- `JuExpr` — a single-field wrapper (`val::Any`) holding one of the typed AST node structs below.
- A family of typed AST node structs: `Var`, `Assign`, `TypedAssign`, `FunCall`, `CurlyCall`, `Literal`, `Block`, `IfStmt`, `WhileStmt`, `ForStmt`, `Return`, `GetProperty`, `SetProperty`, `ArrayRef`, `ArraySet`, `TypedAssert`, `FunDef`, `ModDef`. Each carries an `ast::JuAST` back-pointer to the underlying syntax node.
- A `FlowNodeKind` enum and `FlowNode` struct — flow-graph nodes used during inference (currently the active `InferenceDefinition.jl` has its own `FlowNode` that evolved from this).
- Constructors `makeLiteralFlowNode`, `makeAssignFlowNode`, ... that build flow nodes for each typed AST kind.
- A simpler `Engine` / `Context` / `InferResult` / `InferReport` than the current code.

The active code in `InferenceDefinition.jl` re-implements some of these (`Context`, `Engine`, `FlowNode`, `InferResult`, `InferReport`) but against `JuAST` instead of typed AST. If the typed-AST direction is revived, `type.jl` is the starting point.

### `src/adaptor/DerivedExpr.jl` — structural position markers (83 lines)

A collection of small wrapper structs (`NoDerivedExpr`, `DerivedExprKwArg`, `DerivedExprKwArgLHS`, `DerivedExprParameter`, `DerivedExprImplicitKwArg`, `DerivedExprModuleName`, `DerivedExprForVar`, `DerivedExprField`, `DerivedExprAssignLHS`, `DerivedExprFunName`, `DerivedExprFunParam`, `DerivedExprFunParamList`, `DerivedExprReturnType`, `DerivedExprFunDefArg`, `DerivedExprFunDefArgPos`, `DerivedExprFunSignature`, `DerivedExprArrayRef`, `DerivedExprArraySet`, `DerivedExprForAssign`, `DerivedExprElseIf`).

Each one wraps a parent `JuExpr` (and sometimes a `Symbol` or `Int` index) to identify "which structural slot of the parent this sub-expression occupies." For example, `DerivedExprFunParam(parent, var)` marks a particular keyword parameter slot inside a `FunDef`.

This is the substrate for **IDE / source-mapping features** — given a cursor position in the source, you can navigate up the typed-AST tree and produce a precise structural description of where you are ("keyword argument `scale` of the function definition starting at line 12"). Useful for hover info, jump-to-definition, and error messages that reference structural positions.

### `src/adaptor/JuExprValidator.jl` — typed-AST consistency check (44 lines)

A debug utility. Walks a constructed `JuExpr` and the AST-to-typed-AST source mapping (`SourceMapping`, `ConstructJuExprResult`) and verifies that the typed representation faithfully represents the underlying `JuAST` with no information loss.

The header comment says it directly: "It valids whether a JuExpr and its mapping faithfully represents a JuAST with no information loss." Recursively descends through `JuAST` and checks every non-trivial node is present in `map.ast2exMapping`, skipping `:quote` and `:macrocall` (which were intentionally not converted).

Only meaningful while the typed-AST representation exists — pair with `type.jl` and `DerivedExpr.jl` if reviving.

### `src/adaptor/Scope.jl` — scope-discipline algorithm sketch (324 lines)

Mostly comments + partial implementation sketches. Documents the **scope problem** in Julia:

- Variables assigned in `for` bodies leak into the surrounding function scope (not the loop).
- `if` blocks are not scoped — a `local x = 1` inside an `if` branch can be reassigned after the `if`.
- A variable can be declared multiple times.

The file proposes a **restriction set** that makes inference tractable:

1. A variable can be declared at most once.
2. Any assignment to a variable is dominated by its declaration.
3. Declarations inside an `if`-clause have only-that-branch scope (otherwise the user should promote the declaration outside the `if`).

Then sketches a walker-style algorithm (`walkMark`, `walkLiteral`, `walkAssign`, `walkJuExpr`, `analyzeScope`) over the typed AST that enforces these rules. Several functions are incomplete / have syntax errors (e.g. `error("Unmarked)` on a comment-only line) — clearly design-stage notes, not running code.

The currently-active `src/adaptor/ScopeChecking.jl` is the descendant of this design but operates on `JuAST` directly. Many of the same restrictions are enforced there. If the typed-AST direction is revived, `Scope.jl` is the design document for what the scope pass should do.

## Bringing these back

If you want to revive the typed-AST line of work:

1. Decide whether the active `JuAST` representation stays as the parsing layer (probably yes — it's well-integrated with JuliaSyntax) and the typed AST sits *on top* as a second layer constructed by an `addFile!`-like pass.
2. Restore `type.jl` first — it defines the core types. Will need imports updated for the current module layout (e.g. `using ..SyntaxDefinition`, `using ..Utility`).
3. Restore `DerivedExpr.jl` — pure data definitions, should be drop-in.
4. Write the `JuAST → JuExpr` construction pass (this code does not currently exist outside design notes).
5. Restore `JuExprValidator.jl` to verify the construction pass is lossless.
6. Port the inference walker from typed-AST-aware functions in the legacy `Scope.jl` (mostly a complete rewrite — the legacy code is sketch-quality).

Don't expect any of these files to compile as-is. They were carved out of a different module layout.
