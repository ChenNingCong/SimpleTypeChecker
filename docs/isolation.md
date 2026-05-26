# Isolation / Runtime-context design

A living design doc. Captures the goal state, what's landed, and
what remains. Earlier brainstorming lives in `/isolation.md` (repo
root) — this file is the working spec.

## Why isolate

SimpleTypeChecker used to be *inseparable* from the live Julia JIT
runtime it was analyzing. Every reasoning step about a callee, a
type, or a method reached into `Core.*` of the live process:

- Method lookup (`methodsOf`, `inferReturnType`)
- Return-type inference for sub-calls
- `Core.eval` for evaluating type-level expressions during checking
- `Core.Typeof`, `Core.apply_type` for type construction
- `Core.Builtin` / `Core.IntrinsicFunction` isa-checks
- `getproperty(module, symbol)` for module-qualified name resolution

This coupling has three concrete costs:

1. **We can't trim the checker into a static binary** — the compiler
   adaptor pulls in the full Julia compiler stack.
2. **We can't run the checker against a snapshot of another world.**
   It only asks about *this* process's methods, *this* process's
   loaded modules.
3. **We can't substitute drivers.** Patching, tracing, mocking,
   record-and-replay all want a single seam to intercept.

## Target shape — two-runtime model

Conceptually, there are **two distinct Julia runtimes**:

- **Analyzer runtime** — where the inferencer's code runs. Owns
  `JuAST`, `FlowNode`, `Engine`, `Context`. Sees only primitives
  (`Bool`, `Int`, `Float64`, `String`, `Char`) and opaque handles.
- **Driver runtime** — where the user's program lives. Owns `Module`,
  `Function`, `DataType`, `Method`, `MethodInstance`, the entire
  type lattice. Reachable only through the `RuntimeContext` seam.

These two runtimes have **separate Type tables, separate Symbol
interning, separate object heaps**. A `Symbol` in one is not `===`
to a `Symbol` in the other. The analyzer's `Any` is not the
driver's `Any`. Values cannot freely cross.

### The membrane: `RuntimeContext`

`RuntimeContext` is a struct of typed `FunctionWrapper` fields —
every operation that reaches into the driver runtime goes through
one of them. `FunctionWrapper{Out, Tuple{In...}}` is a concrete
struct holding a C function pointer trampoline; calls dispatch
statically (no method-table lookup at the analyzer side, no
abstract callee).

```
struct RuntimeContext
    # method lookup
    methodsOf::MethodsOfFn
    inferReturnType::InferReturnTypeFn

    # type construction
    makeCallSig::MakeCallSigFn
    makeTupleType::MakeTupleTypeFn
    applyTypeOf::ApplyTypeOfFn
    makeTypeVarOf::MakeTypeVarOfFn
    makeUnionAllOf::MakeUnionAllOfFn
    makeUnionOf::MakeUnionOfFn
    splitUnionOf::SplitUnionOfFn

    # type-query
    subtypeOf::SubtypeOfFn
    isAny, isBool, isTypeValue, isUnion, isTupleType, isConcrete, isBottom

    # constant-inspection (no generic getConstVal)
    constIsTypeValue, constIsModule, constIsFunction, constIsBuiltinOrIntrin
    constEqualsBuiltinEq, constEqualsConst
    liftConstToType, getCallableFromConst

    # module accessors (Module stays opaque)
    constModuleIsDefined, constModuleGetField

    # field-shape
    hasFieldOf, getFieldTypeOf

    # composite type derivation
    makeMatrixOf, getFirstParameterOf, getTypeParameterOf
    removeConstOf, tupleDestructInfo

    # well-known type constants
    makeAnyType, makeBoolType, makeNothingType, makeExprType
    makeIntType, makeTypeType, makeFunctionType, makeBottomTypeOf

    # literal wrappers (primitives in, opaque handle out)
    wrapLiteralBool, wrapLiteralInt, wrapLiteralFloat64, wrapLiteralString
    wrapLiteralChar, wrapLiteralSymbolName, wrapLiteralNothing

    # specific well-known globals
    lookupBaseIdentity, lookupIsa, lookupBitAnd, lookupBitOr, lookupAnyValue

    # display ops (return String)
    typeToString, constToString, explainAbstractType
end
```

~55 fields total. Cluster discipline: every consumer of `.val` /
`.typ` peek gets a **specific** op (never a generic `getConstVal`).
The pressure to add ops is the right pressure — it surfaces every
hidden coupling.

### Opacity rule — what crosses the seam

Three kinds of values cross:

1. **Primitives** that have bit-identical representations both
   sides: `Bool`, `Int`, `Float64`, `Char`, `String`.
2. **Strings**, including those representing names that would be
   Symbols on the driver side. `Symbol` itself **does not cross** —
   the inferencer passes a `String`, the driver does `Symbol(s)` to
   re-intern into its own table.
3. **Opaque handles**, `OpaqueHandle{K<:HandleKind}`. A phantom-typed
   wrapper whose `.val::Any` carries a driver-runtime reference the
   analyzer cannot inspect.

```julia
abstract type HandleKind end
struct CompileTypeK        <: HandleKind end
struct CompileTypeVecK     <: HandleKind end
struct CompileTypePairVecK <: HandleKind end
struct CallableK           <: HandleKind end
struct CallSigK            <: HandleKind end
struct GlobalContextK      <: HandleKind end

struct OpaqueHandle{K<:HandleKind}
    val::Any
end
```

The phantom parameter `K` enforces kind-safety at compile time —
`OpaqueHandle{CompileTypeK}` ≠ `OpaqueHandle{CallableK}`. Julia
rejects passing one where the other is expected. `FunctionWrapper`
auto-`convert`s raw values into handles at the call boundary
(driver side), so call sites stay clean.

### What the analyzer must NOT do

These are violations of the membrane, even when they "work" today
because both runtimes happen to be the same process:

| Anti-pattern | Why wrong | Correct form |
|---|---|---|
| `makeType(Any)` | `Any` is analyzer's Any, not target's | `rtctx.makeAnyType()` |
| `makeConstVal(nothing)` | `nothing` is analyzer's `Nothing` instance | `rtctx.wrapLiteralNothing()` |
| `makeConstVal(Base.identity)` | references analyzer's `Base` | `rtctx.lookupBaseIdentity()` |
| `Matrix{ct.typ}` | parametric apply on `Matrix` peeks `.typ` | `rtctx.makeMatrixOf(ct)` |
| `t.parameters[i]` | peeks DataType internals | `rtctx.getTypeParameterOf(ct, i)` |
| `Symbol` argument at seam | symbols don't cross | `String` argument + driver `Symbol(s)` |
| `ct.val isa Type` | cracks `.val` | `rtctx.constIsTypeValue(ct)` |
| `repr(ct.val)` | analyzer-side `repr` | `rtctx.constToString(ct)` |

### What's true today

After Phases A/B/C (May 2026):

- **All ~30 `Core.*` touches outside `src/compiler/` are gone** —
  every reach into the driver runtime goes through `rtctx`.
- **No inferencer-side `makeType(X)` / `makeConstVal(v)` with
  analyzer-runtime literals.** All Type constants come from
  `rtctx.make*Type()`; all literal values come from
  `rtctx.wrapLiteral*()` or `rtctx.lookup*()`.
- **Symbols at the seam are Strings.** Five op signatures
  (`hasFieldOf`, `getFieldTypeOf`, `constModuleIsDefined`,
  `constModuleGetField`, `makeTypeVarOf`) use `String`. The
  inferencer does `String(p::Symbol)` at the boundary; the driver
  does `Symbol(s)` on its side.
- **`CompileType.val` / `.typ` never peeked from the inferencer.**
  Every consumer goes through a specific membrane op (e.g.
  `constIsModule`, `liftConstToType`, `typeToString`).
- **Display path routes through seam.** `toString(rtctx, ct)`,
  `printSignature(rtctx, ...)`, etc.

Self-check (the checker checking itself) is still passing with the
same set of pre-existing fires as before the refactor — no new
fires introduced. Tests: 82/82.

### What still leaks (known TODOs)

The following are tracked but not yet absorbed:

1. **`eng.mod::Core.Module`** — the user's target module flows
   through `Engine` as a raw `Core.Module` reference. Used by
   `getFromModule(mod, sym)` / `isModuleDefined(mod, sym)` (5 call
   sites). The `getproperty` / `isdefined` operations happen
   driver-side, but the Module reference itself crosses analyzer
   code. Should wrap as `OpaqueHandle{ModuleK}` or a const
   `CompileType` wrapping the Module value.
2. **`Inference.jl:920, 970`** — `JuExpr(Literal(makeConstVal(nothing)), ...)`
   inside `constructJuExpr`. Syntax-construction code that doesn't
   have `rtctx` in scope. Likely needs to plumb `rtctx` through
   `SyntaxBuildResult` or defer the wrap until inference time.
3. **`inferLambdaExpr`** — calls `Core.eval(eng.mod, lam_expr)`
   and wraps the returned function via `makeConstVal(fn)`. Genuinely
   driver-side action (creates a new method in the target module);
   should be moved into a dedicated membrane op.
4. **`fasteval`** — `@nocheck` recursive type-expression evaluator.
   Uses `getglobal`, `Core.apply_type`, `UnionAll`. Known
   self-check fire; refactor into membrane ops or split into a
   driver helper.
5. **`makeMacroCallFlowNode` etc.** — handful of FlowNode
   constructors still callable without rtctx; should require it
   uniformly.

## Implementation history

Three phases landed:

### Phase A — wire RuntimeContext through (Apr–May 2026)
- `RuntimeContext` struct, `OpaqueHandle{K}` phantom-types
- Field added to `GlobalContext`; default factory in CompilerAdaptor
- One call routed (`methodsOf`), MWE test injecting a mock rtctx

### Phase B — type-query / type-construction / display clusters
- Type-query: subtypeOf, isAny/isBool/isTypeValue/isUnion/isTupleType,
  isConcrete, isBottom, hasFieldOf, getFieldTypeOf (#43)
- Constant-inspection: constIsTypeValue/constIsModule/
  constIsFunction/constIsBuiltinOrIntrin, constEqualsBuiltinEq,
  constEqualsConst, liftConstToType, getCallableFromConst,
  constModuleIsDefined, constModuleGetField (#44)
- Type-construction: makeCallSig, makeTupleType, applyTypeOf,
  makeTypeVarOf, makeUnionAllOf, typeEq, makeUnionOf, splitUnionOf (#45)
- Display: typeToString, constToString, explainAbstractType (#46)

### Phase C — close the analyzer-runtime literal leak (May 2026)
- Type-constant ops: makeAnyType/makeBoolType/makeNothingType/
  makeExprType/makeIntType/makeTypeType/makeFunctionType/makeBottomTypeOf (#47)
- Literal-wrap ops: wrapLiteralBool/Int/Float64/String/Char/
  SymbolName/Nothing (#48)
- Symbol→String at the seam (5 ops migrated) (#49)
- Move makeType/makeConstVal/removeConst/makeMatrixType/
  getFirstParameter driver-side (#50)

## Open questions

- **Two-runtime FFI shape.** When the analyzer is genuinely
  sideloaded into a different JIT session, what's the wire
  format for `OpaqueHandle.val`? Today it's `::Any`; in the
  separated world it would be `UInt64` (an iddict key on the
  driver side) or a raw pointer. The phantom `K` type ensures
  we can change `val` without touching call sites.
- **Module references.** Should `Module` cross the seam as an
  `OpaqueHandle{ModuleK}` (uniform) or as a const `CompileType`
  (current ad-hoc convention)? The latter saves one handle kind
  but breaks the "Module is callable-like" symmetry.
- **`fasteval` / `Core.eval` paths.** These are genuinely
  driver-side work. Likely need a `rtctx.evalTypeExpr(mod_h, ast)`
  op that returns a wrapped CompileType — but `ast` is an analyzer-
  side `JuAST`. May need to serialize the JuAST as a String of
  source and reparse driver-side.
- **Mutable runtime context?** I.e. install hooks at runtime, or
  freeze at engine construction? Frozen is simpler; no use case
  for mutability yet.

## Deployment artifacts

Three artifact shapes, three roles. The membrane is what keeps the
analyzer's contract honest across all three — but only one of them
is the user-facing deployment.

| Artifact | How built | How consumed | Role |
|---|---|---|---|
| **Package image** | Automatic on `using SimpleTypeChecker` (Julia ≥ 1.9). Lives under `~/.julia/compiled/v1.X/SimpleTypeChecker/`. | Host Julia process loads it through normal `using`. | **Supported deployment** for the Julia-host case. Lightweight (~1–20 MB) because it's a *patch* on the host's sysimage, not a replacement. |
| **Custom sysimage** | `PackageCompiler.create_sysimage` (not currently wired). | `julia --sysimage=ours.so file.jl`. | Optional faster-startup deployment. Bigger (~150–300 MB) because it bakes a replacement sysimage. |
| **Trim `--output-lib`** | `bin/build_standalone.jl lib`. Produces `bin/build/libsimpletypechecker.so` with a baked minimal sysimage. | Non-Julia host (see `bin/c_host.c`). **Cannot** be dlopen'd from host Julia — sysimage identity conflict. | **Membrane CI gate**: validates the analyzer is closed over its declared deps. Not a deployment. |
| **Trim `--output-exe`** | `bin/build_standalone.jl exe`. Self-contained binary at `bin/build/sttc-check`. | `./sttc-check file.jl` from a shell. | Standalone-binary deployment for users who don't want a Julia runtime. |

### Why dlopen-of-trim-lib from host Julia is forbidden

Not a TLS issue and not a membrane violation — it's **sysimage identity**.

- Trim `--output-lib` bakes a minimal sysimage into the .so as static
  data. The compiled call sites contain absolute pointers into *that*
  sysimage (method-table addresses, type tags, const-pool entries).
- The host Julia process is running against the default sysimage,
  with different method-table addresses.
- libjulia is **one-sysimage-per-process by design**. Loading the
  trim .so means "use *this* sysimage now", which is incompatible
  with the host's already-installed sysimage. The visible symptom
  is an abort in the `@ccallable` thread-adopt prologue, but that's
  downstream of the sysimage conflict.

The membrane is a *source-level* boundary: it controls which symbols
the analyzer names. The sysimage coupling is a *binary-level* fact:
codegen bakes whatever sysimage it was built against. Two separate
concerns, two separate solutions.

Package images dodge this because they're *patches* on top of the
host's sysimage, not replacements. Trim `--output-lib` is a
replacement sysimage — standalone by design, that's the product.

### Why the membrane still matters in the package-image world

Even though the package-image path doesn't *need* the membrane to
load (it composes with the host sysimage automatically), the
membrane earns its keep through three angles:

1. **Driver swap**: live-JIT vs. mock vs. record-and-replay vs. a
   future cross-process driver. Without the seam, every test that
   wants to fake a method lookup has to monkey-patch globals.
2. **Testability in isolation**: the analyzer can be unit-tested
   with a fixture rtctx — no live Julia state required.
3. **Documentation by construction**: the rtctx struct is the
   exhaustive list of "what the analyzer needs from the world".
   Anything not in there can't be a hidden dependency.

The trim `--output-lib` artifact remains the strict pass/fail proof
that the membrane is tight, exercised through `bin/c_host.c`.

## Non-goals

- Not rewriting the inference algorithm — only re-routing its
  external calls.
- Not changing `JuAST` or the syntax adaptor shape (`Base.JuliaSyntax`
  stays as today).
- Not introducing a new package; this is internal restructuring.
- Not breaking `checkFile` / `checkDirectory` user surface — those
  keep working with the default (live-JIT) driver.

## Anchors / related docs

- `/isolation.md` (root) — original brainstorming.
- `docs/nocheck_inventory.md` — which functions are at the adaptor
  seam (GENUINE-keep) vs. which leak past it.
- `docs/untrimmable_calls.md` — trim-verifier's view.
- `feedback_adaptor_interface` (memory) — original "nothing outside
  CompilerAdaptor touches Core.Compiler" rule, now broadened to
  "nothing outside CompilerAdaptor touches the driver runtime."
- `feedback_opaque_types_at_membrane` (memory) — CompileType /
  MethodDef / JuAST stay opaque across rtctx.
