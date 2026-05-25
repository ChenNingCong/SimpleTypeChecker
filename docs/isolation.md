# Isolation / Runtime-context design

A planning doc — not a spec. Captures the target state and what
work it implies, so future passes can be staged against it.

Originates from the rough proposal in `/isolation.md` at the repo root.
This file refines that into a concrete target and a checklist of
current coupling points.

## Why isolate

Today SimpleTypeChecker is *inseparable* from the Julia JIT runtime
it's analyzing. Every reasoning step about a callee, a type, or a
method reaches into `Core.*` of the live process:

- Method lookup (`methodsOf`, `inferReturnType`)
- Type inference for return types of recursive / sub-calls
- `Core.eval` for evaluating type-level expressions during checking
- `Core.Typeof`, `Core.apply_type` for type construction
- `Core.Builtin`, `Core.IntrinsicFunction` for callee categorization
- `getproperty(module, symbol)` for resolving module-qualified names

This coupling has three concrete costs:

1. **We can't trim the checker into a static binary.** The compiler
   adaptor seam pulls in the full Julia compiler stack — which is
   the whole reason `CompilerAdaptor.jl` is `@nocheck` and `untrimmable_calls.md`
   exists. Even the work to migrate to `Base.JuliaSyntax` (Phase 11)
   only closed the *parser* side of this — the checker proper is
   still JIT-tied.
2. **We can't run the checker against a snapshot of another world.**
   We're always asking about *this* process's methods, *this*
   process's loaded modules. There's no way to point the checker
   at a frozen view (e.g. for offline CI, or for analyzing code
   without loading it).
3. **We can't substitute drivers.** Patching, tracing, mocking,
   record-and-replay — every one of these wants a seam to intercept
   the "ask the runtime" call. There isn't one today; the calls
   are inlined throughout `Inference.jl`.

## Target shape

Introduce a `RuntimeContext`: an opaque value, passed explicitly,
carrying a vtable of function pointers (or Julia callable struct,
or C function pointers — TBD) for every operation that today
reaches into the live runtime.

(Named `RuntimeContext` rather than `Context` because the inference
side already has its own `Context` for variable bindings. Casual
references in prose use "runtime context" lowercase.)

```
run_inference(program_text_collection, rtctx)
    -> construct engine (carries `rtctx`)
    -> parse text into AST (via rtctx.parse, OR directly if
       JuliaSyntax stays compile-friendly — open question)
    -> Run type inference, asking the runtime context any time
       we'd normally reach into Core.*:
         rtctx.infer_return_type(sig)
         rtctx.methods_of(f)
         rtctx.method_codeinfo(m)
         rtctx.eval_type_expr(mod, expr)
         rtctx.get_property(mod, sym)
         rtctx.is_builtin(v)
         ...
```

A driver is whatever fills in the runtime context. The default
driver delegates to the live JIT exactly as today. Alternative
drivers can mock, trace, record, or load from a snapshot file.

**Design constraint (load-bearing):** after this rewrite, *the
inferencer core itself must be trim-compilable*. The runtime
context is the **only** place dynamic dispatch is allowed. From
the inferencer's standpoint every `rtctx.foo(...)` call is a
static dispatch (typed `FunctionWrapper` field → ccall trampoline);
any genuine open-world dispatch happens *inside* the driver, behind
that trampoline, where it doesn't pull anything into the
inferencer's compilation unit. This is what makes the seam
load-bearing: it's not just a refactor for clarity, it's the
boundary that splits "compilable" from "not." Field types are
therefore concrete `FunctionWrapper{Out, Tuple{In...}}` aliases,
never bare `Function` or `Any`.

Two benefits the user called out, restated in our vocabulary:

1. **Static control over what we compile.** Once every runtime
   touch goes through the runtime context, the checker's own
   code can be statically analyzed (and trimmed) without dragging
   in the compiler. The runtime context is the only place that
   needs the compiler — and we keep it small enough to vendor
   or hand-port.
2. **Drivers are cheap.** Tracing driver = wrap the default and
   log each call. Snapshot driver = read from a file. Test driver
   = answer from a hand-built table. None of these need touching
   the checker.

## What's already in good shape

The refactor that landed in May 2026 (`project_refactor_plan`)
already put most of the runtime touches behind two adaptor seams:

- **`src/syntax/`** — JuliaSyntax wrapper. After the Phase 11
  migration to `Base.JuliaSyntax`, this is purely textual: text
  in, our own `JuAST` out. No `Core.*` reachable through this
  surface. **This part of the runtime context is already conceptually
  free** — once `Base.JuliaSyntax` is itself trim-friendly
  (close, but not quite — see `patch.jl`), the parser can ride
  as-is.
- **`src/compiler/`** — `CompilerAdaptor.jl` + `CompilerAdaptorTypes.jl`.
  Every `@nocheck` in the codebase that comes from Core.Compiler
  poking lives here. The functions in this file *are* essentially
  the runtime-context vtable already, just inlined as direct calls.
  See `docs/nocheck_inventory.md` — 12 of the 27 surviving
  `@nocheck` markers are in this one file, by design.

So the seam isn't theoretical — it already exists. What's missing
is making it *explicit* (an argument passed around, not a fixed
import) and making it *swappable* (the body of each function is
"talk to the JIT" today; needs to become "talk to whatever the
caller plugged in").

## What still leaks past the seam

Quick grep for `Core.*` references outside `src/compiler/` (this
is what would need to be tightened or routed through the runtime
context):

| file | what leaks | how to absorb |
| --- | --- | --- |
| `inference/UtilityUnType.jl` | `Core.kwcall`, `Core.apply_type` (4 sites) | wrap as `rtctx.apply_type` / `rtctx.kwcall_type` |
| `inference/Inference.jl:2610,2945,2961` | `Core.apply_type`, `Core.Typeof` | same — these are type-construction calls |
| `inference/Inference.jl:570` | `Core.Builtin`, `Core.IntrinsicFunction` isa-checks | `rtctx.is_builtin(v)` |
| `inference/InferenceError.jl:483` | `Core.:(===)` identity | route through Builtin table |
| `inference/InferenceDefinition.jl:22` | `Core.Typeof` for constant typing | `rtctx.typeof_const` |

None of these are deep refactors individually — each is a one- or
two-line wrap. The discipline is the same one we already enforce
for `Core.Compiler.*` (cf. `feedback_adaptor_interface`): the
checker proper shouldn't name `Core.*` directly; that's the
adaptor's job.

Plus the `Core.eval` site (`fasteval` at Inference.jl:2491) and
the `getproperty` site (`evalGetPropertyChain` at Inference.jl:2786)
— these are the two big ones, since they're literally "evaluate
arbitrary code in a live module." Both are already `@nocheck`'d
and live in the GENUINE bucket of `nocheck_inventory.md`. They
become `rtctx.eval_in(mod, expr)` and `rtctx.get_property(mod, sym)`.

## Phased plan

Matches the order the user proposed in `/isolation.md` §25–28,
adapted to what's already done.

### Phase A — wire the runtime-context argument through (no behavior change)

Goal: prove the plumbing. The default runtime context is still
"call into Core.* directly", same as today. But the *path* is
now `eng.rtctx.thing(args)` instead of `thing(args)`.

Steps:

1. Define a `RuntimeContext` struct (in `src/compiler/CompilerAdaptorTypes.jl`,
   or a new `src/compiler/RuntimeContext.jl`) with fields for each
   adaptor operation. Initial version uses Julia callables — defer
   the C-pointer question.
2. Add a `rtctx::RuntimeContext` field on `GlobalContext` (or wherever
   the engine state lives).
3. Pick **one** simple adaptor call (say `methodsOf`) and route it
   through `rtctx`. Add a test that constructs a `GlobalContext`
   with a mocked `methodsOf` returning a fixed `Vector{MethodDef}`,
   and verifies inference uses it.
4. Run the existing suite — should still be 80/80 since the default
   `RuntimeContext` is the existing implementation.

This phase is small enough to land in one commit. Exit criterion:
`RuntimeContext` is a first-class argument, threaded everywhere it
needs to go, but only one call site uses it.

### Phase B — relocate every Core.* touch onto the runtime context

Goal: when `git grep 'Core\\.'` is run outside `src/compiler/`,
nothing comes back.

For each row in the leak table above, add a `RuntimeContext` method,
move the call, replace the `Core.*` reference. Each row is a small
commit; the test suite gates each one.

Probably ~10 commits. No new tests strictly required (the existing
suite covers the call paths), but a test exercising the mocked
runtime context lands once and gets reused.

### Phase C — split the runtime context from the checker

Goal: build the checker without the default driver. The default
driver lives in a separate file (or package) and is what a
`SimpleTypeChecker.jl` user explicitly imports.

This is mostly a packaging change — moving `CompilerAdaptor.jl`'s
*implementation* (not its types) into a separate include path.
The checker now reads `RuntimeContext` types from `CompilerAdaptorTypes.jl`
but the bodies of `methodsOf`, `inferReturnType`, etc. are
elsewhere and don't get compiled when the checker is built
without a driver.

This is where the trim win materializes — the checker's static
analysis no longer has any reason to pull in Core.Compiler.

### Phase D (speculative) — C-pointer runtime context

Goal: drivers in other languages, or drivers in a separate process.
This is the runtime-context-as-vtable-of-C-fptrs version. Out of
scope until A–C are done; the design pressure isn't there yet,
and we might find a pure-Julia runtime context is enough.

## Open questions

- **Does JuliaSyntax need to go through the runtime context?**
  The argument for: keeps a single seam. The argument against:
  parsing is already trim-friendly post-Phase 11 (modulo `patch.jl`),
  so there's no compile-coupling cost to leaving it as a direct
  import. Tentative answer: leave it direct, treat the parser as
  conceptually part of the checker.
- **What's the granularity of the runtime context?** Coarse (one
  method per "ask the runtime a question") is easier to wire;
  fine (one per primitive op) is easier to mock. Probably
  coarse-first, refactor if mocking becomes painful.
- **Does the runtime context need to be mutable?** I.e. can a
  driver install hooks at runtime, or is it frozen at engine
  construction? Frozen-at-construction is simpler; we don't have
  a use case for mutability yet.
- **How does the runtime context interact with `addFile!` /
  `mapAST2Method`?** Those bridge AST→Method by querying the live
  module. They're marked LEGACY-?? in the inventory. They'd
  naturally become runtime-context calls — but the "what module
  is this AST attached to" question is genuinely an inference-side
  concern, not a runtime one. Worth thinking through before
  Phase B touches them.
- **Where do recursive-call return types come from in a mocked
  driver?** A test driver that has no JIT can't do inference.
  Either (a) the driver fakes return-type queries from a hand
  table, (b) the driver routes back into the checker recursively,
  or (c) we accept that some drivers won't support all queries
  and the checker handles `unsupported` gracefully. (c) might
  be the right answer for record-replay specifically.

## Non-goals (explicit)

- Not rewriting the inference algorithm — only re-routing its
  calls.
- Not changing `JuAST` or the syntax adaptor shape.
- Not introducing a new package; this is internal restructuring.
- Not breaking the existing `checkFile` / `checkDirectory` user
  surface — those keep working with the default (JIT) runtime context.

## Anchors / related docs

- `/isolation.md` (root) — the original brainstorming this
  refines.
- `docs/nocheck_inventory.md` — which functions are already
  acting as adaptor-seam (12 in `CompilerAdaptor.jl` are
  GENUINE-keep) vs which leak past it.
- `docs/untrimmable_calls.md` — the trim verifier's view of
  what still pulls in Core.Compiler.
- `feedback_adaptor_interface` (memory) — the existing rule
  that nothing outside `CompilerAdaptor.jl` touches `Core.Compiler.*`.
  This doc is "extend the same discipline to `Core.*` writ large."
