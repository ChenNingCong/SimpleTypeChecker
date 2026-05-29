# TypeScript's checker as interprocedural sparse backward value flow

A presentation of TypeScript's type-checking algorithm at a level of
abstraction useful for designing SimpleTypeChecker's strict-mode and
incremental-analysis strategy. Drops the syntactic and stdlib detail;
keeps the essential demand-driven shape.

## Frame

TypeScript's checker does **not** abstractly interpret every function
body eagerly the way a forward data-flow pass would. It is structured
as a **memoized recursive type query** over a sparse program graph:

- Every meaningful question — *"what is the type of node N?"*, *"what
  is the return type of function F?"* — is a query.
- Queries are answered lazily: the checker only computes what some
  outer query demands.
- Each query is keyed on a stable program object (a `Symbol`) and its
  result is cached. Re-asking is free.
- Recursion through the call graph is finite because cycles are
  detected and broken with a placeholder type.

The "interprocedural sparse backward value flow" framing names what
this is mathematically:

- **Interprocedural** — queries cross function boundaries via call
  sites and return-type lookups.
- **Sparse** — only the subset of the program graph that influences
  the current query is touched.
- **Backward** — the query originates at the use site (e.g. a call) and
  walks back to the definition site to retrieve the answer.
- **Value flow** — the answers being computed are types (i.e. abstract
  values), and they flow along def-use edges, not along the AST in
  reading order.

## Core data structures

```
Symbol     — a stable handle for a named declaration (variable, function,
             class, parameter). One Symbol per declaration; multiple
             AST nodes can share a Symbol (e.g. merged declarations).

Type       — the result of type checking; immutable, structural.

TypeCache  — Symbol → Type (set once, looked up many times).
ReturnCache— Symbol(function) → Type (return type, set after first
             demand of the function's return).

Resolving  — Set of Symbols currently being computed; used to break
             cycles by returning a placeholder if the same Symbol is
             re-entered.
```

The cache keys are `Symbol`s, not AST nodes. Two `x` in different
scopes are different Symbols; one `x` referenced ten times is one
Symbol. This is what makes the caching tight.

## The main query loop

The whole checker is, conceptually, this loop:

```
getTypeOfNode(node):
    if node has a Symbol s:
        return getTypeOfSymbol(s)

    case node of
        Literal l:           return literalType(l)            -- "5" -> 5, "x" -> "x"
        Identifier id:       s = resolveName(id)
                             return getTypeOfSymbol(s)
        Call c:              return checkCall(c)
        BinaryOp b:          return checkBinary(b)
        ArrowFunction f:     return getTypeOfFunction(f)
        ...
```

`getTypeOfSymbol` is the memoization gate:

```
getTypeOfSymbol(s):
    if TypeCache has s:
        return TypeCache[s]

    if s in Resolving:
        return anyType                          -- cycle break, fixed up later

    Resolving.add(s)
    try:
        t = computeTypeOfSymbol(s)              -- the actual work
        TypeCache[s] = t
        return t
    finally:
        Resolving.remove(s)
```

`computeTypeOfSymbol` dispatches by declaration kind:

```
computeTypeOfSymbol(s):
    decl = s.declaration
    case decl of
        VariableDecl{ init }:
            if decl has annotation T:
                return T
            else:
                return getTypeOfNode(init)              -- demand the initializer

        ParameterDecl{ name, annotation }:
            if annotation:
                return annotation
            else:
                return getContextualType(decl)          -- backward inference!

        FunctionDecl f:
            return getTypeOfFunction(f)                 -- signature, not body

        ImportedSymbol s':
            return getTypeOfSymbol(s')                  -- chase the import
```

This is the **sparse, backward, demand-driven** shape:
- We don't walk every node of a file.
- We start at the node the outer query cares about and walk *back* along
  def-use edges only as far as needed.
- Annotated declarations short-circuit immediately — no body walk.

## Function types: signatures, not bodies

The single biggest reason TypeScript scales is that **a function's
type is its signature, not its body**. Bodies are checked for their own
contracts; their *return type* is read from the signature when known
and inferred otherwise.

```
getTypeOfFunction(f):
    params = [getTypeOfSymbol(p.symbol) for p in f.parameters]
    if f has return annotation R:
        ret = R
    else:
        ret = inferReturnTypeFromBody(f)        -- memoized; computed once
    return FunctionType(params, ret)


inferReturnTypeFromBody(f):
    if ReturnCache has f.symbol: return ReturnCache[f.symbol]
    if f.symbol in Resolving:    return anyType          -- recursive function

    Resolving.add(f.symbol)
    try:
        returnTypes = []
        for stmt in walkReturnStatements(f.body):
            returnTypes.append(getTypeOfNode(stmt.expr))
        ret = union(returnTypes)
        ReturnCache[f.symbol] = ret
        return ret
    finally:
        Resolving.remove(f.symbol)
```

Notice: `walkReturnStatements` walks the body looking only for `return`
statements. Other statements are not eagerly type-checked at this phase
— they are only checked when something demands them (e.g. when an outer
query asks "what does this variable resolve to?"). Side-effect-only
statements may never be visited at all if no query needs them.

A separate pass — `checkSourceFile` — eventually visits every node to
fire diagnostics. But the *type computation* engine is fully lazy;
`checkSourceFile` is the driver that demands every meaningful query.

## Call sites are where the call graph is crossed

```
checkCall(c):
    calleeType = getTypeOfNode(c.callee)        -- this may recurse far
    sig = getCallSignature(calleeType, c.arguments)
    for (param, arg) in zip(sig.params, c.arguments):
        argType = getTypeOfNode(arg)
        if not isAssignable(argType, param):
            emit Diagnostic(c, "argument type mismatch")
    return sig.returnType                       -- cached on the function's symbol
```

The recursion into the callee is implicit and bounded:
- If the callee is a local function we've never visited, its
  `getTypeOfFunction` runs, which may demand its body's return statements,
  which may demand more callees, and so on.
- Every callee's signature is memoized per `Symbol` — visited at most
  once per program.

The transitive cost is therefore **O(reachable function symbols ×
average body return-statement work)**, not O(call sites). This is the
sparseness guarantee.

## Contextual typing: forward demand, backward inference

Consider:

```ts
[1, 2, 3].map(x => x + 1)
```

The parameter `x` has no annotation. How does the checker know `x: number`?

```
getContextualType(parameterDecl x of arrow f):
    -- f appears as argument 0 of a call to .map(callback)
    parentCall = parentCallExpression(f)
    targetSig  = getCallSignature(getTypeOfNode(parentCall.callee), ...)
    return targetSig.params[argIndexOf(f)].params[positionOf(x)]
    -- i.e. the type that .map expects its callback's first parameter to have
```

This is a **forward demand creating a backward flow**:
- The query "what is the type of `x`?" originates at the use of `x`
  inside the arrow body.
- The checker, finding no annotation, walks **forward** in the AST to
  the call site that supplies the arrow as an argument.
- It then pulls the parameter type **backward** through the call's
  signature into the arrow's parameter slot.

The TypeScript term is "contextual typing." The graph-theoretic name
is: a backward edge in the type-flow graph whose direction is opposite
to the AST's parent edge. Generics extend this further — type parameters
are *inferred* by unifying argument types with parameter types at the
call site, then propagating the binding back into the callee's return.

## Cycle handling

```
foo(x) { return bar(x); }
bar(y) { return foo(y); }
```

Mutually recursive without annotations. The checker handles this by
the `Resolving` set:

```
inferReturnTypeFromBody(foo):
    Resolving = {foo}
    visit return bar(x):
        getTypeOfNode(bar(x)):
            checkCall: getTypeOfNode(bar):
                getTypeOfFunction(bar):
                    inferReturnTypeFromBody(bar):
                        Resolving = {foo, bar}
                        visit return foo(y):
                            getTypeOfNode(foo(y)):
                                checkCall: getTypeOfNode(foo):
                                    getTypeOfFunction(foo):
                                        inferReturnTypeFromBody(foo):
                                            foo in Resolving → return any
                                        ret(foo) = any (placeholder)
                            -> bar(y): any
                        ret(bar) = any
                -> foo(x): any
    ret(foo) = any
```

After the cycle, both functions are typed as `any`. TypeScript does
**not** iterate to a true fixpoint over the SCC — it accepts the
imprecise `any` and the user is expected to annotate one of them to
break the cycle. This is a pragmatic choice: real fixpoint iteration
costs more than it pays in the typical case, and annotations are cheap.

## Putting it together — the program-level shape

```
checkProgram(files):
    bind(files)                                  -- AST -> Symbol table; one pass
    for file in files:
        checkSourceFile(file)                    -- the diagnostic-emitting pass

checkSourceFile(f):
    for each statement s in f:
        checkStatement(s)                        -- demands queries via getTypeOfNode

checkStatement(s):
    -- e.g. for `let x: T = init;`:
    declType = getTypeOfSymbol(s.x.symbol)       -- triggers init evaluation
    initType = getTypeOfNode(s.init)
    if not isAssignable(initType, declType):
        emit Diagnostic
```

`checkSourceFile` is the driver that demands every query *for diagnostic
purposes*. The type-computation engine underneath it (`getTypeOfNode`,
`getTypeOfSymbol`, `getTypeOfFunction`) is fully lazy and memoized; it
will be re-entered many times by `checkSourceFile` across different
nodes but each `Symbol` is computed exactly once.

This is the asymmetry worth internalizing:

- **`checkSourceFile`** is a forward, eager walk over the AST. It exists
  to fire diagnostics on every node a user might care about. O(AST size).
- **`getTypeOfSymbol` / `inferReturnTypeFromBody`** is a lazy, memoized,
  demand-driven recursion through the program graph. O(reachable
  declarations).

The diagnostic pass is forward and dense; the type-computation pass
underneath it is backward and sparse. Most of the engineering complexity
lives in the latter.

## What this implies for SimpleTypeChecker

Mapping back to our code:

| TypeScript                              | SimpleTypeChecker                              |
|-----------------------------------------|------------------------------------------------|
| `Symbol`                                | `MethodInstance` handle (opaque, via rtctx)    |
| `TypeCache: Symbol -> Type`             | `GlobalContext.cache: Any -> Vector{CompileType}` |
| `ReturnCache`                           | same `cache` — return type slot                |
| `getTypeOfNode`                         | `inferExpr`                                    |
| `getTypeOfFunction` (signature build)   | the body of `inferReturnType` (currently delegates to Core.Compiler) |
| `inferReturnTypeFromBody`               | does **not exist** for our analyzer's strict checks — this is the missing piece |
| `checkSourceFile`                       | `runCheck!` over `queue::Vector{KwFunCall}`    |
| `Resolving`                             | does **not exist** — we'd need it for recursive strict analysis |

The diagnostic-emitting outer pass (`runCheck!`) already exists. The
type-computation inner engine exists *only via delegation to Core.Compiler*
for return-type queries. For our strict checks to run transitively, we
need a SimpleTypeChecker-native `inferReturnTypeFromBody`-equivalent
that:

1. Takes a `MethodInstance` handle (the cache key).
2. Checks the cache; returns immediately if hit.
3. Checks `Resolving`; returns a placeholder (e.g. `Any` + "no strict
   findings yet") if cycle.
4. Walks the method body, recursing into callees via the same function.
5. Stores both the return type and the strict findings on the cache.

The membrane is irrelevant to this design — it's a transport boundary,
not an analysis boundary. The analysis structure lives entirely on the
analyzer side of the membrane; the membrane only mediates how we *read*
opaque handles when we need to (e.g. asking rtctx "what's the
MethodInstance for this call?").

The interesting design choice we'd inherit from TypeScript: **no
fixpoint over SCCs.** When the cycle break returns the placeholder, we
either accept the imprecision or require the user to break the cycle
manually (e.g. by `@nocheck` or by adding a type assertion). The
pragmatic argument for this choice — that real fixpoint iteration
rarely pays off — is worth re-examining in the Julia setting, but the
default should probably mirror TypeScript's: accept the placeholder.

## What this does **not** address

- **Narrowing / control-flow analysis.** TypeScript also runs a CFG
  pass that refines union types along branches (`if (typeof x ===
  'string')` narrows `x: string | number` to `x: string` inside the
  block). This is orthogonal to the demand-driven structure above —
  it's a *local* refinement on top of the global type query engine.
  SimpleTypeChecker has its own scope/flow analysis; the two layers
  compose without conflict.
- **Generic type inference.** Type parameters get inferred at call
  sites by unifying argument types with parameter types. This adds a
  constraint-solving step on top of the queries above but does not
  change their structure.
- **Incremental recheck.** TypeScript's incremental mode invalidates
  cache entries by tracking which `Symbol`s' types depend on which
  declarations. That dependency graph is recorded during the initial
  query traversal — every time `getTypeOfSymbol(a)` reads
  `getTypeOfSymbol(b)`, a dep edge `b -> a` is recorded. On edit, the
  affected `Symbol`'s cache entry is dropped along with everything
  transitively downstream.
