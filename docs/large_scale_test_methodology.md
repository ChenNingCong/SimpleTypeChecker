# Large-Scale Testing Methodology — Prior Art

Step-1 deliverable for `large_scale_test.md`: how do other Julia static-analysis efforts pick and run their corpus, and what should we borrow / diverge from?

## Primary reference: Pelenitsyn et al., OOPSLA 2021

**"Type Stability in Julia: Avoiding Performance Pathologies in JIT Compilation"** — Pelenitsyn, Belyakova, Chung, Tate, Vitek. The largest published Julia-corpus study to date.

Paper PDF: https://arxiv.org/pdf/2109.01950 · Artifact: https://github.com/prl-julia/julia-type-stability

### Corpus selection

| Step | Choice | Number |
|---|---|---|
| Source | Julia General registry (the only public registry at the time) | ~5500 packages (early 2021) |
| Initial filter | Top by GitHub stars | 1000 |
| Viability filter | Test suite must succeed on Julia 1.5.4 | **760 final** |
| Deep-dive subset | Top 10 starred, manual inspection | 10 |

Failures explaining 1000 → 760: missing dependencies, no test suite, timeout.

### Analysis loop (per package)

1. Install package via Pkg.
2. **Run the test suite** to completion. (Their analysis is *dynamic* — they need the JIT to populate the method-instance table.)
3. Query `Base.specializations(...)` for every compiled method instance.
4. Discard instances whose method belongs to a stdlib module (`Base`, `Core`, etc.) to avoid stdlib dominance.
5. For each remaining instance, re-run Julia's inferencer and inspect register types.
6. Record stability + groundedness + per-method metrics (size, gotos, returns, varargs, `@nospecialize`, polymorphism count).

Inference failures themselves: "**at most 5 failures per package**" — very rare.

### What they did NOT filter

- **Python wrappers**: no special handling. PyCall-shaped packages were left in.
- **C wrappers**: same — no special handling.
- **GPU packages**: same. Knet (GPU-heavy) ended up as the major **outlier** (16% stable vs. 74% mean) precisely because its work happens behind the CUDA membrane. Lesson: GPU code distorts results without being excluded; either bucket-separately or filter.
- **Package extensions**: didn't exist in 1.5.4 — pre-1.9.

### What they did do for noise control

- Discarded packages that *fail to test* (broad failure filter — coarse but cheap).
- Stripped stdlib instances post-hoc.
- For aggregate stats, used median alongside mean (Knet-style outliers).
- Manual inspection on the top 10, including git history and README sentiment ("does the project talk about performance?").

## Secondary reference: Rahman & Pandita, EMSE 2023

**"An Empirical Study of Defects in Julia Programs."** Different goal (defect taxonomy from bug reports) but their **repo selection criteria** are useful.

PDF: https://akondrahman.github.io/files/papers/emse2023-julia.pdf

Their criteria for "professional" Julia repos (intersected on top of GitHub Search for Julia code):

1. ≥10 % of files are `.jl`
2. Repo downloadable
3. Not a fork
4. ≥2 commits/month average
5. ≥10 contributors
6. ≥10 issue reports

Their corpus source was **GitHub Search**, not the registry — useful if we ever want to extend beyond registered packages. Both papers landed on GitHub stars + activity as proxies for "worth analyzing."

## Implications for our static analyzer

We're a **static** analyzer, not dynamic, so the playbook diverges from Pelenitsyn:

| Concern | Their (dynamic) approach | Our (static) approach |
|---|---|---|
| Entry-point discovery | Test suite naturally compiles everything reachable | Walk `src/**/*.jl`, treat every top-level method def as an entry point |
| Viability filter | "Tests pass" | "Package loads + macroexpands cleanly" — much weaker, lets more packages through |
| Stdlib noise | Strip post-hoc by module | Already scoped: we only check files added via `addFile!` |
| Per-package isolation | Test runner already isolates | We need per-package process or world-age reset (or `--project=` swap) |
| Timeouts | Implicit via test-runner timeout | We need an explicit budget — our pre-fix ClothSim run was 60s. Set 60–120s/package, kill and mark fail |
| Method invalidation across packages | One Julia per package via Pkg's project | Same approach: spawn a fresh `julia --project=tmp_X` per package, don't carry state |
| Package extensions (1.9+) | N/A | Real issue. Each extension is conditionally loaded based on `Base.loaded_modules`. We need to decide: (a) skip extensions entirely, (b) check each extension independently with its trigger packages preloaded, (c) check only if all triggers are in `weakdeps` |
| GPU / Python / C wrappers | Left in, distorted results | We should *filter* — the static return types coming back from `ccall` / `PyCall.pycall` are effectively `Any`, which floods our `inferReturnType` cache with non-informative data |

### Concrete decisions for step 2

Borrowed from Pelenitsyn:
- **Source**: Julia General registry (more curated than open GitHub search).
- **Ordering**: GitHub stars as initial ranking. Cheap; correlates with "real users will hit this code."
- **Viability filter**: skip packages where `Pkg.add` + `using PkgName` fails.

Diverged from Pelenitsyn (because we're static):
- **Per-package timeout**: 60s wall clock for `runCheck!`. Kill and mark `timeout`.
- **Per-package process**: spawn fresh `julia` subprocess per package. Method-table contamination from one package must not leak into the next.

### Filtering — configurable, not hard-coded

We don't yet have evidence about how our checker handles real-world packages. So the corpus pipeline emits *categories* on every package; **the filter is a UI knob, not a build-time decision**. The DAG builder is permissive — it categorizes everything, drops nothing except "fails to load."

Categories (per package, multi-label):

| Category | Detection | Notes |
|---|---|---|
| `pure_julia` | No `ccall` outside JLL deps, no Python/GPU deps | The starting bucket |
| `jll_dep` | Transitive dep on any `*_jll` package | Acceptable when combined with `pure_julia` — JLLs are trivial `const` + `ccall`, nothing for us to mishandle |
| `python_wrapper` | Transitive dep on `PyCall.jl` / `PythonCall.jl` | `PyObject` returns become `Any` — high noise for our checker |
| `gpu` | Direct dep on `CUDA`, `AMDGPU`, `Metal`, `oneAPI`, `KernelAbstractions` | Knet-style noise per Pelenitsyn |
| `binary_shim` | Name ends in `_jll` | The JLL package itself, not a consumer. Tiny, mostly uninteresting |
| `has_extensions` | Project.toml has `[extensions]` / `[weakdeps]` | Conditional-load semantics need separate handling |
| `interface_package` | Name matches `Base$`, `^Abstract`, or `Interface$`; high abstract-arg density in `src/` | Intentionally dynamic — defines abstract method skeletons for downstream packages to specialize. Our checker can't analyze them well *by design of the package*, not by limitation of ours |
| `load_failed` | `using PkgName` exits non-zero in 30 s | The only category that's actually filtered out — we can't analyze it |

#### Note on `interface_package`

This is a distinct failure mode from "low-quality untyped". Packages like
`ArrayInterface`, `AbstractTrees`, `AbstractFFTs`, `CommonSolve`, `*Base`
deliberately ship abstract method signatures meant for downstream
specialization. The abstract args are the *point* of the package, not a
defect. We tag these and exclude them from the main run — chasing fires
inside them produces nothing useful, because the fires are correctly
identifying "no concrete type here," which is the intended semantics.

Detection is two-layered:
- **Name heuristic** (cheap): regex match `Base$`, `^Abstract`, `Interface$`.
  False positives possible but easy to override.
- **Static-feature confirmation** (from stage 3): high fraction of abstract-typed args
  AND low LOC. A real `Base` package is small and mostly skeletons.

### Starting scope (first batch)

Run only `pure_julia` (optionally + `jll_dep`), top-N by stars. Reason: low-star packages tend to be abandoned, experimental, or hobbyist code with patterns we can't yet handle — chasing each false positive there is expensive. Get clean signal from healthier packages first.

Once we have empirical numbers on this scope, *then* widen the filter and see what new failure modes appear.

### Cascade — cheap filters first

**Goal reframe**: don't try to check every package. Find the *easy* ones — packages that fit our checker well — and run those first. Anything that's going to fail anyway should be dropped at the cheapest possible stage, not after we've spent 60s on `runCheck!`.

Pipeline stages, in increasing cost order. A package failing at stage `k` never reaches stage `k+1`.

| Stage | Cost | What it filters out | Output label |
|---|---|---|---|
| 1. Registry dep walk | ms | Python deps, GPU deps, JLL-only shims | category tags |
| 2. **Our parser pass** | <1 s | Files with syntax our parser doesn't yet support (e.g. unhandled macro forms, `quote`/`Expr` we don't lower, exotic literal forms) | `parse_ok` / `parse_fail(reason)` |
| 3. Cheap static features | <1 s | Computes typed-arg fraction, `Any` density, `@nospecialize` count, `ccall` density. No filtering — fuels the classifier | feature vector |
| 4. `using PkgName` smoke test | up to 30 s | Packages we can't even load (missing deps, init errors) | `load_ok` / `load_failed` |
| 5. Full `runCheck!` | up to 60 s | Whatever the checker actually catches | error count, fire log |

Stage 2 — the parser pass — is the key insight. Our parser is a **subset** of Julia syntax (the features we've explicitly added support for: `where`, `<:`, `::` cast, lambdas, do-blocks, augmented assignment, etc.). A package that uses syntax we don't support is structurally beyond what our checker can handle — no point sending it to stage 5. Stage 2 also produces a useful side-product: the *names* of unsupported syntax forms, telling us what to prioritize implementing next.

The parser pass is also a *malformness* detector — packages with broken `.jl` files (rare, but happens with auto-generated code) are caught here for free.

### Stage 6 — Per-package fix loop (LLM-assisted)

After the cascade, the final pool is small (<1000 packages) and concrete. At this
scale, fully automated runs aren't enough — every package will have idiosyncrasies
(macros we lower differently, conventions we don't yet recognize, third-party
abstractions). An LLM-assisted per-package loop becomes practical here precisely
*because* the pool is small:

For each package in the pool:
1. Run `runCheck!`, capture fire log + source-line context.
2. Feed `(fire log, relevant source, our checker's diagnostic rules)` to an LLM.
3. LLM proposes one of:
   - A per-package `@nocheck` annotation list (skip a known dynamic method).
   - A per-package config override (e.g. "treat this function's return as `Any`").
   - A bug report against our checker if the fire is genuinely a false positive
     we should fix upstream.
   - Confirmation that the fire is a real type bug (the target outcome).
4. Human review of the LLM's classification; record as ground truth.

This loop turns the small pool into labeled data — every package produces a
(features, label) row that feeds the typed-ness classifier (see side track).

Importantly: this is a *workflow* layer over the pipeline, not another filter
stage. It runs after stage 5 (the actual `runCheck!`) and is driven from a
separate triage UI (future work; not part of step 2).

### Ranking the "easy" set

After the cascade, we have a population of packages that all pass stages 1–4. The next question is which of *these* to run first. Three signals available without running stage 5:

- **Stars / activity** (Pelenitsyn-style — correlates with "real users hit this code").
- **Parser-pass density** (fraction of `.jl` files in the package that parsed cleanly). High = friendlier to us.
- **Typed-ness features from stage 3** (high typed-arg fraction, low `Any` density). Friendlier to a static checker.

Combined score (initial, tune later): `stars · parse_pass_fraction · (1 + typed_arg_fraction)`. Negative penalties from category tags. Re-rank as classifier matures.

### Output of step 2

A ranked TSV / JSON manifest with columns:
- `package`, `version`, `stars`, `direct_deps`, `transitive_deps`, `loc_jl`, `has_extensions`
- `bucket` ∈ {`pure`, `jll`, `gpu`, `python`, `extension-heavy`, `excluded`}
- `score` (higher = run earlier)

Score heuristic (cheap to tune later): `stars / (1 + transitive_deps) − 100·has_python − 50·has_gpu − 10·is_jll`.

## What to put in the Flask UI

One row per pipeline step. For step 1 ("download corpus"), the UI displays:
- registry snapshot version + commit sha
- number of packages in registry
- cache status (downloaded vs cached)

For step 2 ("build DAG"):
- live count of packages categorized into each bucket
- top 50 of the ranked list with cumulative `LOC` so we can pick a manageable first batch

## Side track: "typed-ness" classifier

Separate workstream from the corpus runner. The goal is a cheap predictor that, given a package, estimates "how typed is this code?" — i.e., how likely our checker will produce useful signal vs. noise. Then we prioritize the queue by predicted typed-ness rather than just stars.

Doesn't need to be precise — just better-than-random ordering reduces wasted compute.

### Two candidate approaches

**Bayesian / naive-Bayes over static features.** Cheap, interpretable, runs without a model. Features that are trivially extractable per package without invoking the checker:

- Fraction of function defs that have `::Type` annotations on args.
- Fraction of return-type annotations (`::T` after `)`).
- Density of `Any`, `Union{...}`, `Vector{Any}`, `Dict{Any,Any}` in source.
- Count of `@nospecialize`, `@inbounds`, `@simd`.
- `ccall` density.
- LOC, fraction of `const` definitions, fraction of `struct` defs that are concrete vs parametric.

Plain logistic regression or naive Bayes over those features. Cheap to bootstrap, easy to inspect when it gets things wrong.

**Source-embedding model.** Heavier: embed `src/**/*.jl` with a code-embedding model (e.g., CodeBERT-class), train a small head on the typed-ness label. Catches semantic patterns the hand-crafted features miss. Worth doing only if (a) gets clearly better than the Bayesian baseline, (b) we have enough labels — which is the actual constraint.

### Label collection — the actual bottleneck

The classifier is easy; the labels aren't.

Options for `label = typed-ness of package`:

1. **Checker-derived**: run our checker, count `(real_errors / (real_errors + false_positives))` per package. Requires manual triage of fires. Painful per package but high-quality label. **N ~50–100 to bootstrap.**
2. **Self-reported proxy**: scrape README / docs for keywords like "type stable", "performance", "type annotation"; weak label but free. Useful as a *prior* feeding into the Bayesian model.
3. **Pelenitsyn-style proxy**: run `code_warntype` on test-suite-compiled instances, count abstract-typed registers. Free if the test suite runs. Closest to a ground-truth typed-ness signal.

Realistic plan: bootstrap with (2) + (3) as proxy labels on ~500 packages → train baseline classifier → use it to *prioritize* manual triage queue → fold (1) labels back in as we go. Active-learning loop, not a one-shot dataset.

### Where this slots in

Classifier output feeds the ranking score in step 2. Initial ranking uses stars + dep count; once the classifier has any signal at all, it becomes another term:

`score = stars / (1 + deps) + α · predicted_typedness − {penalties}`

Tune α empirically — start small, raise if the classifier becomes accurate.

## Open question for next step

**Driver language: Julia or Python?**

- Julia wins on data layer: `RegistryInstances.jl` and `Pkg` give the registry, deps graph, and Project.toml parsing natively. Subprocess-driving `julia` from `julia` is also less awkward (one runtime).
- Python wins on web layer (Flask). It can shell out to a Julia helper for the registry walk.

Lean: thin Python Flask UI, with a Julia CLI that emits JSON for everything package-graph-related. Keep the boundary at JSON so the UI is unaware of Pkg internals.
