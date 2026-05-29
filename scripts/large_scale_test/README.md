# SimpleTypeChecker — Large-Scale Testing Pipeline

Cascade-of-cheap-filters pipeline for finding Julia packages our checker can
handle well. See `docs/large_scale_test_methodology.md` for the design.

## Architecture

- **Flask UI** (`app.py`) — thin shell, one page per pipeline stage.
- **Julia data layer** (`julia/*.jl`) — does the actual registry walk,
  categorization, parsing. Emits JSON into `data/`.
- **JSON seam** — Flask never imports Julia. Stages communicate via files.

## Run

```bash
cd scripts/large_scale_test
python3 app.py
# open http://127.0.0.1:5050/
```

Each stage page has a "run" button that shells out to the corresponding
Julia script. Re-running is cheap when nothing has changed (each script is
idempotent against its inputs).

## Stages

| ID | Script | Output | Status |
|----|--------|--------|--------|
| 0  | `julia/snapshot_registry.jl` | `data/registry_snapshot.json` | ✓ |
| 1  | `julia/categorize.jl`        | `data/categorized.json`       | TODO |
| 2  | `julia/parse_pass.jl`        | `data/parse_pass.json`        | TODO |
| 3  | `julia/features.jl`          | `data/features.json`          | TODO |
| 4  | `julia/smoke_load.jl`        | `data/smoke_load.json`        | TODO |
| 5  | `julia/runcheck.jl`          | `data/runcheck.json`          | TODO |

## Dependencies

- Python 3.10+, Flask
- Julia 1.12+ (uses `Pkg.Registry.reachable_registries`)
- A populated General registry (`~/.julia/registries/General*`)
