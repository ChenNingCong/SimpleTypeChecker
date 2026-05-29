"""Flask UI for SimpleTypeChecker large-scale testing pipeline.

Run with:
    cd scripts/large_scale_test && python3 app.py
Then open http://127.0.0.1:5050/

The data layer (registry snapshot, categorization, parser pass) is in
julia/*.jl. Flask shells out to julia via subprocess and reads the JSON
artifacts the scripts emit into data/.
"""

import json
import os
import shlex
import subprocess
from collections import Counter
from pathlib import Path

from flask import Flask, jsonify, redirect, render_template, request, url_for


ROOT = Path(__file__).resolve().parent
DATA_DIR = ROOT / "data"
JULIA_DIR = ROOT / "julia"
DATA_DIR.mkdir(exist_ok=True)

SNAPSHOT_FILE = DATA_DIR / "registry_snapshot.json"
CATEGORIZED_FILE = DATA_DIR / "categorized.json"
ACTIVE_POOL_FILE = DATA_DIR / "active_pool.json"
PARSE_PASS_FILE = DATA_DIR / "parse_pass.json"
PARSE_PROGRESS_FILE = DATA_DIR / "parse_pass_progress.json"
FETCH_SUMMARY_FILE = DATA_DIR / "fetch_summary.json"
FETCH_PROGRESS_FILE = DATA_DIR / "fetch_progress.json"
TAR_DIR = DATA_DIR / "tarballs"
RESULTS_DIR = DATA_DIR / "results"

PIPELINE = [
    {
        "id": "stage0",
        "title": "Stage 0 — Registry snapshot",
        "blurb": "Snapshot the General registry to JSON. Cheap, idempotent.",
        "artifact": SNAPSHOT_FILE,
        "route": "stage0",
    },
    {
        "id": "stage1",
        "title": "Stage 1 — Categorize by deps",
        "blurb": "Walk the dep graph; tag pure_julia / jll_dep / python_wrapper / gpu / binary_shim / has_extensions.",
        "artifact": CATEGORIZED_FILE,
        "route": "stage1",
    },
    {
        "id": "stage2",
        "title": "Stage 2 — Parser pass",
        "blurb": "Run our parser on src/**/*.jl of candidate packages. Drop unparseable; histogram unsupported syntax.",
        "artifact": PARSE_PASS_FILE,
        "route": "stage2",
    },
]


app = Flask(__name__)


def artifact_status(path: Path) -> dict:
    if not path.exists():
        return {"exists": False, "size_bytes": 0, "mtime": None}
    st = path.stat()
    return {
        "exists": True,
        "size_bytes": st.st_size,
        "mtime": st.st_mtime,
    }


def run_julia(script: str, *args: str, timeout: int = 600) -> dict:
    """Invoke a Julia script under julia/ and return {ok, stdout, stderr, returncode}.

    Uses scripts-local Julia env at julia/ so scripts can `using JSON` etc.
    without polluting the main SimpleTypeChecker project.
    """
    script_path = JULIA_DIR / script
    cmd = ["julia", f"--project={JULIA_DIR}", str(script_path), *args]
    try:
        proc = subprocess.run(
            cmd,
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as e:
        return {
            "ok": False,
            "stdout": e.stdout or "",
            "stderr": (e.stderr or "") + f"\n[timeout after {timeout}s]",
            "returncode": -1,
            "cmd": " ".join(shlex.quote(c) for c in cmd),
        }
    return {
        "ok": proc.returncode == 0,
        "stdout": proc.stdout,
        "stderr": proc.stderr,
        "returncode": proc.returncode,
        "cmd": " ".join(shlex.quote(c) for c in cmd),
    }


@app.route("/")
def index():
    rows = []
    for stage in PIPELINE:
        rows.append({**stage, "status": artifact_status(stage["artifact"])})
    return render_template("index.html", pipeline=rows)


def _funnel_row(label, count, of=None):
    return {"label": label, "count": count,
            "pct": (100.0 * count / of) if (of and of > 0) else None}


@app.route("/dashboard")
def dashboard():
    """Cross-stage summary: funnel, parse-pass breakdown, quality metrics."""
    funnel = []
    snapshot = None
    if SNAPSHOT_FILE.exists():
        with open(SNAPSHOT_FILE) as f:
            snapshot = json.load(f)
        total = snapshot["package_count"]
        funnel.append(_funnel_row("Registered (General registry)", total, of=total))

    categorized = load_categorized()
    if categorized is not None:
        total = categorized.get("package_count", 0)
        bc = categorized.get("bucket_counts", {})
        funnel.append(_funnel_row("After categorization (pure_julia)",
                                  bc.get("pure_julia", 0), of=total))

    pool = None
    if ACTIVE_POOL_FILE.exists():
        with open(ACTIVE_POOL_FILE) as f:
            pool = json.load(f)
        ref = funnel[0]["count"] if funnel else None
        funnel.append(_funnel_row("Active pool (current filter)",
                                  pool["package_count"], of=ref))

    parse = None
    if PARSE_PASS_FILE.exists():
        with open(PARSE_PASS_FILE) as f:
            parse = json.load(f)

    status_breakdown = {}
    parse_clean = 0
    parse_partial = 0
    too_large = 0
    no_src = 0
    download_failed = 0
    other = 0
    jl_bytes_total = 0
    tar_bytes_total = 0
    sizes = []
    err_clusters = Counter()
    top_clean = []  # candidates for stage 3+

    if parse is not None:
        for r in parse["results"]:
            st = r.get("status", "unknown")
            status_breakdown[st] = status_breakdown.get(st, 0) + 1
            if st == "parse_ok":
                parse_clean += 1
                jl_bytes_total += r.get("jl_total_bytes", 0)
                tar_bytes_total += r.get("tar_bytes", 0)
                sizes.append(r.get("jl_total_bytes", 0))
                top_clean.append(r)
            elif st == "parse_partial":
                parse_partial += 1
                jl_bytes_total += r.get("jl_total_bytes", 0)
                tar_bytes_total += r.get("tar_bytes", 0)
                err = r.get("first_error", "")
                err_clusters[_cluster_err(err)] += 1
            elif st == "too_large":
                too_large += 1
            elif st == "no_src_jl":
                no_src += 1
            elif st == "download_failed":
                download_failed += 1
            else:
                other += 1
        # Top-30 by julia code size — best-candidate "good" packages worth
        # promoting to stage 3+.
        top_clean.sort(key=lambda r: -r.get("jl_total_bytes", 0))
        top_clean = top_clean[:30]
        ref = pool["package_count"] if pool else None
        funnel.append(_funnel_row("Stage 2: parse-clean", parse_clean, of=ref))

    # Median jl size + tar-to-jl ratio
    median_jl = 0
    if sizes:
        sizes_sorted = sorted(sizes)
        median_jl = sizes_sorted[len(sizes_sorted) // 2]
    junk_ratio = (tar_bytes_total / jl_bytes_total) if jl_bytes_total else None

    # Background-job status (parse_pass)
    running = PARSE_PROGRESS_FILE.exists() and (
        parse is None or not parse.get("finished", False))
    progress = None
    if PARSE_PROGRESS_FILE.exists():
        with open(PARSE_PROGRESS_FILE) as f:
            progress = json.load(f)

    return render_template(
        "dashboard.html",
        funnel=funnel,
        snapshot=snapshot,
        categorized_count=(categorized or {}).get("package_count", 0),
        pool=pool,
        parse=parse,
        status_breakdown=status_breakdown,
        median_jl_kb=median_jl / 1024,
        jl_bytes_total=jl_bytes_total,
        tar_bytes_total=tar_bytes_total,
        junk_ratio=junk_ratio,
        err_clusters=dict(err_clusters.most_common(10)),
        top_clean=top_clean,
        running=running,
        progress=progress,
    )


_ERR_PREFIX = ("Bad flag :", "ParseError", "Internal implementation", "Syntax error")
def _cluster_err(msg: str) -> str:
    """Bucket a parser-error string into a coarse cluster key so the
    dashboard can show 'top 10 error types' instead of 10000 unique strings."""
    if not msg:
        return "(empty)"
    for p in _ERR_PREFIX:
        if msg.startswith(p):
            return p + " …"
    # Fall back to the first sentence.
    return msg.split("\n", 1)[0][:80]


@app.route("/stage0")
def stage0():
    status = artifact_status(SNAPSHOT_FILE)
    snapshot = None
    sample = []
    if status["exists"]:
        with open(SNAPSHOT_FILE) as f:
            snapshot = json.load(f)
        sample = snapshot["packages"][:20]
    return render_template(
        "stage0.html",
        status=status,
        snapshot=snapshot,
        sample=sample,
    )


@app.route("/stage0/run", methods=["POST"])
def stage0_run():
    result = run_julia("snapshot_registry.jl", timeout=300)
    return jsonify(result)


# Cache the categorized payload in memory — it's ~6 MB and we don't want
# to re-read on every filter-AJAX. Invalidate on mtime change.
_CAT_CACHE = {"mtime": None, "data": None}


def load_categorized():
    if not CATEGORIZED_FILE.exists():
        return None
    mt = CATEGORIZED_FILE.stat().st_mtime
    if _CAT_CACHE["mtime"] != mt:
        with open(CATEGORIZED_FILE) as f:
            _CAT_CACHE["data"] = json.load(f)
        _CAT_CACHE["mtime"] = mt
    return _CAT_CACHE["data"]


# Order in which buckets are displayed; also the canonical predicate set.
BUCKET_ORDER = [
    "pure_julia",
    "jll_dep",
    "binary_shim",
    "python_wrapper",
    "gpu",
    "interface_package",
    "has_extensions",
]

# Default strict filter — narrows scope greatly. User can relax via UI.
DEFAULT_EXCLUDE = ["binary_shim", "python_wrapper", "gpu", "interface_package", "has_extensions"]
DEFAULT_REQUIRE = ["pure_julia"]


def _julia_lower_tuple(pkg):
    return (pkg.get("julia_lower_major", 0), pkg.get("julia_lower_minor", 0))


def filter_packages(data, require=None, exclude=None,
                    rdeps_min=None, rdeps_max=None,
                    ndeps_min=None, ndeps_max=None,
                    julia_supports_target=None,
                    julia_lower_min=None, julia_lower_max=None):
    require = set(require or [])
    exclude = set(exclude or [])
    out = []
    for pkg in data["packages"]:
        bs = set(pkg["buckets"])
        if require and not require.issubset(bs):
            continue
        if exclude & bs:
            continue
        rd = pkg.get("rdeps", 0)
        if rdeps_min is not None and rd < rdeps_min:
            continue
        if rdeps_max is not None and rd > rdeps_max:
            continue
        nd = pkg.get("n_deps", 0)
        if ndeps_min is not None and nd < ndeps_min:
            continue
        if ndeps_max is not None and nd > ndeps_max:
            continue
        if julia_supports_target is True and not pkg.get("julia_supports_target", False):
            continue
        if julia_lower_min is not None:
            lm, ln = julia_lower_min
            if _julia_lower_tuple(pkg) < (lm, ln):
                continue
        if julia_lower_max is not None:
            lm, ln = julia_lower_max
            if _julia_lower_tuple(pkg) > (lm, ln):
                continue
        out.append(pkg)
    return out


@app.route("/stage1")
def stage1():
    if not SNAPSHOT_FILE.exists():
        return render_template("stage1.html", missing_prereq=True)
    status = artifact_status(CATEGORIZED_FILE)
    if not status["exists"]:
        return render_template(
            "stage1.html",
            status=status,
            summary=None,
            bucket_counts={},
            bucket_order=BUCKET_ORDER,
            default_exclude=DEFAULT_EXCLUDE,
            default_require=DEFAULT_REQUIRE,
            missing_prereq=False,
        )
    data = load_categorized()
    summary = {
        "package_count": data.get("package_count"),
        "snapshot_revision": data.get("snapshot_revision"),
    }
    bucket_counts = data.get("bucket_counts", {})
    return render_template(
        "stage1.html",
        status=status,
        summary=summary,
        bucket_counts=bucket_counts,
        bucket_order=BUCKET_ORDER,
        default_exclude=DEFAULT_EXCLUDE,
        default_require=DEFAULT_REQUIRE,
        missing_prereq=False,
    )


@app.route("/stage1/run", methods=["POST"])
def stage1_run():
    result = run_julia("categorize.jl", timeout=120)
    return jsonify(result)


@app.route("/stage1/save_pool", methods=["POST"])
def stage1_save_pool():
    """Persist the current filter result to data/active_pool.json.

    Stores: filter spec + slim package records (name, uuid, tree_sha,
    latest_version, julia_compat). Stage 2 reads this — it doesn't need
    bucket info or dep graphs.
    """
    data = load_categorized()
    if data is None:
        return jsonify({"error": "categorized.json not built yet"}), 400
    body = request.get_json(silent=True) or {}

    def _opt_int(key):
        v = body.get(key)
        if v is None or v == "":
            return None
        try:
            return int(v)
        except (TypeError, ValueError):
            return None

    def _opt_version(key):
        v = body.get(key)
        if v is None or v == "":
            return None
        parts = str(v).split(".")
        try:
            return (int(parts[0]), int(parts[1]) if len(parts) > 1 else 0)
        except (TypeError, ValueError):
            return None

    matched = filter_packages(
        data,
        require=body.get("require", []),
        exclude=body.get("exclude", []),
        rdeps_min=_opt_int("rdeps_min"),
        rdeps_max=_opt_int("rdeps_max"),
        ndeps_min=_opt_int("ndeps_min"),
        ndeps_max=_opt_int("ndeps_max"),
        julia_supports_target=bool(body.get("julia_supports_target", False)) or None,
        julia_lower_min=_opt_version("julia_lower_min"),
        julia_lower_max=_opt_version("julia_lower_max"),
    )

    pool = {
        "filter_spec": body,
        "snapshot_revision": data.get("snapshot_revision"),
        "package_count": len(matched),
        "packages": [
            {
                "name": p["name"],
                "uuid": p["uuid"],
                "tree_sha": p.get("tree_sha", ""),
                "latest_version": p["latest_version"],
                "julia_compat": p.get("julia_compat", ""),
            }
            for p in matched
        ],
    }
    with open(ACTIVE_POOL_FILE, "w") as f:
        json.dump(pool, f)
    return jsonify({"ok": True, "count": len(matched), "saved_to": str(ACTIVE_POOL_FILE)})


@app.route("/stage1/filter", methods=["POST"])
def stage1_filter():
    """Apply require/exclude predicates and return count + sample."""
    data = load_categorized()
    if data is None:
        return jsonify({"error": "categorized.json not built yet"}), 400
    body = request.get_json(silent=True) or {}
    require = body.get("require", [])
    exclude = body.get("exclude", [])
    sort_by = body.get("sort_by", "name")  # name | rdeps_desc | ndeps_asc

    def _opt_int(key):
        v = body.get(key)
        if v is None or v == "":
            return None
        try:
            return int(v)
        except (TypeError, ValueError):
            return None

    def _opt_version(key):
        v = body.get(key)
        if v is None or v == "":
            return None
        parts = str(v).split(".")
        try:
            major = int(parts[0])
            minor = int(parts[1]) if len(parts) > 1 else 0
            return (major, minor)
        except (TypeError, ValueError):
            return None

    matched = filter_packages(
        data,
        require=require,
        exclude=exclude,
        rdeps_min=_opt_int("rdeps_min"),
        rdeps_max=_opt_int("rdeps_max"),
        ndeps_min=_opt_int("ndeps_min"),
        ndeps_max=_opt_int("ndeps_max"),
        julia_supports_target=bool(body.get("julia_supports_target", False)) or None,
        julia_lower_min=_opt_version("julia_lower_min"),
        julia_lower_max=_opt_version("julia_lower_max"),
    )

    if sort_by == "rdeps_desc":
        matched.sort(key=lambda p: -p.get("rdeps", 0))
    elif sort_by == "ndeps_asc":
        matched.sort(key=lambda p: p.get("n_deps", 0))
    elif sort_by == "ndeps_desc":
        matched.sort(key=lambda p: -p.get("n_deps", 0))
    else:
        matched.sort(key=lambda p: p["name"])

    # Per-bucket counts WITHIN the matched set — useful for showing "of
    # the strict pool, how many also have jll_dep?"
    cnt = Counter()
    for p in matched:
        for b in p["buckets"]:
            cnt[b] += 1

    sample = [
        {
            "name": p["name"],
            "latest_version": p["latest_version"],
            "buckets": p["buckets"],
            "n_deps": p.get("n_deps", len(p["deps_uuids"])),
            "rdeps": p.get("rdeps", 0),
            "version_major": p.get("version_major", 0),
            "julia_compat": p.get("julia_compat", ""),
            "julia_supports_target": p.get("julia_supports_target", False),
            "repo": p["repo"],
        }
        for p in matched[:50]
    ]
    return jsonify({
        "count": len(matched),
        "total": len(data["packages"]),
        "sample": sample,
        "bucket_counts_in_filter": dict(cnt),
    })


@app.route("/stage2")
def stage2():
    if not CATEGORIZED_FILE.exists():
        return render_template("stage2.html", missing_prereq=True, pool=None)

    pool = None
    if ACTIVE_POOL_FILE.exists():
        with open(ACTIVE_POOL_FILE) as f:
            pool = json.load(f)

    status = artifact_status(PARSE_PASS_FILE)
    summary = None
    if status["exists"]:
        with open(PARSE_PASS_FILE) as f:
            summary = json.load(f)
    return render_template(
        "stage2.html",
        status=status,
        summary=summary,
        pool=pool,
        missing_prereq=False,
    )


def _spawn_julia_bg(script_name: str, env_extra: dict, log_prefix: str) -> dict:
    script = JULIA_DIR / script_name
    cmd = ["julia", f"--project={JULIA_DIR}", str(script)]
    env = os.environ.copy()
    env.update({k: str(v) for k, v in env_extra.items()})
    log = DATA_DIR / f"{log_prefix}.log"
    err = DATA_DIR / f"{log_prefix}.err"
    log.write_text("")
    err.write_text("")
    proc = subprocess.Popen(
        cmd, cwd=ROOT, env=env,
        stdout=open(log, "w"), stderr=open(err, "w"),
        start_new_session=True,
    )
    return {"ok": True, "pid": proc.pid,
            "cmd": " ".join(shlex.quote(c) for c in cmd),
            "log": str(log), "err": str(err)}


@app.route("/stage2/fetch_run", methods=["POST"])
def stage2_fetch_run():
    if not ACTIVE_POOL_FILE.exists():
        return jsonify({"ok": False, "error": "no active pool — save one from stage 1"}), 400
    body = request.get_json(silent=True) or {}
    return jsonify(_spawn_julia_bg(
        "fetch_tarballs.jl",
        {"PARSE_MAX_TAR_MB": int(body.get("max_tar_mb", 1))},
        "fetch",
    ))


@app.route("/stage2/parse_run", methods=["POST"])
def stage2_parse_run():
    if not ACTIVE_POOL_FILE.exists():
        return jsonify({"ok": False, "error": "no active pool"}), 400
    if not TAR_DIR.exists():
        return jsonify({"ok": False, "error": "no tarball cache — run fetch first"}), 400
    body = request.get_json(silent=True) or {}
    return jsonify(_spawn_julia_bg(
        "parse_pass.jl",
        {"PARSE_FORCE_REPROCESS": "true" if body.get("force") else "false"},
        "parse_pass",
    ))


@app.route("/stage2/progress")
def stage2_progress():
    """Unified status: fetch + parse + cache counts."""
    out = {
        "tar_count": len(list(TAR_DIR.glob("*.tar.gz"))) if TAR_DIR.exists() else 0,
        "result_count": len(list(RESULTS_DIR.glob("*.json"))) if RESULTS_DIR.exists() else 0,
    }
    if FETCH_PROGRESS_FILE.exists():
        with open(FETCH_PROGRESS_FILE) as f:
            out["fetch"] = json.load(f)
    if PARSE_PROGRESS_FILE.exists():
        with open(PARSE_PROGRESS_FILE) as f:
            out["parse"] = json.load(f)
    return jsonify(out)


if __name__ == "__main__":
    app.run(host="127.0.0.1", port=5050, debug=True)
