#!/usr/bin/env julia
#
# Stage 1: categorize each package in the registry snapshot with bucket tags.
#
# Tags are multi-label and computed from the dep graph + name heuristics.
# Filtering is NOT applied here — that's the UI's job. We emit every
# package with its full bucket list so the frontend can mix-and-match
# predicates without re-running this stage.
#
# Inputs:  data/registry_snapshot.json
# Outputs: data/categorized.json

using JSON

const SCRIPT_DIR = @__DIR__
const DATA_DIR   = abspath(joinpath(SCRIPT_DIR, "..", "data"))
const SNAPSHOT   = joinpath(DATA_DIR, "registry_snapshot.json")
const OUT        = joinpath(DATA_DIR, "categorized.json")

# UUIDs of "membrane" packages we identify by direct name. We look these
# up by name in the snapshot once, then chase transitive reverse-deps.
const PYTHON_NAMES = Set(["PyCall", "PythonCall"])
const GPU_NAMES    = Set(["CUDA", "AMDGPU", "Metal", "oneAPI", "KernelAbstractions"])

# Name regexes for interface-style packages. Intentionally conservative —
# false positives are easy to override later via an allowlist.
const INTERFACE_NAME_PATTERNS = [
    r"Base$",         # FooBase
    r"^Abstract",     # AbstractFoo
    r"Interface$",    # FooInterface
]

function load_snapshot()
    open(SNAPSHOT) do f
        return JSON.parse(f)
    end
end

# Reverse adjacency: uuid -> Set{uuid of direct dependents}
function build_reverse_deps(packages::Vector)
    rev = Dict{String, Set{String}}()
    for pkg in packages
        u = pkg["uuid"]
        rev[u] = get!(rev, u, Set{String}())
        for d in pkg["deps_uuids"]
            push!(get!(() -> Set{String}(), rev, d), u)
        end
    end
    return rev
end

# Reverse-BFS: starting from seed UUIDs, traverse the reverse-dep edges
# to find every package that transitively depends on at least one seed.
# Seeds themselves are included in the result.
function transitive_dependents(seeds, rev_deps::Dict{String, Set{String}})
    closure = Set{String}()
    queue = collect(seeds)
    for s in seeds
        push!(closure, s)
    end
    while !isempty(queue)
        u = pop!(queue)
        for v in get(rev_deps, u, Set{String}())
            if !(v in closure)
                push!(closure, v)
                push!(queue, v)
            end
        end
    end
    return closure
end

function matches_interface_name(name::AbstractString)
    for re in INTERFACE_NAME_PATTERNS
        if occursin(re, name)
            return true
        end
    end
    return false
end

# Parse a semver string ("1.2.3-pre+meta") into (major, minor, patch). We
# only need major + minor here — for "is this >= 1.0?" the leading digit
# is enough.
function parse_version_tuple(s::AbstractString)
    # Strip pre-release / metadata suffixes.
    core = split(s, ['-', '+']; limit=2)[1]
    parts = split(core, '.')
    function _toint(x)
        v = tryparse(Int, x)
        v === nothing ? 0 : v
    end
    major = length(parts) >= 1 ? _toint(parts[1]) : 0
    minor = length(parts) >= 2 ? _toint(parts[2]) : 0
    patch = length(parts) >= 3 ? _toint(parts[3]) : 0
    return (major, minor, patch)
end

function categorize(packages::Vector, rev_deps::Dict{String, Set{String}})
    # Resolve seed UUIDs from names.
    by_name = Dict(p["name"] => p["uuid"] for p in packages)

    python_seeds = Set{String}()
    for n in PYTHON_NAMES
        haskey(by_name, n) && push!(python_seeds, by_name[n])
    end

    gpu_seeds = Set{String}()
    for n in GPU_NAMES
        haskey(by_name, n) && push!(gpu_seeds, by_name[n])
    end

    python_closure = transitive_dependents(python_seeds, rev_deps)
    gpu_closure    = transitive_dependents(gpu_seeds, rev_deps)

    name_by_uuid = Dict(p["uuid"] => p["name"] for p in packages)

    out = Vector{Dict{String, Any}}()
    sizehint!(out, length(packages))

    for pkg in packages
        u = pkg["uuid"]
        name = pkg["name"]
        buckets = String[]

        is_python = u in python_closure
        is_gpu    = u in gpu_closure
        is_jll_self = endswith(name, "_jll")
        is_interface_byname = matches_interface_name(name)

        any_jll_dep = any(d -> endswith(get(name_by_uuid, d, ""), "_jll"),
                          pkg["deps_uuids"])

        is_python && push!(buckets, "python_wrapper")
        is_gpu && push!(buckets, "gpu")
        is_jll_self && push!(buckets, "binary_shim")
        is_interface_byname && push!(buckets, "interface_package")
        any_jll_dep && !is_jll_self && push!(buckets, "jll_dep")

        # "pure_julia" is the residual — no Python, no GPU, not a JLL itself.
        # JLL deps are still allowed (we tag them, but accept).
        if !is_python && !is_gpu && !is_jll_self
            push!(buckets, "pure_julia")
        end

        # Reverse-dependent count: how many other registered packages
        # list this one as a direct dep. Cheap popularity proxy.
        rdeps = length(get(rev_deps, u, Set{String}()))

        # Major version from latest_version. >= 1 means the package has
        # made a stable-API commitment.
        (major, minor, patch) = parse_version_tuple(pkg["latest_version"])

        push!(out, Dict{String,Any}(
            "name"                  => name,
            "uuid"                  => u,
            "latest_version"        => pkg["latest_version"],
            "tree_sha"              => get(pkg, "tree_sha", ""),
            "version_major"         => major,
            "version_minor"         => minor,
            "version_patch"         => patch,
            "repo"                  => pkg["repo"],
            "deps_uuids"            => pkg["deps_uuids"],
            "n_deps"                => length(pkg["deps_uuids"]),
            "rdeps"                 => rdeps,
            "julia_compat"          => get(pkg, "julia_compat", ""),
            "julia_supports_target" => get(pkg, "julia_supports_target", false),
            "julia_lower_major"     => get(pkg, "julia_lower_major", 0),
            "julia_lower_minor"     => get(pkg, "julia_lower_minor", 0),
            "buckets"               => buckets,
        ))
    end

    return out
end

function bucket_counts(packages)
    cnt = Dict{String, Int}()
    for pkg in packages
        for b in pkg["buckets"]
            cnt[b] = get(cnt, b, 0) + 1
        end
    end
    return cnt
end

function main()
    isfile(SNAPSHOT) || error("snapshot not found: $SNAPSHOT — run stage 0 first")

    @info "Loading snapshot..."
    snap = load_snapshot()
    pkgs = snap["packages"]
    @info "Loaded packages" count=length(pkgs) revision=snap["registry_revision"]

    @info "Building reverse-dep adjacency..."
    rev = build_reverse_deps(pkgs)

    @info "Categorizing..."
    categorized = categorize(pkgs, rev)

    counts = bucket_counts(categorized)
    @info "Bucket counts:"
    for (b, n) in sort(collect(counts); by = x -> -x[2])
        println("  $(rpad(b, 22)) $n")
    end

    out = Dict{String, Any}(
        "snapshot_revision" => snap["registry_revision"],
        "package_count" => length(categorized),
        "bucket_counts" => counts,
        "packages" => categorized,
    )

    open(OUT, "w") do io
        JSON.print(io, out)
    end
    println("Wrote $(length(categorized)) categorized packages to $OUT")
    return 0
end

if abspath(PROGRAM_FILE) == @__FILE__
    exit(main())
end
