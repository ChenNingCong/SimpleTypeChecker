#!/usr/bin/env julia
#
# Phase: downloader only. For every package in data/active_pool.json,
# HEAD the Pkg-server URL; if Content-Length <= max-tar; download the
# raw .tar.gz to data/tarballs/{uuid}.tar.gz. Never decompresses.
#
# Idempotent: skip if file already exists. Re-running only fetches
# newly added packages.
#
# Env knobs:
#   PARSE_MAX_TAR_MB        — default 1
#   PARSE_FETCH_PROGRESS_EVERY  — default 10

using JSON
using Downloads

const SCRIPT_DIR    = @__DIR__
const DATA_DIR      = abspath(joinpath(SCRIPT_DIR, "..", "data"))
const POOL_FILE     = joinpath(DATA_DIR, "active_pool.json")
const TAR_DIR       = joinpath(DATA_DIR, "tarballs")
const PROGRESS_FILE = joinpath(DATA_DIR, "fetch_progress.json")
const SUMMARY_FILE  = joinpath(DATA_DIR, "fetch_summary.json")

const MAX_TAR_MB    = parse(Int, get(ENV, "PARSE_MAX_TAR_MB", "1"))
const PROGRESS_EVERY = parse(Int, get(ENV, "PARSE_FETCH_PROGRESS_EVERY", "10"))
const PKG_SERVER    = get(ENV, "JULIA_PKG_SERVER", "https://pkg.julialang.org")

tarball_url(uuid, sha) = "$(PKG_SERVER)/package/$(uuid)/$(sha)"
tarball_path(uuid) = joinpath(TAR_DIR, "$(uuid).tar.gz")

function head_size(url)::Int
    sz = -1
    try
        Downloads.request(url; method="HEAD", throw=false) do resp
            for (k, v) in resp.headers
                if lowercase(k) == "content-length"
                    sz = parse(Int, v)
                end
            end
        end
    catch
        sz = -1
    end
    return sz
end

function write_progress(processed, total, current, started, downloaded, skipped, errors)
    open(PROGRESS_FILE, "w") do io
        JSON.print(io, Dict(
            "processed" => processed, "total" => total, "current" => current,
            "started" => started, "elapsed_sec" => time() - started,
            "downloaded" => downloaded, "skipped" => skipped, "errors" => errors,
        ))
    end
end

function main()
    isfile(POOL_FILE) || error("active pool not found: $POOL_FILE")
    mkpath(TAR_DIR)
    pool = open(JSON.parse, POOL_FILE)
    pkgs = pool["packages"]
    n = length(pkgs)
    started = time()
    @info "Fetch starting" pool_size=n max_tar_mb=MAX_TAR_MB tarball_dir=TAR_DIR

    downloaded = 0  # newly downloaded this run
    skipped = 0     # already-cached
    errors = 0      # too_large, HEAD or GET failures
    error_records = Vector{Dict{String,Any}}()
    max_bytes = MAX_TAR_MB * 1024 * 1024

    for (i, pkg) in enumerate(pkgs)
        name = pkg["name"]; uuid = pkg["uuid"]; sha = pkg["tree_sha"]
        i % PROGRESS_EVERY == 1 && write_progress(i-1, n, name, started, downloaded, skipped, errors)

        out = tarball_path(uuid)
        if isfile(out)
            skipped += 1
            continue
        end
        if isempty(sha)
            errors += 1
            push!(error_records, Dict("uuid"=>uuid,"name"=>name,"reason"=>"no_tree_sha"))
            continue
        end

        url = tarball_url(uuid, sha)
        sz = head_size(url)
        if sz > 0 && sz > max_bytes
            errors += 1
            push!(error_records, Dict("uuid"=>uuid,"name"=>name,"reason"=>"too_large","tar_bytes"=>sz))
            continue
        end

        # Download to temp file alongside the cache, atomic-rename on success.
        tmp = out * ".part"
        try
            Downloads.download(url, tmp)
            actual = filesize(tmp)
            if actual > max_bytes
                rm(tmp; force=true)
                errors += 1
                push!(error_records, Dict("uuid"=>uuid,"name"=>name,"reason"=>"too_large_after_get","tar_bytes"=>actual))
                continue
            end
            mv(tmp, out; force=true)
            downloaded += 1
        catch e
            rm(tmp; force=true)
            errors += 1
            push!(error_records, Dict("uuid"=>uuid,"name"=>name,
                                       "reason"=>"download_failed",
                                       "err"=>first(string(e), 200)))
        end
    end

    write_progress(n, n, "(done)", started, downloaded, skipped, errors)
    open(SUMMARY_FILE, "w") do io
        JSON.print(io, Dict(
            "pool_size" => n,
            "downloaded" => downloaded,
            "skipped_cached" => skipped,
            "errors" => errors,
            "max_tar_mb" => MAX_TAR_MB,
            "elapsed_sec" => time() - started,
            "error_records" => error_records,
            "finished" => true,
        ))
    end
    @info "Fetch complete" downloaded skipped errors elapsed=time()-started
    return 0
end

if abspath(PROGRAM_FILE) == @__FILE__
    exit(main())
end
