#!/usr/bin/env julia
#
# Stage 2: parser-pass over the active pool.
#
# For each package in data/active_pool.json:
#   1. HEAD the Pkg-server URL to get Content-Length.
#   2. Skip if > MAX_TAR_MB megabytes (people upload data + binaries that
#      bloat tarballs; we don't want to pay for those).
#   3. Download the gzipped tarball to RAM. No temp file.
#   4. Stream-decompress + walk the tar headers in RAM, extracting only
#      `*/src/**/*.jl` to byte buffers.
#   5. Run SimpleTypeChecker's parser on each .jl content. Record pass/fail
#      and the first failure message per package.
#   6. Aggregate: jl_total_size_bytes is the real "Julia code size" — large
#      tarball + small jl_total_size_bytes = lots of non-code junk.
#
# Output is written incrementally to data/parse_pass.json so partial
# results are visible while the run is in progress.

using JSON
using Downloads
using CodecZlib

# SimpleTypeChecker — point at the repo root via LOAD_PATH so we don't
# pollute the scripts-local env's manifest.
const REPO_ROOT = abspath(joinpath(@__DIR__, "..", "..", ".."))
push!(LOAD_PATH, REPO_ROOT)
import SimpleTypeChecker
import SimpleTypeChecker.SyntaxAdaptor: parseJuAST

const SCRIPT_DIR    = @__DIR__
const DATA_DIR      = abspath(joinpath(SCRIPT_DIR, "..", "data"))
const POOL_FILE     = joinpath(DATA_DIR, "active_pool.json")
const TAR_DIR       = joinpath(DATA_DIR, "tarballs")
const RESULTS_DIR   = joinpath(DATA_DIR, "results")
const SUMMARY_FILE  = joinpath(DATA_DIR, "parse_pass.json")
const PROGRESS_FILE = joinpath(DATA_DIR, "parse_pass_progress.json")

const PROGRESS_EVERY = parse(Int, get(ENV, "PARSE_PROGRESS_EVERY", "10"))
const FORCE_REPROCESS = lowercase(get(ENV, "PARSE_FORCE_REPROCESS", "false")) == "true"

tarball_path(uuid) = joinpath(TAR_DIR, "$(uuid).tar.gz")
result_path(uuid) = joinpath(RESULTS_DIR, "$(uuid).json")

# === In-memory tar walker ===
# POSIX ustar format. We only need filename + size + content for regular
# files. Long-name extensions ('L' / 'K' typeflag, pax 'x' / 'g') are
# handled by reading the extension's data into a side-buffer and using
# it as the *next* entry's name.

function _read_octal(bytes::AbstractVector{UInt8})::Int
    n = 0
    for b in bytes
        b == 0x00 && break
        b == UInt8(' ') && continue
        if UInt8('0') <= b <= UInt8('7')
            n = n * 8 + (Int(b) - Int('0'))
        else
            break
        end
    end
    return n
end

function _read_null_terminated(bytes::AbstractVector{UInt8})::String
    # Find the first NUL or use the whole slice.
    idx = findfirst(==(0x00), bytes)
    if idx === nothing
        return String(copy(bytes))
    end
    return String(copy(bytes[1:idx-1]))
end

# Categorization of a path inside the tarball. Pkg-server tarballs have
# no leading top-directory — entries go straight to `src/Foo.jl`,
# `Project.toml`, etc. (confirmed via `tar -tzf`). So we classify by the
# very first path segment.
@enum FileKind FK_SRC FK_TEST FK_DOCS FK_EXT FK_ROOT FK_OTHER

function classify_path(name::AbstractString)
    if !occursin('/', name)
        return (FK_ROOT, "", String(name))
    end
    seg1_end = findfirst('/', name)
    seg1 = SubString(name, 1, seg1_end - 1)
    rest_len = lastindex(name) - seg1_end
    if seg1 == "src" && rest_len > 0
        return (FK_SRC, "src", String(name))
    elseif seg1 == "test" && rest_len > 0
        return (FK_TEST, "test", String(name))
    elseif seg1 == "docs" && rest_len > 0
        return (FK_DOCS, "docs", String(name))
    elseif seg1 == "ext" && rest_len > 0
        return (FK_EXT, "ext", String(name))
    else
        return (FK_OTHER, String(seg1), String(name))
    end
end

mutable struct PackageContents
    src_files            :: Dict{String, Vector{UInt8}}
    test_files           :: Dict{String, Vector{UInt8}}
    n_src_jl             :: Int
    n_test_jl            :: Int
    n_docs_jl            :: Int
    n_ext_jl             :: Int
    n_other_jl           :: Int
    n_total_files        :: Int
    has_project_toml     :: Bool
    has_manifest_toml    :: Bool
    has_readme           :: Bool
    has_runtests         :: Bool
    has_docs_dir         :: Bool
    has_ext_dir          :: Bool
    has_compat_helper    :: Bool   # .github / CompatHelper indicator of active maintenance
end
PackageContents() = PackageContents(
    Dict{String,Vector{UInt8}}(), Dict{String,Vector{UInt8}}(),
    0, 0, 0, 0, 0, 0,
    false, false, false, false, false, false, false,
)

"""
    walk_tarball(tar_bytes) -> PackageContents

Iterate the decompressed tarball entirely in memory, collecting (a) `.jl`
content under `src/` and `test/` for downstream parsing/LOC and (b) a
bag of cheap structural flags + file counts for fingerprinting.
"""
function walk_tarball(tar_bytes::Vector{UInt8})
    pc = PackageContents()
    n = length(tar_bytes)
    pos = 1                       # 1-based, points to start of next header
    next_name_override = ""       # set by 'L' / 'K' or pax-x extensions
    while pos + 511 <= n
        hdr = view(tar_bytes, pos:(pos+511))
        if all(==(0x00), hdr)
            break
        end
        name      = isempty(next_name_override) ? _read_null_terminated(view(hdr, 1:100)) : next_name_override
        prefix    = _read_null_terminated(view(hdr, 346:500))
        if !isempty(prefix) && isempty(next_name_override)
            name = string(prefix, "/", name)
        end
        size      = _read_octal(view(hdr, 125:136))
        typeflag  = hdr[157]
        pos      += 512
        pad       = mod(-size, 512)

        if typeflag == UInt8('L') || typeflag == UInt8('K')
            payload_end = pos + size - 1
            if payload_end <= n
                next_name_override = _read_null_terminated(view(tar_bytes, pos:payload_end))
            end
            pos += size + pad
            continue
        elseif typeflag == UInt8('x') || typeflag == UInt8('g')
            payload_end = pos + size - 1
            if payload_end <= n
                txt = String(copy(tar_bytes[pos:payload_end]))
                for line in split(txt, '\n')
                    sp = findfirst(==(' '), line)
                    sp === nothing && continue
                    kv = SubString(line, sp+1, lastindex(line))
                    if startswith(kv, "path=")
                        next_name_override = String(SubString(kv, 6))
                        break
                    end
                end
            end
            pos += size + pad
            continue
        end

        # Directories — note structural flags but no content.
        if typeflag == UInt8('5')
            kind, _bucket, rel = classify_path(name)
            kind == FK_DOCS && (pc.has_docs_dir = true)
            kind == FK_EXT  && (pc.has_ext_dir  = true)
            pos += size + pad
            next_name_override = ""
            continue
        end

        # Regular files.
        if typeflag == UInt8('0') || typeflag == 0x00
            pc.n_total_files += 1
            kind, _bucket, rel = classify_path(name)
            payload_end = pos + size - 1

            # Structural flags from filenames.
            base = isempty(rel) ? name : rel
            if endswith(base, "/Project.toml") || base == "Project.toml"
                pc.has_project_toml = true
            elseif endswith(base, "/Manifest.toml") || base == "Manifest.toml"
                pc.has_manifest_toml = true
            elseif occursin(r"(^|/)README(\.md|\.MD|\.txt|)$", base)
                pc.has_readme = true
            elseif endswith(base, "/runtests.jl") || base == "test/runtests.jl"
                pc.has_runtests = true
            elseif occursin(r"\.github/workflows/CompatHelper", name)
                pc.has_compat_helper = true
            end

            if endswith(name, ".jl")
                content_ok = payload_end <= n
                content = content_ok ? tar_bytes[pos:payload_end] : UInt8[]
                if kind == FK_SRC
                    pc.n_src_jl += 1
                    if content_ok
                        pc.src_files[name] = content
                    end
                elseif kind == FK_TEST
                    pc.n_test_jl += 1
                    if content_ok
                        pc.test_files[name] = content
                    end
                elseif kind == FK_DOCS
                    pc.n_docs_jl += 1
                elseif kind == FK_EXT
                    pc.n_ext_jl += 1
                    pc.has_ext_dir = true
                else
                    pc.n_other_jl += 1
                end
            end
        end
        pos += size + pad
        next_name_override = ""
    end
    return pc
end

# === Code fingerprint ===
# Cheap text-feature pass over src/ files. Counts are sums across the whole
# src/ tree. We use byte-level regex/strstr to stay allocation-light.

struct CodeFingerprint
    loc                  :: Int       # newline-terminated lines
    comment_lines        :: Int       # lines whose first non-space char is '#'
    blank_lines          :: Int
    function_count       :: Int
    struct_count         :: Int       # both struct and mutable struct
    mutable_struct_count :: Int
    typeannot_count      :: Int       # `::` occurrences
    where_count          :: Int       # `where ` occurrences
    union_count          :: Int       # `Union{`
    any_token_count      :: Int       # `Any` as a word
    macro_count          :: Int       # `@\w+` (any macro use)
    generated_count      :: Int       # `@generated`
    nospecialize_count   :: Int
    inbounds_count       :: Int
    eval_count           :: Int       # `eval(`, `@eval `, `Core.eval`
    ccall_count          :: Int       # `ccall(`
    nocheck_count        :: Int       # `@nocheck` (our own annotation)
end

function _count_substr(content::AbstractString, needle::AbstractString)
    n, i, L, M = 0, 1, lastindex(content), lastindex(needle)
    while true
        idx = findnext(needle, content, i)
        idx === nothing && break
        n += 1
        i = first(idx) + M
        i > L && break
    end
    return n
end

function _count_regex(content::AbstractString, re::Regex)
    n = 0
    for _ in eachmatch(re, content)
        n += 1
    end
    return n
end

function fingerprint_srcfiles(files::Dict{String, Vector{UInt8}})
    loc = comment_lines = blank_lines = 0
    function_count = struct_count = mutable_struct_count = 0
    typeannot_count = where_count = union_count = any_token_count = 0
    macro_count = generated_count = nospecialize_count = inbounds_count = 0
    eval_count = ccall_count = nocheck_count = 0

    for (_path, bytes) in files
        s = String(copy(bytes))
        # Line metrics.
        for line in split(s, '\n')
            loc += 1
            stripped = lstrip(line)
            if isempty(stripped)
                blank_lines += 1
            elseif startswith(stripped, '#')
                comment_lines += 1
            end
        end
        loc -= 1  # last split element after trailing newline

        function_count       += _count_regex(s, r"\bfunction\s")
        mutable_struct_count += _count_regex(s, r"\bmutable\s+struct\b")
        struct_count         += _count_regex(s, r"\bstruct\b")
        typeannot_count      += _count_substr(s, "::")
        where_count          += _count_regex(s, r"\bwhere\b")
        union_count          += _count_substr(s, "Union{")
        any_token_count      += _count_regex(s, r"\bAny\b")
        macro_count          += _count_regex(s, r"@[A-Za-z_][A-Za-z_0-9]*")
        generated_count      += _count_substr(s, "@generated")
        nospecialize_count   += _count_substr(s, "@nospecialize")
        inbounds_count       += _count_substr(s, "@inbounds")
        eval_count           += _count_substr(s, "eval(")
        eval_count           += _count_substr(s, "@eval")
        ccall_count          += _count_substr(s, "ccall(")
        nocheck_count        += _count_substr(s, "@nocheck")
    end
    return CodeFingerprint(loc, comment_lines, blank_lines,
        function_count, struct_count, mutable_struct_count,
        typeannot_count, where_count, union_count, any_token_count,
        macro_count, generated_count, nospecialize_count, inbounds_count,
        eval_count, ccall_count, nocheck_count)
end

# Bytes-LOC for test files — we don't need feature breakdown, just LOC.
function test_loc(files::Dict{String, Vector{UInt8}})
    n = 0
    for (_p, b) in files
        for c in b
            c == UInt8('\n') && (n += 1)
        end
    end
    return n
end

# Adapter: keep the old name as a thin shim so we don't have to rewrite
# the rest of the script. Returns just src/ jl content for the parser
# pass; everything else is in the PackageContents.
function walk_tar_for_jl(tar_bytes::Vector{UInt8})
    pc = walk_tarball(tar_bytes)
    return pc.src_files, pc
end

# === Parser-pass per package ===

struct ParseResult
    n_files::Int
    n_pass::Int
    n_fail::Int
    jl_total_bytes::Int
    first_error::String
end

function parse_jl_files(files::Dict{String, Vector{UInt8}})::ParseResult
    n_files = length(files)
    n_pass = 0
    n_fail = 0
    total_bytes = 0
    first_err = ""
    for (path, bytes) in files
        total_bytes += length(bytes)
        src = String(copy(bytes))
        try
            parseJuAST(src, path)
            n_pass += 1
        catch e
            n_fail += 1
            if isempty(first_err)
                first_err = first(string(e), 280)
            end
        end
    end
    return ParseResult(n_files, n_pass, n_fail, total_bytes, first_err)
end

# === Main loop ===

function load_pool()
    isfile(POOL_FILE) || error("active pool not found: $POOL_FILE — save one from stage 1 first")
    return open(JSON.parse, POOL_FILE)
end

function write_progress(processed::Int, total::Int, current::AbstractString,
                        started::Float64; processed_this_run::Int=0, skipped_cached::Int=0)
    open(PROGRESS_FILE, "w") do io
        JSON.print(io, Dict(
            "processed" => processed,
            "processed_this_run" => processed_this_run,
            "skipped_cached" => skipped_cached,
            "total" => total,
            "current" => current,
            "started" => started,
            "elapsed_sec" => time() - started,
        ))
    end
end

# Aggregate-summary helper. Reads every per-uuid result file and emits
# data/parse_pass.json so the dashboard has a single artifact to load.
function rebuild_summary(pool, started::Float64; finished::Bool=false)
    results = Vector{Dict{String,Any}}()
    for f in readdir(RESULTS_DIR; sort=true)
        endswith(f, ".json") || continue
        try
            push!(results, open(JSON.parse, joinpath(RESULTS_DIR, f)))
        catch
            # corrupt partial file — skip
        end
    end
    open(SUMMARY_FILE, "w") do io
        JSON.print(io, Dict(
            "pool_size" => pool["package_count"],
            "finished" => finished,
            "started" => started,
            "elapsed_sec" => time() - started,
            "results" => results,
        ))
    end
end

# Write one per-package result atomically (tmp + rename) so a kill mid-write
# can't leave a half-JSON file behind.
function save_result(uuid::AbstractString, rec::Dict)
    p = result_path(uuid)
    tmp = p * ".part"
    open(tmp, "w") do io
        JSON.print(io, rec)
    end
    mv(tmp, p; force=true)
end

function main()
    pool = load_pool()
    pkgs = pool["packages"]
    n = length(pkgs)
    mkpath(RESULTS_DIR)
    isdir(TAR_DIR) || error("tarball cache not found: $TAR_DIR — run fetch_tarballs.jl first")
    started = time()
    @info "Parse pass starting" pool_size=n force_reprocess=FORCE_REPROCESS

    processed_this_run = 0
    skipped_cached = 0

    for (i, pkg) in enumerate(pkgs)
        name = pkg["name"]
        uuid = pkg["uuid"]
        version = pkg["latest_version"]

        if i % PROGRESS_EVERY == 1 || i == n
            write_progress(i-1, n, name, started;
                            processed_this_run=processed_this_run,
                            skipped_cached=skipped_cached)
        end

        rp = result_path(uuid)
        if !FORCE_REPROCESS && isfile(rp)
            skipped_cached += 1
            continue
        end

        tarpath = tarball_path(uuid)
        if !isfile(tarpath)
            save_result(uuid, Dict("name"=>name,"uuid"=>uuid,"version"=>version,
                                    "status"=>"no_cached_tarball"))
            processed_this_run += 1
            continue
        end

        bytes = try
            read(tarpath)
        catch e
            save_result(uuid, Dict("name"=>name,"uuid"=>uuid,"version"=>version,
                                    "status"=>"read_failed","err"=>first(string(e),200)))
            processed_this_run += 1
            continue
        end

        decompressed = try
            transcode(GzipDecompressor, bytes)
        catch e
            save_result(uuid, Dict("name"=>name,"uuid"=>uuid,"version"=>version,
                                    "status"=>"gunzip_failed","err"=>first(string(e),200),
                                    "tar_bytes"=>length(bytes)))
            processed_this_run += 1
            continue
        end

        pc = walk_tarball(decompressed)
        if isempty(pc.src_files)
            save_result(uuid, Dict("name"=>name,"uuid"=>uuid,"version"=>version,
                                    "status"=>"no_src_jl",
                                    "tar_bytes"=>length(bytes),
                                    "n_total_files"=>pc.n_total_files))
            processed_this_run += 1
            continue
        end

        pr = parse_jl_files(pc.src_files)
        fp = fingerprint_srcfiles(pc.src_files)
        n_test_loc = test_loc(pc.test_files)

        save_result(uuid, Dict(
            "name"               => name,
            "uuid"               => uuid,
            "version"            => version,
            "status"             => pr.n_fail == 0 ? "parse_ok" : "parse_partial",
            "tar_bytes"          => length(bytes),
            "jl_total_bytes"     => pr.jl_total_bytes,

            # Parse results
            "n_files"            => pr.n_files,
            "n_pass"             => pr.n_pass,
            "n_fail"             => pr.n_fail,
            "first_error"        => pr.first_error,

            # File-count fingerprint
            "n_src_jl"           => pc.n_src_jl,
            "n_test_jl"          => pc.n_test_jl,
            "n_docs_jl"          => pc.n_docs_jl,
            "n_ext_jl"           => pc.n_ext_jl,
            "n_other_jl"         => pc.n_other_jl,
            "n_total_files"      => pc.n_total_files,

            # Structural flags
            "has_project_toml"   => pc.has_project_toml,
            "has_manifest_toml"  => pc.has_manifest_toml,
            "has_readme"         => pc.has_readme,
            "has_runtests"       => pc.has_runtests,
            "has_docs_dir"       => pc.has_docs_dir,
            "has_ext_dir"        => pc.has_ext_dir,
            "has_compat_helper"  => pc.has_compat_helper,

            # Test code volume
            "test_loc"           => n_test_loc,

            # Source code fingerprint
            "loc"                => fp.loc,
            "comment_lines"      => fp.comment_lines,
            "blank_lines"        => fp.blank_lines,
            "function_count"     => fp.function_count,
            "struct_count"       => fp.struct_count,
            "mutable_struct_count"=> fp.mutable_struct_count,
            "typeannot_count"    => fp.typeannot_count,
            "where_count"        => fp.where_count,
            "union_count"        => fp.union_count,
            "any_token_count"    => fp.any_token_count,
            "macro_count"        => fp.macro_count,
            "generated_count"    => fp.generated_count,
            "nospecialize_count" => fp.nospecialize_count,
            "inbounds_count"     => fp.inbounds_count,
            "eval_count"         => fp.eval_count,
            "ccall_count"        => fp.ccall_count,
            "nocheck_count"      => fp.nocheck_count,
        ))
        processed_this_run += 1

        # Rebuild aggregate summary every ~50 processed for live dashboard.
        if processed_this_run % 50 == 0
            rebuild_summary(pool, started)
        end
    end

    write_progress(n, n, "(done)", started;
                    processed_this_run=processed_this_run,
                    skipped_cached=skipped_cached)
    rebuild_summary(pool, started; finished=true)
    @info "Parse pass complete" elapsed=time()-started processed_this_run skipped_cached
    return 0
end

if abspath(PROGRAM_FILE) == @__FILE__
    exit(main())
end
