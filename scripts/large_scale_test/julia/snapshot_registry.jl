#!/usr/bin/env julia
#
# Stage 0: snapshot the General registry to JSON.
#
# Usage:
#   julia --project=@. snapshot_registry.jl [out_path]
#
# Reads the user's installed General registry (whatever Pkg.Registry can
# reach — works for both extracted-dir and tarball-form registries) and
# emits one JSON record per package:
#
#   { "name", "uuid", "latest_version", "repo", "deps_uuids", "subdir" }
#
# Idempotent: if out_path exists and its `registry_revision` matches the
# current registry tree hash, the script is a no-op.

using Pkg
using Pkg.Registry: reachable_registries, registry_info
using Pkg.Versions: VersionSpec
import Pkg.Versions

# The Julia version we plan to use for analysis. We surface "is the
# latest registered version of this package compatible with this Julia?"
# as a per-package boolean.
const TARGET_JULIA = v"1.12.0"

# Tiny hand-rolled JSON writer. Avoids pulling in JSON.jl as a dep for a
# script that just dumps a flat list of records.
struct JSONOut
    io::IO
end

function jwrite_str(io::IO, s::AbstractString)
    print(io, '"')
    for c in s
        if c == '"'
            print(io, "\\\"")
        elseif c == '\\'
            print(io, "\\\\")
        elseif c == '\n'
            print(io, "\\n")
        elseif c == '\r'
            print(io, "\\r")
        elseif c == '\t'
            print(io, "\\t")
        elseif c < ' '
            print(io, "\\u", lpad(string(UInt32(c), base=16), 4, '0'))
        else
            print(io, c)
        end
    end
    print(io, '"')
end

function jwrite(io::IO, v::AbstractString); jwrite_str(io, v); end
function jwrite(io::IO, v::Bool);           print(io, v ? "true" : "false"); end
function jwrite(io::IO, v::Nothing);        print(io, "null"); end
function jwrite(io::IO, v::Integer);        print(io, v); end
function jwrite(io::IO, v::AbstractVector)
    print(io, '[')
    for (i, x) in enumerate(v)
        i > 1 && print(io, ',')
        jwrite(io, x)
    end
    print(io, ']')
end
function jwrite(io::IO, v::AbstractDict)
    print(io, '{')
    first = true
    for (k, val) in v
        first ? (first = false) : print(io, ',')
        jwrite_str(io, string(k))
        print(io, ':')
        jwrite(io, val)
    end
    print(io, '}')
end

function find_general()
    regs = reachable_registries()
    for r in regs
        if r.name == "General"
            return r
        end
    end
    error("General registry not found in reachable registries. Run `Pkg.Registry.add(\"General\")` first.")
end

# Returns Vector{Dict} — one entry per package in the General registry.
function snapshot(reg)
    out = Vector{Dict{String,Any}}()
    sizehint!(out, length(reg.pkgs))
    for (uuid, pkgentry) in reg.pkgs
        info = try
            registry_info(pkgentry)
        catch e
            @warn "registry_info failed" pkg=pkgentry.name err=e
            continue
        end
        # info.version_info is Dict{VersionNumber, VersionInfo}
        versions = sort!(collect(keys(info.version_info)))
        isempty(versions) && continue
        latest = last(versions)
        # Git tree-sha for the latest version. We need this to fetch the
        # source tarball from the Pkg server at stage 2.
        tree_sha = string(info.version_info[latest].git_tree_sha1)
        # Deps as UUIDs — names are derivable later by joining on uuid.
        # info.deps is Dict{VersionRange, Dict{String, UUID}}.
        # We take deps for the latest version: any VersionRange that
        # contains `latest`.
        dep_uuids = Set{String}()
        for (vr, dmap) in info.deps
            if latest in vr
                for (_name, du) in dmap
                    push!(dep_uuids, string(du))
                end
            end
        end
        # Julia compat for the latest version. info.compat is
        # Dict{VersionRange, Dict{String, VersionSpec}}. We look up
        # "julia" entries whose key contains the latest version.
        # Also extract the lower bound — packages requiring Julia >= 1.10
        # are typically more actively maintained than ones still claiming
        # 1.0+ or 0.7+ compat.
        julia_compat_str = ""
        julia_supports_target = false
        julia_lower_major = 0
        julia_lower_minor = 0
        for (vr, cmap) in info.compat
            if latest in vr
                if haskey(cmap, "julia")
                    spec = cmap["julia"]
                    julia_compat_str = string(spec)
                    if TARGET_JULIA in spec
                        julia_supports_target = true
                    end
                    # Walk the spec's ranges to find the minimum lower bound.
                    lo_seen = nothing
                    for r in spec.ranges
                        # r is a VersionRange; r.lower is a VersionBound.
                        if !isempty(r.lower.t)
                            t = r.lower.t
                            v = (t[1], length(t) >= 2 ? t[2] : 0)
                            if lo_seen === nothing || v < lo_seen
                                lo_seen = v
                            end
                        end
                    end
                    if lo_seen !== nothing
                        julia_lower_major, julia_lower_minor = lo_seen
                    end
                    break
                end
            end
        end
        push!(out, Dict{String,Any}(
            "name"                  => pkgentry.name,
            "uuid"                  => string(uuid),
            "latest_version"        => string(latest),
            "tree_sha"              => tree_sha,
            "repo"                  => something(info.repo, ""),
            "subdir"                => something(info.subdir, ""),
            "deps_uuids"            => sort!(collect(dep_uuids)),
            "julia_compat"          => julia_compat_str,
            "julia_supports_target" => julia_supports_target,
            "julia_lower_major"     => julia_lower_major,
            "julia_lower_minor"     => julia_lower_minor,
        ))
    end
    sort!(out, by = d -> d["name"])
    return out
end

function main(out_path::AbstractString)
    @info "Locating General registry..."
    reg = find_general()
    @info "Found registry" name=reg.name path=reg.path tree_info=string(reg.tree_info)

    rev = string(reg.tree_info)

    if isfile(out_path)
        # Cheap idempotency check: parse the first 1024 bytes looking for
        # the registry_revision field. Avoids a JSON parser dep here.
        head = read(out_path, String)
        if occursin("\"registry_revision\":\"$rev\"", head)
            println("Up to date — $(out_path) already at revision $rev")
            return 0
        end
    end

    pkgs = snapshot(reg)
    @info "Snapshotting" count=length(pkgs)

    open(out_path, "w") do io
        print(io, "{")
        jwrite_str(io, "registry_revision"); print(io, ":")
        jwrite_str(io, rev); print(io, ",")
        jwrite_str(io, "registry_path"); print(io, ":")
        jwrite_str(io, reg.path); print(io, ",")
        jwrite_str(io, "package_count"); print(io, ":")
        print(io, length(pkgs)); print(io, ",")
        jwrite_str(io, "packages"); print(io, ":")
        jwrite(io, pkgs)
        print(io, "}\n")
    end
    println("Wrote $(length(pkgs)) packages to $out_path")
    return 0
end

if abspath(PROGRAM_FILE) == @__FILE__
    out = length(ARGS) >= 1 ? ARGS[1] :
        joinpath(@__DIR__, "..", "data", "registry_snapshot.json")
    out = abspath(out)
    mkpath(dirname(out))
    exit(main(out))
end
