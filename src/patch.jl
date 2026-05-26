# Temporary upstream patch for Base.JuliaSyntax (Julia 1.12+).
#
# `parse_block` doesn't propagate the callable type into its body, so the
# downstream call `parse_block_inner(ps, down)` looks like
# `parse_block_inner(::ParseState, ::Function)` to the compiler — an
# unresolved call. Without this patch the runtime hits a `MethodError`
# on any input that exercises a block-parsing path (try/catch, let,
# begin, function bodies, if/elseif, ...) in a trimmed binary.
#
# Fix: add `where {F <: Function}` so each callsite specializes
# `parse_block` per callable, propagating into `parse_block_inner`'s
# existing `where F` specialization. Delete the unspecialized methods
# so dispatch can only reach the typed ones.
#
# Upstream context: https://github.com/JuliaLang/JuliaSyntax.jl/pull/491
# was the canonical trim-friendliness pass — it added `where F` to
# `parse_block_inner` and typed `internal_error`'s Vararg, but missed
# `parse_block` itself. The gap is unpatched on JuliaSyntax.jl `main`
# and Julia `master` as of 2026-05-25.
#
# Applied at module-LOAD time (top-level, NOT `__init__`). Reason: the
# trim build (`juliac --trim`) does whole-program reachability analysis
# against the method table as it stands when our module finishes loading.
# `__init__` runs *after* that snapshot — in fact `__init__` cannot run
# inside a trimmed binary at all (the eval machinery is stripped). So
# patching from `__init__` is structurally too late for trim.
#
# Patching at top-level fails during `.ji` precompile generation
# (Julia 1.12 forbids cross-module `eval` there), so we guard with
# `JLOptions().outputji == C_NULL`. The parent module's `__init__`
# re-applies the patch as a fallback for the precompile cache path.
#
# Idempotence: the function bails early if every `parse_block` method
# already has a `UnionAll` signature, so double-application is safe.
#
# Delete this file once upstream lands the fix.

function _apply_juliasyntax_patch!()
    parse_block = Base.JuliaSyntax.parse_block
    if all(m -> m.sig isa UnionAll, methods(parse_block))
        return  # upstream already typed every method — bypass.
    end
    # In a trimmed binary at bootstrap, method redefinition into a Base
    # module raises "Binding invalidation is not permitted during
    # bootstrap." Silently no-op — the upstream gap remains, but at
    # least we don't crash. Normal Julia sessions take the success path.
    try
        block_kind = Base.JuliaSyntax.Kind("block")
        Base.eval(Base.JuliaSyntax, quote
            function parse_block(ps::ParseState, down::F = parse_eq, mark = position(ps)) where {F <: Function}
                parse_block_inner(ps, down)
                emit(ps, mark, $block_kind)
            end
        end)
        for m in collect(methods(parse_block))
            if !(m.sig isa UnionAll)
                Base.delete_method(m)
            end
        end
    catch
        # Trimmed-binary bootstrap or other frozen-world context.
    end
    return
end

# NOTE: a top-level `_apply_juliasyntax_patch!()` here is useless: when
# SimpleTypeChecker is precompiled (the normal `using` path), Julia
# loads the package from its `.ji` cache and DOES NOT re-execute
# top-level statements — only `__init__` runs. And even if we ran the
# top-level patch during precompile generation, the modification to
# Base.JuliaSyntax's method table belongs to Base, not us, so it gets
# discarded when the .ji is loaded into a fresh session.
#
# Two consequences:
#   - Normal `using SimpleTypeChecker`: `__init__` (in SimpleTypeChecker.jl)
#     re-applies the patch on every load. Works as before.
#   - `juliac --trim`: `__init__` doesn't run before reachability
#     analysis snapshots the method table. The standalone entry
#     (`bin/standalone_entry.jl`) is a plain script, NOT a precompiled
#     module, so its top-level DOES run at build time. We call
#     `_apply_juliasyntax_patch!()` directly there, after `using`,
#     so trim sees the typed `parse_block` methods.
