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
# Applied from `__init__()` so it runs at load time, not precompile
# time (Julia 1.12 forbids `eval` into another module during precompile,
# and forbids method overwriting during precompile). NOTE: this means
# `juliac --trim=safe` still flags the unspecialized method bodies as
# verifier errors, since it snapshots the method table at compile time.
# Use `--trim=unsafe-warn` to build anyway — the patched binary then
# runs correctly because the patch fires from `__init__()` at startup.
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
