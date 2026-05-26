module SimpleTypeChecker

# Temporary upstream patch — bypassed automatically once upstream lands.
include("./patch.jl")

include("./toplevel.jl")

# `_apply_juliasyntax_patch!()` is intentionally NOT called from
# `__init__`. The patch mutates `Base.JuliaSyntax`'s method table
# (adds a `where F` `parse_block` and deletes the untyped methods),
# which invalidates JuliaSyntax's precompile cache. The unpatched
# `parse_block` is functionally correct at normal runtime — the patch
# is only needed for `juliac --trim` reachability analysis, and is
# applied directly from `bin/standalone_entry.jl` for that path.
__init__() = nothing

end # module SimpleTypeChecker
