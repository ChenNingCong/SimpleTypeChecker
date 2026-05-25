module SimpleTypeChecker

# Temporary upstream patch — bypassed automatically once upstream lands.
include("./patch.jl")

include("./toplevel.jl")

__init__() = _apply_juliasyntax_patch!()

end # module SimpleTypeChecker
