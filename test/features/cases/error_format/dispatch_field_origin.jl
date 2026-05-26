module DispatchFieldOrigin

# Mirrors the exact shape the checker fires on in its own self-check at
# Inference.jl:1581 — a local variable receives a field of `Any` type and
# is then passed to a call expecting a concrete type.
#
# Expected diagnostic (rustc-shape):
#
#   error[JTC-DISPATCH]: call has an argument of abstract type
#      --> .../dispatch_field_origin.jl:<call line>:<col>
#       |
#    L  |     consume(vv, "x")
#       |             ^^ argument `vv` has type Any
#       |
#   note: `vv` became Any here
#      --> .../dispatch_field_origin.jl:<assign line>:<col>
#       |
#    M  |     vv = h.val
#       |          ----- field access `.val`
#       |
#   help: annotate `vv` with a concrete type, or guard with `vv isa T` before the call

struct Holder
    val
end

function consume(x::Int, y::String)::Nothing
    return nothing
end

function caller(h::Holder)::Nothing
    vv = h.val
    consume(vv, "x")
    return nothing
end

end
