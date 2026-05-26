module OriginField
# A struct with an abstract field type. Calling through the field traces
# the abstract argument back to the field access at the origin site.
struct Holder
    f::Function     # ← abstract field
end
function go(h::Holder, x::Int)::Int
    return h.f(x)
end
end
