module OriginChain
# Abstract value flows through an assignment before reaching the call site.
# The trace should hop from the variable use back to where it was assigned.
struct Holder
    f::Function
end
function go(h::Holder, x::Int)::Int
    g = h.f          # ← abstract value enters here
    return g(x)      # ← reported here; trace points back to line 8
end
end
