module DispatchUserChain
# User-defined functions calling each other with concrete types. All
# dispatch is statically resolvable; no diagnostics should fire.
struct Pair2
    a::Int
    b::Int
end
add_pair(p::Pair2)::Int = p.a + p.b
double(x::Int)::Int = x * 2
function f(p::Pair2)::Int
    return double(add_pair(p))
end
end
