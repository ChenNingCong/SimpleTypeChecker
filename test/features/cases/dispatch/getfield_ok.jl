module DispatchGetfield
# `p.a` lowers to `Core.getfield`, a Builtin. Static.
struct P
    a::Int
    b::Float64
end
function f(p::P)::Int
    return p.a
end
end
