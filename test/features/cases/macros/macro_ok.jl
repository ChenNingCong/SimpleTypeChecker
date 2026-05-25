module MacroOk
# Passthrough macros that don't change semantics — @inbounds, @inline,
# @fastmath, @views — should be unwrapped transparently.
function f(xs::Vector{Int})::Int
    a = @inbounds xs[1]
    b = @inline a + 1
    c = @fastmath Float64(b) * 2.0
    return Int(c)
end
end
