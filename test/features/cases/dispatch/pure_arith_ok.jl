module DispatchPureArith
# `1 + 2`, `x * y` etc. lower to Core intrinsics (add_int, mul_int) which are
# static. Nothing should fire.
function f(x::Int, y::Int)::Int
    return x + y * 2
end
function g()::Int
    return 1 + 2
end
end
