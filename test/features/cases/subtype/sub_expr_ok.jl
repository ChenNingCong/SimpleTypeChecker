module SubExprOk
# T1 <: T2 produces a Bool. When both sides are constant types the result is
# a compile-time constant.
const isnum = Int <: Real
f()::Bool = Int <: Real
end
