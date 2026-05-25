module LambdaOk
# Lambdas passed to higher-order functions used to error with "Nested
# function is unsupported". They now eval to concrete singleton function
# types so map/filter/sort accept them.
function f(xs::Vector{Int})
    return map(x -> x * 2, xs)
end
function g(xs::Vector{Int})
    return sort(xs, by = x -> x)
end
function h()::Function
    return function(x::Int, y::Int)
        return x + y
    end
end
end
