module TryBasicOk
# Simplest try/catch: clean body, catch binds the exception to `e` (treated
# as Any inside the catch block).
function safe_divide(a::Int, b::Int)::Int
    try
        return div(a, b)
    catch e
        return 0
    end
end
end
