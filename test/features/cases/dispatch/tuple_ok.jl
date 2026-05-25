module DispatchTuple
# `(1, 2.0, "x")` lowers to `Core.tuple` — a Builtin. Static, even with a
# heterogeneous element list (the result type is concrete Tuple{...}).
function f()::Tuple{Int, Float64, String}
    return (1, 2.0, "x")
end
end
