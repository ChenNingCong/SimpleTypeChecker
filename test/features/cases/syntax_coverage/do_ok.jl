module DoOk
# `f(args) do x; body end` lowers to `f((x,) -> body, args)`. Previously
# the checker errored "Unsupported AST type do". Use a do-block with a
# concrete callee whose first-arg-is-Function dispatch resolves cleanly.
function f()::Int
    xs::Vector{Int} = [1, 2, 3]
    n::Int = sum(xs) do x
        x * 2
    end
    return n
end
end
