module NospecializeOk
# `@nospecialize` in parameter slot is a method-table annotation, not a
# real wrapper. Previously the parser reported "Invalid argument
# definition" because constructParam didn't peel the macrocall.
function f(@nospecialize(x))::Int
    return 1
end

# Space form parses to the same AST as the parens form — both must work.
function g(@nospecialize y)::Int
    return 2
end

# Mixed with typed and plain args.
function h(a::Int, @nospecialize(b), c::String)::Int
    return a
end

function caller()::Int
    return f(:sym) + g(1.0) + h(1, nothing, "x")
end
end
