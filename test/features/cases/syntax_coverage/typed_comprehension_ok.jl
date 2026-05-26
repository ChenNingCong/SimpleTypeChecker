module TypedComprehensionOk

# `T[expr for x in xs]` returns `Vector{T}`. Previously rejected with
# `JTC-UNSUPPORTED: Unsupported AST type typed_comprehension`.

function dummy(xs::Vector{Int})::Int
    return length(xs)
end

function caller()::Int
    xs = Int[1, 2, 3]
    # The comprehension's result must type as Vector{Int} for `dummy`
    # (which expects Vector{Int}) to dispatch concretely.
    ys = Int[x for x in xs]
    return dummy(ys)
end

end
