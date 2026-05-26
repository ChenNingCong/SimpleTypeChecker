module WhereBracesMultiOk
# Multiple sparams in braces: one bounded, one unbounded.
function pair_first(x::T, y::S)::T where {T<:Real, S}
    return x
end
end
