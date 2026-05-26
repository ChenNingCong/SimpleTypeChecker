module WhereBracesOk
# `where {T<:Bound}` braces form. Should parse cleanly — previously the
# checker rejected this with "Parameters (a.k.a where) … only contain
# symbol or a bound" because the braces wrapper wasn't unwrapped.
function dot3(u::Vector{T}, v::Vector{T})::T where {T<:AbstractFloat}
    s::T = zero(T)
    for i in 1:length(u)
        s = s + u[i] * v[i]
    end
    return s
end
end
