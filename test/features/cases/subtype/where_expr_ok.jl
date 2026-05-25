module WhereExprOk
# `where` in expression context produces a constant UnionAll when usable.
f()::Type = Vector{T} where T
g() = Int <: (Vector{T} where T)
end
