module OpeqBad
# `s += "x"` against `s::Int` exercises the update path with a type
# mismatch — the augmented assignment must still flow through dispatch
# so the bad call site is caught.
function bad()::Int
    s::Int = 0
    s += "x"
    return s
end
end
