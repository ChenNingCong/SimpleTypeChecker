module OpeqOk
# Augmented assignments parse with head=:op=, args=[lhs, op, rhs] —
# previously rejected as "Assignment expression should only have 2 children".
function sum1(n::Int)::Int
    s::Int = 0
    i::Int = 1
    while i <= n
        s += i
        i += 1
    end
    return s
end
function arr_inc!(v::Vector{Int}, i::Int)::Nothing
    v[i] += 1
    return nothing
end
end
