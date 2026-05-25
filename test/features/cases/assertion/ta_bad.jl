module TaBad
# Asserted type incompatible with the value's static type. Expected to flag
# InferenceErrorAssignInitIncompatible.
function f(x::Float64)
    y = x::Int
    return y
end
end
