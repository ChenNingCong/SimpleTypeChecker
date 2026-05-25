module DispatchStructField
# Concrete struct, concrete-field access + arithmetic. Nothing dynamic
# anywhere in the chain — verifies a slightly richer "clean" baseline
# than the bare pure_arith case.
struct Box
    value::Int
    factor::Float64
end
function scaled(b::Box)::Float64
    return Float64(b.value) * b.factor
end
end
