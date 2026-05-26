module PushEltypeMismatch
# Strict-mode test: push!(v::Vector{Float64}, 1) is rejected because
# Int requires implicit convert to Float64. Default rtctx approves
# (Julia's dispatch is happy with this). Strict rtctx flags it.
function f()
    v = Float64[]
    push!(v, 1)        # rejected in strict mode (Int vs Float64)
    push!(v, 2.5)      # approved (Float64 vs Float64)
    return v
end
end
