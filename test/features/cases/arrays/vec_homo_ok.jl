module VecHomo
# Homogeneous untyped vector literals — element type is inferred uniformly.
f()::Vector{Int} = [1, 2, 3]
g()::Vector{Float64} = [1.0, 2.0]
end
