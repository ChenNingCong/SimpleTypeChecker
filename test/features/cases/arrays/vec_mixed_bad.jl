module VecMixed
# Heterogeneous untyped vector literal — expected to flag
# InferenceErrorElementNonSameType.
f() = [1, 2.0, 3]
end
