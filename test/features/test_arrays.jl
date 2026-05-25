# Untyped vector literals `[a, b, c]`.

@testset "untyped vector literal [a, b, c]" begin
    expect_clean("arrays/vec_homo_ok.jl", :VecHomo)
    expect_error("arrays/vec_mixed_bad.jl", :VecMixed, "ElementNonSameType")
    expect_error("arrays/vec_empty_bad.jl", :VecEmpty, "EmptyAnyArray")
end
