# `<:` and `where` in expression context.

@testset "subtype operator `<:` in expression context" begin
    expect_clean("subtype/sub_expr_ok.jl", :SubExprOk)
end

@testset "`where` in expression context" begin
    expect_clean("subtype/where_expr_ok.jl", :WhereExprOk)
end
