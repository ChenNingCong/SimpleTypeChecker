# Passthrough macros (`@inbounds`, `@inline`, `@fastmath`, `@views`, ...).

@testset "passthrough macros (@inbounds, @views, @inline, @fastmath, ...)" begin
    expect_clean("macros/macro_ok.jl", :MacroOk)
end
