# Coverage-broadening test: parse/checker features that real Julia code
# routinely uses but the checker previously refused (where {T<:Bound}, op=
# augmented assignment, do-block calls).

@testset "syntax coverage (where {…}, op=, do-blocks)" begin
    expect_clean("syntax_coverage/where_braces_ok.jl", :WhereBracesOk)
    expect_clean("syntax_coverage/where_braces_multi_ok.jl", :WhereBracesMultiOk)
    expect_clean("syntax_coverage/opeq_ok.jl", :OpeqOk)
    # The bad-op-eq case must produce *some* error (the kind depends on
    # how the dispatch failure surfaces); just assert the path doesn't
    # spuriously pass.
    @test any_error(check_case("syntax_coverage/opeq_bad.jl", :OpeqBad))
    expect_clean("syntax_coverage/do_ok.jl", :DoOk)
end

@testset "syntax coverage (signature forms)" begin
    # `@nospecialize(x)` / `@nospecialize x` annotations in parameter slot.
    expect_clean("syntax_coverage/nospecialize_ok.jl", :NospecializeOk)
    # `function Foo{K,V}(...)` parametric constructor name.
    expect_clean("syntax_coverage/parametric_ctor_ok.jl", :ParametricCtorOk)
    # `function Base.write(...)` qualified-name method extension.
    expect_clean("syntax_coverage/qualified_fn_ok.jl", :QualifiedFnOk)
    # `T[expr for x in xs]` typed comprehension — result must be Vector{T}
    # so a `Vector{Int}`-typed downstream call dispatches concretely.
    expect_clean("syntax_coverage/typed_comprehension_ok.jl", :TypedComprehensionOk)
end

@testset "value-flow origin tracing in dispatch errors" begin
    # Abstract callee comes directly from a field access. The trace
    # reports the field's name on the same line.
    expect_error("syntax_coverage/origin_field.jl", :OriginField, "FunCallArgs")
    # Abstract value flows through one assignment before reaching the
    # call site. The trace should hop back to the field-access origin.
    expect_error("syntax_coverage/origin_chain.jl", :OriginChain, "FunCallArgs")
end

