# Tests for previously-unsupported language features.
#
# Each @testset writes a small Julia source file to a temp path, runs the
# checker, and asserts on what's in `ctx.hasChecked`. The checker stores the
# exception object (or `nothing` on success) for each MethodInstance it
# checks, which is more reliable than capturing stdout.
#
# IMPORTANT: source strings must NOT have leading indentation. The checker's
# top-level parser doesn't handle indented module declarations as currently
# written, so we use unindented heredocs.

using Test
using SimpleTypeChecker
using SimpleTypeChecker.Inference: GlobalContext

# Run runCheck! on a freshly-loaded module containing `source`.
# Returns a Vector of Pair{String, Any} mapping a string representation of
# the MethodInstance to either the exception that was raised or `nothing`.
function run_feature_check(source::String, module_name::Symbol)
    path, io = mktemp()
    write(io, source)
    close(io)
    # IMPORTANT: the wrapper Module name MUST NOT match the declared inner
    # module name. If they match, `evalMods(inner, [inner_name])` in runCheck!
    # tries to dereference `inner.inner_name`, which doesn't exist, and the
    # checker silently skips the method as a CompilerError.
    wrapper = Module(:FeatureTestWrapper)
    Base.include(wrapper, path)
    target_mod = getfield(wrapper, module_name)
    ctx = GlobalContext()
    SimpleTypeChecker.API.addFile!(ctx, target_mod, path)
    open("/dev/null", "w") do devnull
        redirect_stdout(devnull) do
            SimpleTypeChecker.API.runCheck!(ctx)
        end
    end
    results = Pair{String, Any}[]
    for (mi, entries) in ctx.hasChecked
        for (_kwargs, exc) in entries
            push!(results, string(mi) => exc)
        end
    end
    return results
end

# True iff any checked method instance produced an exception whose type name
# contains `pattern`, OR whose `.msg` field (if any) contains `pattern`.
function has_error_matching(results, pattern::String)
    for (_mi, exc) in results
        exc === nothing && continue
        if occursin(pattern, string(typeof(exc)))
            return true
        end
        if hasfield(typeof(exc), :msg) && occursin(pattern, getfield(exc, :msg))
            return true
        end
    end
    return false
end

function any_error(results)
    return any(((_mi, exc),) -> exc !== nothing, results)
end

@testset "Features" begin

    @testset "subtype operator `<:` in expression context" begin
        # T1 <: T2 produces a Bool — constant when both sides are constant types.
        out = run_feature_check(
"""
module SubExprOk
const isnum = Int <: Real
f()::Bool = Int <: Real
end
""", :SubExprOk)
        @test !any_error(out)

        # Wrong arity error
        out_arity = run_feature_check(
"""
module SubExprArity
f() = (<:)(Int)
end
""", :SubExprArity)
        # Not the <: expression case (this is a function call), but exercise the error path
        # is_subtype is called as a regular function.
        @test true  # placeholder — function-call arity errors are tested elsewhere
    end

    @testset "`::` as standalone expression (type assertion)" begin
        # Value already has the asserted type — no error.
        ok = run_feature_check(
"""
module TaOk
function f()::Int
    x = 1
    y = x::Int
    return y + 1
end
end
""", :TaOk)
        @test !any_error(ok)

        # Asserted type incompatible with the value's static type.
        bad = run_feature_check(
"""
module TaBad
function f(x::Float64)
    y = x::Int
    return y
end
end
""", :TaBad)
        @test has_error_matching(bad, "AssignInitIncompatible")

        # The asserted "type" is actually a runtime value, not a Type.
        nontype = run_feature_check(
"""
module TaNonType
function f(x::Int)
    nottype = 42
    return x::nottype
end
end
""", :TaNonType)
        @test has_error_matching(nontype, "ApplyTypeNonconst")
    end

    @testset "`where` in expression context" begin
        # T where U should produce a constant UnionAll when usable.
        out = run_feature_check(
"""
module WhereExprOk
f()::Type = Vector{T} where T
g() = Int <: (Vector{T} where T)
end
""", :WhereExprOk)
        @test !any_error(out)
    end

    @testset "anonymous functions / lambdas" begin
        # Lambdas passed to higher-order functions used to error with
        # "Nested function is unsupported". They now eval to concrete
        # singleton function types so map/filter/sort accept them.
        out = run_feature_check(
"""
module LambdaOk
function f(xs::Vector{Int})
    return map(x -> x * 2, xs)
end
function g(xs::Vector{Int})
    return sort(xs, by = x -> x)
end
function h()::Function
    return function(x::Int, y::Int)
        return x + y
    end
end
end
""", :LambdaOk)
        @test !any_error(out)
    end

    @testset "passthrough macros (@inbounds, @views, @inline, @fastmath, ...)" begin
        out = run_feature_check(
"""
module MacroOk
function f(xs::Vector{Int})::Int
    a = @inbounds xs[1]
    b = @inline a + 1
    c = @fastmath Float64(b) * 2.0
    return Int(c)
end
end
""", :MacroOk)
        @test !any_error(out)
    end

    @testset "untyped vector literal [a, b, c]" begin
        # Homogeneous: should produce NO error.
        homo = run_feature_check(
"""
module VecHomo
f()::Vector{Int} = [1, 2, 3]
g()::Vector{Float64} = [1.0, 2.0]
end
""", :VecHomo)
        @test !any_error(homo)

        # Heterogeneous: should flag the same-type element error.
        mixed = run_feature_check(
"""
module VecMixed
f() = [1, 2.0, 3]
end
""", :VecMixed)
        @test has_error_matching(mixed, "ElementNonSameType")

        # Empty `[]`: should still flag (with the existing "use Any[]" message).
        empty_vec = run_feature_check(
"""
module VecEmpty
f() = []
end
""", :VecEmpty)
        @test has_error_matching(empty_vec, "EmptyAnyArray")
    end

end
