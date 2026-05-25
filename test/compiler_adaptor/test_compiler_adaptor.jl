# Unit tests for the CompilerAdaptor interface.
#
# These tests exist to make Julia version bumps cheap: re-run this file on the
# new Julia, fix anything that breaks here, and the rest of the codebase should
# stay green. If you add to the adaptor's public API, add a test here.

using Test
using SimpleTypeChecker
using SimpleTypeChecker.Inference: MethodHandle, MethodDef,
    inferReturnType, inferKeywordMethodHandles,
    isInPureContext, currentWorld,
    methodHandleDef, methodHandleModule, methodHandleSpecTypes,
    methodHandleSparamCount, methodHandleSparamValue,
    methodHandleSpecTypesLength, methodHandleSpecTypesParam,
    makeSparamCompileType, makeArgCompileType,
    methodDefSig, isConcreteMethodDef, methodDefCodeInfo,
    codeInfoMethodHandle,
    GlobalContext, CompileType, makeType

# Sample functions to exercise the inferencer
adaptor_add(x::Int, y::Int) = x + y
adaptor_identity(x::T) where {T} = x
adaptor_kw(x::Int; scale::Int = 2) = x * scale

# Helper: build a MethodHandle for a given signature, using only the adaptor's
# public surface. inferKeywordMethodHandles is a thin wrapper over
# specialize_method, so it serves equally well as a generic MI factory.
function fixture_mi(sig)
    ctx = GlobalContext()
    mis = inferKeywordMethodHandles(ctx, sig)
    @assert !isempty(mis) "no MethodHandle for $sig"
    return first(mis)
end

# Nested-call fixture: exercises that inferReturnType queues nested user MIs.
adaptor_helper(x::Int) = x * 3
adaptor_outer(x::Int, y::Int) = adaptor_helper(x) + adaptor_helper(y)

@testset "CompilerAdaptor" begin

    @testset "type aliases" begin
        @test MethodHandle === Core.MethodInstance
        @test MethodDef === Core.Method
    end

    @testset "runtime status" begin
        @test isInPureContext() == false
        @test currentWorld() isa UInt
        @test currentWorld() > 0
    end

    @testset "inferReturnType — concrete signature" begin
        ctx = GlobalContext()
        rts = inferReturnType(ctx, Tuple{typeof(adaptor_add), Int, Int})
        @test rts isa Vector{CompileType}
        @test length(rts) == 1
        @test rts[1].typ === Int
    end

    @testset "inferReturnType — parametric signature" begin
        ctx = GlobalContext()
        rts = inferReturnType(ctx, Tuple{typeof(adaptor_identity), Int})
        @test length(rts) == 1
        @test rts[1].typ === Int
    end

    @testset "inferReturnType — builtin (Core.Builtin) branch" begin
        ctx = GlobalContext()
        # `===` is a Core.Builtin, exercises the isa(f, Core.Builtin) branch
        rts = inferReturnType(ctx, Tuple{typeof(===), Int, Int})
        @test length(rts) == 1
        @test rts[1].typ === Bool
    end

    @testset "inferReturnType — (f, tt) overload" begin
        ctx = GlobalContext()
        rts = inferReturnType(ctx, adaptor_add, (Int, Int))
        @test length(rts) == 1
        @test rts[1].typ === Int
    end

    @testset "inferKeywordMethodHandles — kwarg dispatch" begin
        ctx = GlobalContext()
        # The keyword-call signature uses Core.kwcall with a NamedTuple
        kwsig = Tuple{typeof(Core.kwcall), NamedTuple{(:scale,), Tuple{Int}}, typeof(adaptor_kw), Int}
        mis = inferKeywordMethodHandles(ctx, kwsig)
        @test mis isa Vector{MethodHandle}
        @test length(mis) >= 1
        @test all(mi -> mi isa MethodHandle, mis)
    end

    @testset "MethodHandle accessors — concrete method" begin
        mi = fixture_mi(Tuple{typeof(adaptor_add), Int, Int})
        @test mi isa MethodHandle

        @test methodHandleDef(mi) isa MethodDef
        @test methodHandleModule(mi) isa Module
        # specTypes should be a Tuple type
        @test methodHandleSpecTypes(mi) <: Tuple
        # adaptor_add has no static parameters
        @test methodHandleSparamCount(mi) == 0
        # specTypes parameters: function + 2 args = 3
        @test methodHandleSpecTypesLength(mi) == 3
        # First parameter is the function type, then Int, Int
        @test methodHandleSpecTypesParam(mi, 1) === typeof(adaptor_add)
        @test methodHandleSpecTypesParam(mi, 2) === Int
        @test methodHandleSpecTypesParam(mi, 3) === Int
    end

    @testset "MethodHandle accessors — parametric (where T)" begin
        mi = fixture_mi(Tuple{typeof(adaptor_identity), Int})
        @test mi isa MethodHandle
        # sparam {T} bound to Int
        @test methodHandleSparamCount(mi) == 1
        @test methodHandleSparamValue(mi, 1) === Int
        @test methodHandleSpecTypesParam(mi, 1) === typeof(adaptor_identity)
        @test methodHandleSpecTypesParam(mi, 2) === Int
    end

    @testset "makeSparamCompileType / makeArgCompileType" begin
        mi = fixture_mi(Tuple{typeof(adaptor_identity), Int})

        sct = makeSparamCompileType(mi, 1)
        @test sct isa CompileType
        @test sct.val === Int   # T is bound to Int as a constant

        act = makeArgCompileType(mi, 2)  # second parameter (after function type) is the x::T arg
        @test act isa CompileType
        @test act.typ === Int
    end

    @testset "inferReturnType queues nested user MethodInstances" begin
        # adaptor_outer calls adaptor_helper twice; the helper MI should land
        # in ctx.queue via the interp.codegen iteration at the end of inferReturnType.
        # (Base built-ins like +(::Int,::Int) do NOT get queued — they're inlined
        # by Julia and never create a separate MI in codegen.)
        ctx = GlobalContext()
        inferReturnType(ctx, Tuple{typeof(adaptor_outer), Int, Int})
        @test !isempty(ctx.queue)
        helper_seen = any(q -> methodHandleSpecTypesParam(q.mi, 1) === typeof(adaptor_helper), ctx.queue)
        @test helper_seen
    end

    @testset "MethodDef accessors" begin
        ms = methods(adaptor_add)
        @test !isempty(ms)
        meth = first(ms)::MethodDef
        @test methodDefSig(meth) <: Tuple
        @test isConcreteMethodDef(meth) == true

        ci = methodDefCodeInfo(meth)
        @test ci isa Core.CodeInfo
        @test codeInfoMethodHandle(ci) isa MethodHandle
    end

    @testset "isConcreteMethodDef — non-concrete should be false" begin
        # adaptor_identity has a where {T} clause → its sig is a UnionAll
        ms = methods(adaptor_identity)
        meth = first(ms)::MethodDef
        @test methodDefSig(meth) isa UnionAll
        @test isConcreteMethodDef(meth) == false
    end

end
