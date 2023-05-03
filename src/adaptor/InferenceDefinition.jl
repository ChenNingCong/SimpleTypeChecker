using ..SyntaxDefinition
using ..Utility
import ..Utility
struct CompileType
    isConst::Bool
    val::Any
    typ::Any
end

function makeBottomType()
    CompileType(true, nothing, Union{})
end

function isBottomType(t::CompileType)::Bool
    return t.typ == Union{}
end

function makeConstVal(val::Any)
    CompileType(true, val, Core.Typeof(val))
end

function makeType(val::Any)
    if Base.issingletontype(val)
        CompileType(true, val.instance, val)
    elseif val isa Type && val <: Type && val != Union{} && val isa DataType
        # this is incorrect, but how can we infer the type here?
        CompileType(true, val.parameters[1], val)
    else
        CompileType(false, nothing, val)
    end
end

function isConstVal(typ::CompileType)
    return typ.isConst
end

function lift(v::CompileType)
    if isConstVal(v)
        return makeType(v.val)
    else
        error("Internal error")
    end
end

#=

All the difficulties are caused by the fact that we need to maintain bimapping between data structure
GreenNode <-> JuAST
specialized JuExpr <-> FlowNode 

Due to specialization, a JuAST can associated with multiple JuExprs
=#
# A wrapper for any Julia value
mutable struct JuExpr
    const val::Any
    const ast::JuAST
    # this constructor is dangerous
    JuExpr() = new(nothing)
    JuExpr(val::Any, ast::JuAST) = new(val, ast)
end

function isValidJuExpr(ex::JuExpr)::Bool
    val = ex.val
    if val isa Nothing
        return true
    else
        return false
    end
end

# AST construction
abstract type TypedAST end
struct Var <: TypedAST 
    id::Symbol
end

#=
    local x
    local x::Int
    local x::Int = 1
    x::Int = 1
    all lower to this struct
=#
struct Declaration <: TypedAST
    id::Symbol
    typ::Maybe{JuExpr}
    rhs::Maybe{JuExpr}
end

struct DeclarationList <: TypedAST
    # vector of Declaration actually
    declares::Vector{JuExpr}
end


struct Assign <: TypedAST
    lhs::Symbol
    rhs::JuExpr
end

struct FunCall <: TypedAST
    f::JuExpr
    args::Vector{JuExpr}
    kwargs::Vector{Pair{Symbol, JuExpr}}
end

struct CurlyCall <: TypedAST
    f::JuExpr
    args::Vector{JuExpr}
end

struct Literal <: TypedAST
    val::CompileType
end

struct Block <: TypedAST
    stmts::Vector{JuExpr}
end

struct LetStmt <: TypedAST
    declares::Vector{Pair{Symbol, Maybe{JuExpr}}}
    body::JuExpr
end

struct IfStmt <: TypedAST
    branches::Vector{Pair{JuExpr, JuExpr}}
    else_::Maybe{JuExpr}
end

struct WhileStmt <: TypedAST
    cond::JuExpr
    body::JuExpr
end

struct ForStmt <: TypedAST
    var::Symbol
    iter::JuExpr
    body::JuExpr
end

struct Return <: TypedAST
    e::Maybe{JuExpr}
end

struct GetProperty <: TypedAST
    x::JuExpr
    p::Symbol
end

struct SetProperty <: TypedAST
    x::JuExpr
    p::Symbol
    v::JuExpr
end

struct ArrayRef <: TypedAST
    arr::JuExpr
    i::Vector{JuExpr}
end

struct ArraySet <: TypedAST
    arr::JuExpr
    i::Vector{JuExpr}
    v::JuExpr
end

struct TypedAssert <: TypedAST
    lhs::JuExpr
    rhs::JuExpr
end

struct ContinueStmt <: TypedAST end
struct BreakStmt <: TypedAST end

struct FunDef <: TypedAST
    f::Base.RefValue{Symbol}
    args::Vector{Pair{Symbol, Maybe{JuExpr}}}
    optargs::Vector{Pair{Symbol, Pair{Maybe{JuExpr}, JuExpr}}}
    kwargs::Vector{Pair{Symbol, Pair{Maybe{JuExpr}, JuExpr}}}
    params::Vector{Symbol}
    rt::Maybe{JuExpr}
    body::JuExpr
end

struct ModDef <: TypedAST
    name::Symbol
    stmts::Vector{JuExpr}
end

# End of AST construction 

# Flow node
# There are two types of flow node : value flow nodea and control flow node
# Value flow nodes store the evaluation result of a JuExpr (one-to-one mapping with JuExpr)
# Control flow nodes originate from some control flow facts (phi-node, pi-node), it has no corresponding JuExpr
@enum FlowNodeKind begin
    ValueFlowNodeBegin

    LiteralFlowNode
    AssignFlowNode
    UntypedDeclareFlowNode
    TypedDeclareFlowNode
    IfStmtFlowNode
    EmptyElseFlowNode
    BlockFlowNode
    EmptyBlockFlowNode
    VarFlowNode
    GlobalVarFlowNode
    FunCallNode
    CurlyCallNode
    GetPropertyNode
    SetPropertyNode
    ArrayRefNode
    ArraySetNode
    ReturnNode
    EmptyReturnNode
    ExpectedReturnNode
    ParamNode
    SparamNode
    WhileStmtFlowNode
    ForStmtVarNode # like a local variable declaration
    ForStmtFlowNode
    DeclarationListFlowNode
    BreakStmtFlowNode
    ContinueStmtFlowNode
    UninitializedFlowNode

    ControlFlowNodeBegin
    PhiFlowNode
    ForVarNode                 
    ForUpdateNode            # variable that loses constantness in the loop
    ConditionalFlowNode      # variable that defines conditionally
    PiNode                   # pi projection of a variable
    NegPiNode                # pi projection of a variable on a negative condition branch
end

mutable struct FlowNode
    const ex::JuExpr
    const nodeKind::FlowNodeKind
    const source::Vector{FlowNode}
    const typ::CompileType
    # record the bottom source of the node
    # can have multiple sources
    const isInitialized::Bool
    function FlowNode(ex::JuExpr, nodeKind::FlowNodeKind, source::Vector{FlowNode}, typ::CompileType, isInitialized = true)
        return new(ex, nodeKind, source, typ, isInitialized)
    end
end

function makeLiteralFlowNode(ex::JuExpr, typ::CompileType)
    FlowNode(ex, LiteralFlowNode, FlowNode[], typ)
end

function makeReturnFlowNode(ex::JuExpr, e::FlowNode)
    FlowNode(ex, ReturnNode, FlowNode[e], makeBottomType())
end

function makeEmptyReturnFlowNode(ex::JuExpr)
    FlowNode(ex, EmptyReturnNode, FlowNode[], makeBottomType())
end


#=========begin here =======#

function makeGetPropertyFlowNode(ex::JuExpr, node::FlowNode, typ::CompileType)
    FlowNode(ex, GetPropertyNode, FlowNode[node], typ)
end

function makeSetPropertyFlowNode(ex::JuExpr, xnode::FlowNode, vnode::FlowNode, typ::CompileType)
    FlowNode(ex, SetPropertyNode, FlowNode[xnode, vnode], typ)
end

function makeArrayRefNode(ex::JuExpr, args::Vector{FlowNode}, typ::CompileType)
    FlowNode(ex, ArrayRefNode, args, typ)
end

function makeArraySetNode(ex::JuExpr, args::Vector{FlowNode}, typ::CompileType)
    FlowNode(ex, ArraySetNode, args, typ)
end

function makeVarFlowNode(ex::JuExpr, ref::FlowNode)
    FlowNode(ex, VarFlowNode, FlowNode[ref], ref.typ)
end

function makeGlobalVarFlowNode(ex::JuExpr, typ::CompileType)
    FlowNode(ex, GlobalVarFlowNode, FlowNode[], typ)
end

function makeFunCallFlowNode(ex::JuExpr, args::Vector{FlowNode}, typ::CompileType)
    FlowNode(ex, FunCallNode, args, typ)
end

function makeCurlyCallFlowNode(ex::JuExpr, args::Vector{FlowNode}, typ::CompileType)
    FlowNode(ex, CurlyCallNode, args, typ)
end

function makeAssignFlowNode(ex::JuExpr, rhs::FlowNode)
    FlowNode(ex, AssignFlowNode, FlowNode[rhs], rhs.typ, #=isInitialized=#true)
end

function makeEmptyBlockFlowNode(ex::JuExpr)
    FlowNode(ex, EmptyBlockFlowNode, FlowNode[], makeConstVal(nothing))
end

function makeBlockFlowNode(ex::JuExpr, last::FlowNode)
    FlowNode(ex, BlockFlowNode, FlowNode[], last.typ)
end

function makePiNode(ex::JuExpr, condnode::FlowNode, typ::CompileType)
    FlowNode(ex, PiNode, FlowNode[condnode], typ)
end

function makeNegPiNode(ex::JuExpr, condnode::FlowNode, typ::CompileType)
    FlowNode(ex, NegPiNode, FlowNode[condnode], typ)
end

function makeEmptyElseFlowNode(ex::JuExpr)
    FlowNode(ex, EmptyElseFlowNode, FlowNode[], makeConstVal(nothing))
end

function makeDeclarationListNode(ex::JuExpr)
    FlowNode(ex, DeclarationListFlowNode, FlowNode[], makeConstVal(nothing))
end

function makePhiFlowNode(ex::JuExpr, v::Vector{FlowNode}, typ::CompileType)
    FlowNode(ex, PhiFlowNode, v, typ)
end

function makeBreakStmtNode(ex::JuExpr)
    FlowNode(ex, BreakStmtFlowNode, FlowNode[], makeBottomType())
end

function makeContinueStmtNode(ex::JuExpr)
    FlowNode(ex, ContinueStmtFlowNode, FlowNode[], makeBottomType())
end

struct DerivedExpr
    val::Any
end

# We have only one SourceMapping for each method definition
# JuAST and JuExpr is shared between multiple specialized instances of a function
struct SourceMapping
    ast2exMapping::Dict{JuAST, DerivedExpr}
    # for debug only
    ex2astMapping::Dict{JuExpr, JuAST}
end

function SourceMapping()
    return SourceMapping(Dict{JuAST, DerivedExpr}(), Dict{JuExpr, JuAST}())
end

# We have one FlowMapping for each method specialization
struct FlowMapping
    ex2flowMapping::Dict{JuExpr, FlowNode}
    # we have no flow2astMapping here, because that mapping is stored in FlowNode
end

function FlowMapping()
    FlowMapping(Dict{JuExpr, FlowNode}())
end

struct ContextValue
    typ::FlowNode               # primary type of this context variable
    curtyp::FlowNode            # inferred type on this path
end

# Context for variable binding and other useful things, immutable
struct Context
    mapping::ImmutableDict{Symbol, ContextValue}
end

struct ErrorLogger <: IO
    io::IO
end

@nocheck function Base.write(log::ErrorLogger, x::UInt8)
    write(log.io, x)
end

mutable struct GlobalContext
    queue::Vector{Core.MethodInstance}
    hasChecked::Dict{Core.MethodInstance, Any}
    methodDefs::Dict{Core.Method, Pair{JuExpr, SourceMapping}}
    cache::Dict{Any, Vector{Any}}
end

function GlobalContext()
    GlobalContext(Core.MethodInstance[], Dict{Core.MethodInstance, Any}(), Dict{Core.Method, Pair{JuExpr, SourceMapping}}(), Dict{Any, Vector{Any}}())
end

mutable struct Engine
    const globalCtx::GlobalContext
    const mi::Core.MethodInstance
    const mod::Core.Module
    const errio::ErrorLogger
    const sourceMapping::SourceMapping
    const flowMapping::FlowMapping
    # perform necessary end and begin
    const arrayContext::Vector{FlowNode}
    retVal::Maybe{FlowNode}
end


function addFlowMapping!(eng::Engine, ex::JuExpr, node::FlowNode)::Nothing
    if haskey(eng.flowMapping.ex2flowMapping, node)
        error("Internal error : FlowNode already exists")
    end
    eng.flowMapping.ex2flowMapping[ex] = node
    return 
end

function Engine(ctx::GlobalContext, mi::Core.MethodInstance, mod::Core.Module, sourceMapping::SourceMapping)
    return Engine(ctx, mi, mod, ErrorLogger(stdout), sourceMapping, FlowMapping(), FlowNode[], None(FlowNode))
end

function Context()
    return Context(ImmutableDict{Symbol, ContextValue}())
end

function hasvar(ctx::Context, var::Symbol)::Bool
    return exist(ctx.mapping, var)
end

function lookup(ctx::Context, var::Symbol)::ContextValue
    return ctx.mapping.data[var]
end

function update(ctx::Context, var::Symbol, val::ContextValue)::Context
    return Context(Utility.update(ctx.mapping, var, val))
end

# Inference result for one AST
struct InferResult
    ctx::Context
    node::FlowNode
end

struct InferReport
    f::JuExpr
    mi::Core.MethodInstance
    eng::Engine
    rel::InferResult
end

@nocheck function fasteval(mod::Module, x::JuExpr)
    v = x.val
    if v isa Var
        return getproperty(mod, v.id)
    elseif v isa GetProperty
        return getproperty(fasteval(mod, v.x), v.p)
    elseif v isa CurlyCall
        return Core.apply_type(fasteval(mod, v.f), fasteval.(Ref(mod), v.args)...)
    else
        error("Not suppoerted $(typeof(x))")
    end
end