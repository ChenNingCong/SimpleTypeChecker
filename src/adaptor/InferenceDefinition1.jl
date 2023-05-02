using ..SyntaxDefinition
using ..Utility

# A wrapper for any Julia value
struct JuVal
    val::Any
    isPoison::Bool
    isConst::Bool
end

isPoisonVal(v::JuVal) = v.isPoison
isConstVal(v::JuVal) = v.isConst
@inline makeConstVal(v::Any) = JuVal(v, false, true)
@inline makeNonConstVal() = JuVal(nothing, false, false)
@inline makePoisonVal() = JuVal(nothing, true, false)

# A wrapper for any Julia type
struct JuType
    val::Any
    isPoison::Bool
end
isPoisonType(v::JuType) = v.isPoison
@inline makeJuType(v::Any) = JuType(v, false)
@inline makePoisonType() = JuType(nothing, true)

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
end

mutable struct DerivedExpr
    const val::Any
    const ast::JuAST
end

# AST construction
abstract type TypedAST end
struct Var <: TypedAST 
    id::Symbol
end

#=
    Definition (Declaration with initial value)
    x[1] = v
    (x, y) = v
    x = v
=#
struct GenericAssign <: TypedAST
    # Union{DerivedExprAssignLHSTuple, 
    #       DerivedExprAssignLHSArrayRef,
    #       DerivedExprAssignLHSVar}
    lhs::DerivedExpr
    rhs::JuExpr
end


#=
    local x
    local x::Int
    local x::Int = 1
    x::Int = 1
=#
struct Declaration <: TypedAST
    # Union{DerivedExprDeclarationUntyped,
    #       DerivedExprDeclarationTyped}
    declares::Vector{DerivedExpr}
end


struct FunCall <: TypedAST
    f::JuExpr
    args::Vector{JuExpr}
    # optional argument is not positional when used
    # optional argument is positional only when defined
    kwargs::Vector{Pair{Symbol, JuExpr}}
end

struct CurlyCall <: TypedAST
    f::JuExpr
    args::Vector{JuExpr}
end

struct Literal <: TypedAST
    val::JuVal
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
    f::Symbol
    args::Vector{Pair{Symbol, Maybe{JuExpr}}}
    kwargs::Vector{Pair{Symbol, Maybe{JuExpr}}}
    params::Vector{Symbol}
    rt::Maybe{JuExpr}
    body::JuExpr
    # where this method is defined
    # mods::Vector{Symbol}
end

struct ModDef <: TypedAST
    name::Symbol
    stmts::Vector{JuExpr}
end

function getLiteralVal(l::Literal)::JuVal
    return l.val
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

    ControlFlowNodeBegin
    ForUpdateNode            # variable that loses constantness in the loop
    ConditionalFlowNode      # variable that defines conditionally
    PiNode                   # pi projection of a variable
    NegPiNode                # pi projection of a variable on a negative condition branch
end

mutable struct FlowNode
    const ast::JuExpr
    const nodeKind::FlowNodeKind
    const source::Vector{FlowNode}
    const val::JuVal # constant value
    const typ::JuType
end

function makeLiteralFlowNode(ast::Literal, val::JuVal)
    FlowNode(ast.ast, LiteralFlowNode, FlowNode[], val, makeJuType(Core.Typeof(val.val)))
end

function makeBlockFlowNode(ast::Block, last::FlowNode)
    FlowNode(ast.ast, BlockFlowNode, FlowNode[], last.val, last.typ)
end

function makeEmptyBlockFlowNode(ast::Block)
    FlowNode(ast.ast, EmptyBlockFlowNode, FlowNode[], makeConstVal(nothing), makeJuType(Nothing))
end

function makeAssignFlowNode(ast::Assign, rhs::FlowNode)
    FlowNode(ast.ast, AssignFlowNode, FlowNode[rhs], rhs.val, rhs.typ)
end

function makeVarFlowNode(ast::Var, ref::FlowNode)
    FlowNode(ast.ast, VarFlowNode, FlowNode[ref], ref.val, ref.typ)
end

function makeGlobalVarFlowNode(ast::Var, val::JuVal)
    FlowNode(ast.ast, GlobalVarFlowNode, FlowNode[], val, makeJuType(Core.Typeof(val.val)))
end

function makeFunCallFlowNode(ast::FunCall, args::Vector{FlowNode}, val::JuVal, typ::JuType)
    FlowNode(ast.ast, FunCallNode, args, val, typ)
end

function makeCurlyCallFlowNode(ast::CurlyCall, args::Vector{FlowNode}, val::JuVal, typ::JuType)
    FlowNode(ast.ast, CurlyCallNode, args, val, typ)
end

function makeReturnFlowNode(ast::Return, e::FlowNode)
    FlowNode(ast.ast, ReturnNode, FlowNode[e], e.val, e.typ)
end

function makeEmptyReturnFlowNode(ast::Return)
    FlowNode(ast.ast, EmptyReturnNode, FlowNode[], makeConstVal(nothing), makeJuType(Nothing))
end

# We have only one SourceMapping for each method definition
# JuAST and JuExpr is shared between multiple specialized instances of a function
struct SourceMapping
    # Notice the asymmetry here
    ex2astMapping::Dict{Int, JuAST}
    ast2exMapping::Dict{JuAST, DerivedExpr}
end

function SourceMapping()
    return SourceMapping(Dict{Int, JuAST}(), Dict{JuAST, DerivedExpr}())
end

# We have one FlowMapping for each method specialization
struct FlowMapping
    ex2flowMapping::Dict{Int, FlowNode}
    # we have no flow2astMapping here, because that mapping is stored in FlowNode
end

struct ContextValue
    typ::FlowNode               # primary type of this context variable
    curtyp::FlowNode            # inferred type on this path
end

# Context for variable binding and other useful things, immutable
struct Context
    mapping::ImmutableDict{Symbol, ContextValue}
end

mutable struct Engine
    const mod::Core.Module
    # TODO : add a logger here!
    retVal::Maybe{JuExpr}
    const errio::IO
    const debugio::IO
    const sourceMapping::SourceMapping
    const flowMapping::FlowMapping
end

function Engine(mod::Core.Module)
    return Engine(mod, nothing, stdout, stdout)
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
    return Context(update(ctx.mapping, var, val))
end

# Inference result for one AST
struct InferResult
    ctx::Context
    node::FlowNode
end

struct InferReport
    f::FunDef
    tt::Any
    eng::Engine
    rel::InferResult
end