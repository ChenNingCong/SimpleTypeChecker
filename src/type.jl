using JuliaSyntax
# A wrapper for JuliaSyntax's Expr
struct Location
    file::JuliaSyntax.SourceFile
    span::Tuple{UInt, UInt}
end

mutable struct JuAST
    const head::Symbol
    const val::Any
    const span::Location
    const children::Vector{JuAST}
end

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

# A wrapper for any Julia value
struct JuExpr
    val::Any
end

# AST construction
const TypedAST = JuExpr

struct Var
    ast::JuAST
    id::Symbol
end

struct Assign
    ast::JuAST
    lhs::Var
    rhs::TypedAST
end

struct TypedAssign
    ast::JuAST
    lhs::Var
    typ::TypedAST
    rhs::TypedAST
end

struct FunCall
    ast::JuAST
    f::TypedAST
    args::Vector{TypedAST}
    kwargs::Vector{TypedAST}
end

struct CurlyCall
    ast::JuAST
    f::TypedAST
    args::Vector{TypedAST}
end

struct Literal
    ast::JuAST
    val::JuVal
end

struct Block
    ast::JuAST
    stmts::Vector{TypedAST}
end

struct IfStmt
    ast::JuAST
    branches::Vector{Tuple{TypedAST, TypedAST}}
    else_::Union{TypedAST, Nothing}
end

struct WhileStmt
    ast::JuAST
    cond::TypedAST
    body::TypedAST
end

struct ForStmt
    ast::JuAST
    var::Var
    iter::TypedAST
    body::TypedAST
end

struct Return
    ast::JuAST
    e::Union{TypedAST, Nothing}
end

struct GetProperty
    ast::JuAST
    x::TypedAST
    p::Symbol
end

struct SetProperty
    ast::JuAST
    x::TypedAST
    p::Symbol
    v::TypedAST
end

struct ArrayRef
    ast::JuAST
    arr::TypedAST
    i::Vector{TypedAST}
end

struct ArraySet
    ast::JuAST
    arr::TypedAST
    i::Vector{TypedAST}
    v::TypedAST
end

struct TypedAssert
    ast::JuAST
    lhs::TypedAST
    rhs::TypedAST
end

struct FunDef
    ast::JuAST
    f::Symbol
    args::Vector{Pair{Symbol, Union{TypedAST, Nothing}}}
    kwargs::Vector{Pair{Symbol, Union{TypedAST, Nothing}}}
    params::Vector{Symbol}
    rt::Union{Nothing, TypedAST}
    body::TypedAST
end

struct ModDef
    ast::JuAST
    name::Symbol
    stmts::JuExpr
end

function getLiteralVal(l::Literal)::JuVal
    return l.val
end

# End of AST construction 

# Flow node, used to map type and ast
@enum FlowNodeKind begin
    LiteralFlowNode
    AssignFlowNode
    EmptyBlockFlowNode
    EmptyElseFlowNode
    BlockFlowNode
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

    ForEndNode
    ForVarNode
    ForUpdateNode # variable that loses constantness in the loop

    IfFlowNode # PhiNode actually
    ConditionalFlowNode # variable that defines conditionally
    PiNode
    NegPiNode
end

mutable struct FlowNode
    const ast::JuAST
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
    retVal::Union{Nothing, FlowNode}
    const errio::IO
    const debugio::IO
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