using ..SyntaxDefinition
using ..Utility
import ..Utility
import JuliaSyntax

struct CompileType
    isConst::Bool
    val::Any
    typ::Any
end

function makeBottomType()
    CompileType(true, nothing, Union{})
end

@nocheck function isBottomType(t::CompileType)::Bool
    return t.typ == Union{}
end

function makeConstVal(val::Any)
    CompileType(true, val, Core.Typeof(val))
end

@nocheck function makeType(val::Any)
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

@nocheck function removeConst(typ::CompileType)
    if typ.isConst
        makeType(typ.typ)
    else
        return typ
    end
end

@nocheck function lift(v::CompileType)
    if isConstVal(v)
        return makeType(v.val)
    else
        error("Internal error")
    end
end

@nocheck function convert2ConstVal(val::JuASTVal)::CompileType
    makeConstVal(val.val)
end

#=
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
    TupleLiteralFlowNode
    DestructFlowNode
    
    ControlFlowNodeBegin
    PhiFlowNode
    ForVarNode                 
    ForUpdateNode            # variable that loses constantness in the loop
    ConditionalFlowNode      # variable that defines conditionally
    PiNode                   # pi projection of a variable
    NegPiNode                # pi projection of a variable on a negative condition branch
end
=#

mutable struct FlowNode
    # field (ast shared by all flow node)
    const ast::JuAST
    const source::Vector{FlowNode}
    const typ::CompileType
    const kind::Any # value depends on different flow kind
end

struct LiteralFlowNode end

struct UpdateOp
    isUpdate::Bool
    isDotcall::Bool
    ast::JuAST # ast of the whole assignment, update operator has no mapping ast
    op::Symbol
    function UpdateOp(ast::JuAST, op::Symbol, isDotcall::Bool)
        new(true, isDotcall, ast, op)
    end
    function UpdateOp(ast::JuAST, isDotcall::Bool)
        new(false, isDotcall)
    end
    function UpdateOp()
        new(false, false)
    end
end

struct PiFlowNode end
# represent all kind of assignment flow node
# we can add additional fields here, to easy compiler analysis
struct AssignLHSVarFlowNode 
    isInitialized::Bool
    updateOp::UpdateOp
end
struct ConditionalFlowNode end
struct ForUpdateFlowNode end
struct DeclarationFlowNode end
struct AssignLHSArrayRefFlowNode end

struct LetFlowNode end
struct WhileStmtFlowNode end
#=
There are many kinds of flow nodes that reprensent an assignment action (can appear in ContextValue)
Lowered from                  FlowNode
PiFlowNode                    type narrowing of a variable, must be initialized
PhiNode                       merge of definitions in each branch (for or if-else), must be initialized 
AssignLHSVarFlowNode             assignment with storage type (`local x::Int` or `x = 1` ), potentially uninitialized
ConditionalFlowNode           merge of definitions in each branch (for or if-else), inferred as uninitialized
ForUpdateFlowNode             change of constantness for varables in for-loop, must be initialized
DeclarationFlowNode           assignment without storage type (`local x`), must be uninitialized

Note : x[1] = 1 and x.x = 1 is not treated as variable assignment

AssignLHSTupleDestructFlowNode     definition (assignment with initializer), must be initialized, can't not used in `local`

Lattice
Must Unitialized <: Potentially Unitialized <: Must Initialized
DeclarationFlowNode AssignLHSVarFlowNode，ConditionalFlowNode， other FlowNode

=#
function mustInitialized(node::FlowNode)::Bool
    val = node.kind
    if val isa PiFlowNode
        return true
    elseif val isa AssignLHSVarFlowNode
        return val.isInitialized
    elseif val isa ConditionalFlowNode
        return false
    elseif val isa PhiFlowNode
        return true
    elseif val isa ForUpdateFlowNode
        return true
    elseif val isa DeclarationFlowNode
        return false
    # TODO : add other kinds of FlowNode here!!!!
    elseif val isa SparamFlowNode
        return true
    elseif val isa ParamFlowNode
        return true
    else
        throw(InternalError(""))
    end
end

function mustUnInitialized(node::FlowNode)::Bool
    val = node.kind
    if val isa DeclarationFlowNode
        return true
    elseif val isa AssignLHSVarFlowNode
        return !val.isInitialized
    else
        return false
    end
end

function isDeclarationFlowNode(node::FlowNode)::Bool
    val = node.kind
    if val isa DeclarationFlowNode
        return true
    else
        return false
    end
end

struct AssignExprFlowNode end
struct VarFlowNode end
struct GlobalVarFlowNode end

struct MethodCallStruct
    fargs::Vector{FlowNode}
    kwargs::Vector{Pair{Symbol, FlowNode}}
    function MethodCallStruct(fargs::Vector{FlowNode}, kwargs::Vector{Pair{Symbol, FlowNode}})
        new(fargs, kwargs)
    end
end

function MethodCallStruct(fargs::Vector{FlowNode})::MethodCallStruct
    MethodCallStruct(fargs, Pair{Symbol, FlowNode}[])
end

function MethodCallStruct(f::FlowNode, args::Vector{FlowNode})::MethodCallStruct
    MethodCallStruct(vcat(f, args))
end

struct FunCallFlowNode 
    ms::MethodCallStruct
end
struct BlockFlowNode end
struct TupleFlowNode end
struct LiteralExprFlowNode end
struct MacroCallFlowNode end
struct CurlyCallFlowNode end
struct ReturnFlowNode end
struct BreakStmtFlowNode end
struct ContinueStmtFlowNode end
struct ArrayRefFlowNode end
struct GetPropertyFlowNode end
struct IfStmtFlowNode end
struct PhiFlowNode end
struct AssignLHSTupleDestructFlowNode end
struct SparamFlowNode end
struct ParamFlowNode end
struct ExpectedReturnFlowNode end
struct SetPropertyFlowNode end
struct KeyParamFlowNode end
# special node for Expr, Expr is a mutable that is local to the function body
# we treat the value as a non-constant, because it's mutable
function makeLiteralFlowNode(ast::JuAST, val::JuASTVal)
    FlowNode(ast, FlowNode[], convert2ConstVal(val), LiteralFlowNode())
end

function makeLiteralExprFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeType(Expr), LiteralExprFlowNode())
end
function makeMacroCallFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeType(Nothing), MacroCallFlowNode())
end

function makeAssignLHSVarFlowNode(ast::JuAST, rhs::FlowNode, updateOp::UpdateOp)
    FlowNode(ast, FlowNode[rhs], rhs.typ, AssignLHSVarFlowNode(true, updateOp))
end

function makeAssignLHSLocalVarFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeBottomType(), DeclarationFlowNode())
end

function makeAssignLHSVarAssertFlowNode(ast::JuAST, typ::FlowNode, styp::CompileType, isInitialized::Bool)
    FlowNode(ast, FlowNode[typ], styp, AssignLHSVarFlowNode(isInitialized, UpdateOp()))
end

function makeLetFlowNode(ast::JuAST, typ::FlowNode)
    FlowNode(ast, FlowNode[typ], typ.typ, LetFlowNode())
end

function makeForUpdateFlowNode(ast::JuAST, node::FlowNode)
    if mustInitialized(node) && isConstVal(node.typ)
        return FlowNode(ast, FlowNode[node], removeConst(node.typ), ForUpdateFlowNode())
    else
        return node
    end
end

function makeIterateFlowNode(ast::JuAST, tt::CompileType)
    FlowNode(ast, FlowNode[], tt, nothing)
end

#=
function makeForVarFlowNode(ast::JuAST, rhs::FlowNode, tt::CompileType)
    FlowNode(ast, FlowNode[rhs], tt, AssignLHSVarFlowNode(true, UpdateOp(), rhs))
end
=#
function makeAssignExprFlowNode(ast::JuAST, rhs::FlowNode)
    FlowNode(ast, FlowNode[rhs], rhs.typ, AssignExprFlowNode())
end

function makeVarFlowNode(ast::JuAST, ref::FlowNode)
    FlowNode(ast, FlowNode[ref], ref.typ, VarFlowNode())
end

function makeGlobalVarFlowNode(ast::JuAST, typ::CompileType)
    FlowNode(ast, FlowNode[], typ, GlobalVarFlowNode())
end

function makeFunCallFlowNode(ast::JuAST, ms::MethodCallStruct, typ::CompileType)
    FlowNode(ast, FlowNode[], typ, FunCallFlowNode(ms))
end

function makeEmptyBlockFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeConstVal(nothing), BlockFlowNode())
end

function makeBlockFlowNode(ast::JuAST, last::FlowNode)
    FlowNode(ast, FlowNode[last], last.typ, BlockFlowNode())
end

function makeTupleFlowNode(ast::JuAST, args::Vector{FlowNode}, tt::CompileType)
    FlowNode(ast, args, tt, TupleFlowNode())
end

function makeCurlyCallFlowNode(ast::JuAST, args::Vector{FlowNode}, typ::CompileType)
    FlowNode(ast, args, typ, CurlyCallFlowNode())
end

function makeEmptyReturnFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeBottomType(), ReturnFlowNode())
end

function makeReturnFlowNode(ast::JuAST, e::FlowNode)
    FlowNode(ast, FlowNode[e], makeBottomType(), ReturnFlowNode())
end

function makeBreakStmtNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeBottomType(), BreakStmtFlowNode())
end

function makeContinueStmtNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeBottomType(), ContinueStmtFlowNode())
end

function makeArrayRefFlowNode(ast::JuAST, ms::MethodCallStruct, typ::CompileType)
    FlowNode(ast, ms.fargs, typ, ArrayRefFlowNode())
end

function makeGetPropertyFlowNode(ast::JuAST, node::FlowNode, typ::CompileType)
    FlowNode(ast, FlowNode[node], typ, GetPropertyFlowNode())
end

function makeEmptyElseFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeConstVal(nothing), IfStmtFlowNode())
end

function makeIfStmtFlowNode(ast::JuAST, typ::CompileType)
    FlowNode(ast, FlowNode[], typ, IfStmtFlowNode())
end

function makePiFlowNode(ast::JuAST, condnode::FlowNode, typ::CompileType)
    FlowNode(ast, FlowNode[condnode], typ, PiFlowNode())
end

function makeNegPiFlowNode(ast::JuAST, condnode::FlowNode, typ::CompileType)
    FlowNode(ast, FlowNode[condnode], typ, PiFlowNode())
end

function makeConditionalFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeBottomType(), ConditionalFlowNode())
end

function makePhiFlowNode(ast::JuAST, nodes::Vector{FlowNode}, v::CompileType)
    FlowNode(ast, nodes, v, PhiFlowNode())
end

function makeAssignLHSTupleDestructFlowNode(ast::JuAST, node::FlowNode, v::CompileType)
    FlowNode(ast, FlowNode[node], v, AssignLHSTupleDestructFlowNode())
end

function makeAssignLHSArrayRefFlowNode(ast::JuAST, ms::MethodCallStruct, v::CompileType)
    FlowNode(ast, ms.fargs, v, AssignLHSArrayRefFlowNode())
end

function makeSparamFlowNode(ast::JuAST, v::CompileType)
    FlowNode(ast, FlowNode[], v, SparamFlowNode())
end

function makeParamFlowNode(ast::JuAST, v::CompileType)
    FlowNode(ast, FlowNode[], v, ParamFlowNode())
end

function makeKeyParamFlowNode(ast::JuAST, v::CompileType)
    FlowNode(ast, FlowNode[], v, KeyParamFlowNode())
end

function makeExpectedReturnFlowNode(ast::JuAST, v::CompileType)
    FlowNode(ast, FlowNode[], v, ExpectedReturnFlowNode())
end


function makeSetPropertyFlowNode(ast::JuAST, xnode::FlowNode, rhsnode::FlowNode, typ::CompileType)
    FlowNode(ast, FlowNode[xnode, rhsnode], typ, SetPropertyFlowNode())
end

function makeWhileStmtFlowNode(ast::JuAST)
    FlowNode(ast, FlowNode[], makeType(Nothing), WhileStmtFlowNode())
end

#=
#=========begin here =======#
function makeDeclarationListNode(ex::JuExpr)
    FlowNode(ex, DeclarationListFlowNode, FlowNode[], makeConstVal(nothing))
end

function makePhiFlowNode(ex::JuExpr, v::Vector{FlowNode}, typ::CompileType)
    FlowNode(ex, PhiFlowNode, v, typ)
end

=#

struct ErrorLogger <: IO
    io::IOBuffer
end

@nocheck function Base.write(log::ErrorLogger, x::UInt8)
    write(log.io, x)
end

struct Argument
    name::Symbol
    typ::Maybe{JuAST}
    initializer::Maybe{JuAST}
end

struct FunDef
    ast::JuAST
    # type params and bound
    sparams::Vector{Pair{Symbol, Maybe{JuAST}}}
    rt::Maybe{JuAST}

    # we disallow destruct syntax in all kinds of parameters
    # we disallow short hand function syntax
    # we only support qualified name

    # (::f)(...) functor is not supported yet
    fname::Vector{Symbol}
    args::Vector{Argument}
    optargs::Vector{Argument}
    kwargs::Vector{Argument}
    body::JuAST
end

mutable struct GlobalContext
    errio::ErrorLogger
    # each source file has an individual JuAST
    fileMapping::Dict{JuliaSyntax.SourceFile, JuAST}
    # each method has an individual JuAST
    methodDefs::Dict{Core.Method, FunDef}

    queue::Vector{Core.MethodInstance}
    hasChecked::Dict{Core.MethodInstance, Any}
    cache::Dict{Any, Vector{CompileType}}
end

function GlobalContext()
    GlobalContext(ErrorLogger(IOBuffer()),
                  Dict{JuliaSyntax.SourceFile, JuAST}(),
                  Dict{Core.Method, FunDef}(),
                  Core.MethodInstance[], 
                  Dict{Core.MethodInstance, Any}(), 
                  Dict{Any, Vector{Any}}())
end


#=
Our check is still incorrectly implemented

We need to walk AST for three times
First time : collect explicitly declarated variable
Second time : collect implicitly declarated variable (variable defined with assign)
Third time : validate variable declaration
=#
struct ScopeInfo
    parent::JuAST
    # variable that is declared explicitly with local or for/let/function params, collected in first round
    locals::Dict{Symbol, JuAST} 
    # variable that we have meet in second turn
    curlocals::Set{Symbol} 
    # variable that is declared explicitly with global, collected in first round
    globals::Dict{Symbol, JuAST}
    # variable that we have meet in second turn
    curglobals::Set{Symbol}
    # variable that is local to this scope, but defined through an assignment
    implicitlocals::Dict{Symbol, JuAST} 
    curimplicitlocals::Set{Symbol}

    modified::Dict{Symbol, JuAST} # variable that is not in this scope and modified in this scope
    # relationship 
    # locals + implicitlocals = alllocals
    # curlocals <: alllocals
    # intersect(locals, implicitlocals) = empty
    # intersect(modifies, alllocals) = empty
end

function prettySymbols(s)
    str = join(s, ", ")
    if length(str) == 0
        return "(Empty)"
    end
    return str
end

@nocheck function Base.show(io::IO, info::ScopeInfo)
    println(io, "At $(formatLocation(info.parent.loc))")
    println(io, "Locals : ", prettySymbols(keys(info.locals)))
    println(io, "Globals : ", prettySymbols(keys(info.globals)))
    println(io, "Implicit locals : ", prettySymbols(keys(info.implicitlocals)))
    println(io, "Modified : ", prettySymbols(keys(info.modified)))
    return
end

function ScopeInfo(parent::JuAST)
    ScopeInfo(parent,
              Dict{Symbol, JuAST}(),
              Set{Symbol}(),
              Dict{Symbol, JuAST}(),
              Set{Symbol}(),
              Dict{Symbol, JuAST}(),
              Set{Symbol}(),
              Dict{Symbol, JuAST}())
end

const ScopeInfos = Dict{JuAST, ScopeInfo}

mutable struct ScopeInfoContext
    const ctx::GlobalContext
    const infos::ScopeInfos
    chains::Vector{ScopeInfo}
    walkTurn::Int
end

function addInfo!(ctx::ScopeInfoContext, ast::JuAST, info::ScopeInfo)::Nothing
    if haskey(ctx.infos, ast)
        error("Repetive attach scope")
    end
    ctx.infos[ast] = info
    return
end

function ScopeInfoContext(ctx::GlobalContext)
    ScopeInfoContext(ctx,
                     ScopeInfos(),
                     ScopeInfo[],
                     1)
end


struct ContextValue
    typ::FlowNode               # primary type of this context variable
    curtyp::FlowNode            # inferred type on this path
end

struct Context
    mapping::ImmutableDict{Symbol, ContextValue}
end

function Context()
    return Context(ImmutableDict{Symbol, ContextValue}())
end

function hasVar(ctx::Context, var::Symbol)::Bool
    return Utility.exist(ctx.mapping, var)
end

function lookup(ctx::Context, var::Symbol)::ContextValue
    return ctx.mapping.data[var]
end

function update(ctx::Context, var::Symbol, val::ContextValue)::Context
    return Context(Utility.update(ctx.mapping, var, val))
end

function getAllVars(ctx::Context)
    return keys(ctx.mapping.data)
end

mutable struct Engine
    const globalCtx::GlobalContext
    const mi::Core.MethodInstance
    const mod::Core.Module
    const errio::ErrorLogger
    # a JuAST can have multiple mapping
    const flowMapping::Dict{JuAST, Vector{FlowNode}}
    const scopeInfos::ScopeInfos
    # perform necessary end and begin
    const arrayContext::Vector{FlowNode}
    const loopContext::Vector{Vector{Context}}
    retVal::Maybe{FlowNode}
end

function enterLoop(eng::Engine)
    push!(eng.loopContext, FlowNode[])
end

function leaveLoop(eng::Engine)
    pop!(eng.loopContext)
end

function getGlobalCtx(eng::Engine)
    return eng.globalCtx
end

function getMod(mi::Core.MethodInstance)::Core.Module
    v = mi.def
    if v isa Module
        return v
    else
        return v.module
    end
end

function Engine(ctx::GlobalContext, mi::Core.MethodInstance, scopeInfos::ScopeInfos)
    # stdout is abstract
    return Engine(ctx, mi, getMod(mi), ctx.errio, Dict{JuAST, Vector{FlowNode}}(), scopeInfos, FlowNode[], Vector{Vector{Context}}(), None(FlowNode))
end

function addFlowMapping!(eng::Engine, ast::JuAST, node::FlowNode)::Nothing
    map = eng.flowMapping
    if !haskey(map, ast)
        map[ast] = FlowNode[]
    end
    push!(map[ast], node)
    return
end

struct InferResult
    ctx::Context
    node::FlowNode
end

struct InferReport
    ast::JuAST
    mi::Core.MethodInstance
    eng::Engine
    rel::InferResult
end

function displayResult(io::IO, rel::InferResult)
    displayContext(io, rel.ctx)
end

@nocheck function displayContext(io::IO, ctx::Context)
    all = sort(collect(ctx.mapping.data), by = x->x[1])
    cons_v = [i for i in all if isConstVal(i[2].curtyp.typ)]
    # TODO : how should we deal with variout assignment node?
    other_v = [i for i in all if !isConstVal(i[2].curtyp.typ)]
    #other_v_cond = [i for i in all if !isConstVal(i[2].curtyp.typ) && i[2].curtyp.nodeKind == ConditionalFlowNode]
    if !isempty(cons_v)
        println(io, "Constant :")
        for (k,v) in cons_v
            println(io, "  $k : $(repr(v.curtyp.typ.val))")
        end
    end
    if !isempty(other_v)
        println(io, "Variable : (Slot type and Current type)")
        for (k,v) in other_v
            a = v.typ.typ.typ
            b = v.curtyp.typ.typ
            if (a == b)
                println(io, "  $k::$a")
            else
                println(io, "  $k::$a | $b")
            end
        end
    end
    #=
    if !isempty(other_v_cond)
        println(io, "Conditionally Defined Variable : (Slot type and Current type)")
        for (k,v) in other_v_cond
            a = v.typ.typ.typ
            b = v.curtyp.typ.typ
            if (a == b)
                println(io, "  $k::$a")
            else
                println(io, "  $k::$a | $b")
            end
        end
    end
    =#
    if isempty(all)
        println(io, "(Empty Context)")
    end
    displayReturn
end

function displayReturn(io::ErrorLogger, eng::Engine)
    val = eng.retVal
    if !isNone(val)
        println(io, "Return Type : $(toString(castJust(val).typ))")
    end
end

@nocheck function displayReport(io::ErrorLogger, r::InferReport)
    println(io, '\u2500'^64)
    ex = r.f
    val = ex.val
    if val isa FunDef
        println(io, "Inference Result for Method $(r.mi)")
        displayReturn(io, r.eng)
        displayResult(io, r.rel)
        println(io, '\u2500'^64)
    else
        error("Internal Error")
    end
    return
end