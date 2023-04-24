module Interface
include("datastruct.jl")
include("adapter.jl")
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
    i::TypedAST
end

struct ArraySet
    ast::JuAST
    arr::TypedAST
    i::TypedAST
    v::TypedAST
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

function getLiteralVal(l::Literal)::JuVal
    return l.val
end

# end of ast construction 

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
    
    ParamNode
    SparamNode

    
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

function tryMergeFlowType(v1::FlowNode, v2::FlowNode)
    v1typ = v1.typ
    v2typ = v2.typ
    @assert !isPoisonType(v1typ) && !isPoisonType(v2typ)
    if v2typ.val <: v1typ.val
        return true
    else
        return false
    end
end

function tryMergeValue(v1::JuVal, v2::JuVal)::Bool
    if isConstVal(v1) && isConstVal(v2)
        if v1.val == v2.val
            return true
        end
    elseif !isConstVal(v1) && !isConstVal(v2)
        return true
    end
    return false
end

function tryMergeType(v1::JuType, v2::JuType, allowUnion::Bool)
    @assert !isPoisonType(v1) && !isPoisonType(v2)
    newv = Union{v1.val, v2.val}
    if !allowUnion
        if newv != v1.val
            error("if enlarge types")
        end
    end
    return makeJuType(newv)
end

function tryMergeFlowNode(ast::JuAST, v::Vector{FlowNode}, allowUnion::Bool)::FlowNode
    tmpval = v[1].val
    tmptyp = v[1].typ
    allConst = true
    for i in 2:length(v)
        if !tryMergeValue(tmpval, v[i].val)
            allConst = false
        end
        tmptyp = tryMergeType(tmptyp, v[i].typ, allowUnion)
    end
    if allConst
        return FlowNode(ast, IfFlowNode, v, tmpval, tmptyp)
    else
        return FlowNode(ast, IfFlowNode, v, makeNonConstVal(), tmptyp)
    end
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

function makeEmtpyReturnFlowNode(ast::Return)
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
    retVal::Union{Nothing, FlowNode}
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

# Inference result for one ast
struct InferResult
    ctx::Context
    node::FlowNode
end

function flattenIf!(rel, ast::JuAST)::Union{JuExpr, Nothing}
    if length(ast.children) == 3 
        push!(rel, (typedConvertAST(ast.children[1]), typedConvertAST(ast.children[2])))
        if ast.children[3].head == :elseif
            flattenIf!(rel, ast.children[3])
        else
            return typedConvertAST(ast.children[3])
        end
    else
        return nothing
    end
end

function flattenIf(ast::JuAST)::JuExpr
    rel = Vector{Tuple{JuExpr, JuExpr}}()
    else_b = flattenIf!(rel, ast)
    return JuExpr(IfStmt(ast, rel, else_b))
end

function collectArgs(e::JuAST)::Pair{Symbol, Union{JuExpr,Nothing}}
    if e.head == :(::)
        if length(e.children) != 2
            error("Invalid assert syntax")
        end
        var = e.children[1]
        ex = e.children[2]
        if var.head != :literal
            error("assert is applied to non-symbol")
        end
        return (var.val::Symbol) => typedConvertAST(ex)
    elseif e.head == :literal
        return (e.val::Symbol) => nothing
    else
        error("Not assert")
    end
end

function extractSymbol(ast::JuAST)::Symbol
    if ast.head != :literal
        error("Not a symbol")
    end
    return ast.val::Symbol
end

function typedConvertAST(ast::JuAST)::JuExpr
    # TODO : check whether this is correct!
    if ast.head == :literal
        if ast.val isa Symbol
            return JuExpr(Var(ast, ast.val))
        else
            return JuExpr(Literal(ast, makeConstVal(ast.val)))
        end
    elseif ast.head == :quote
        if length(ast.children) >= 1 && ast.children[1].val isa Symbol
            return JuExpr(Literal(ast, makeConstVal(ast.children[1].val)))
        else
            error("Disallowed quoted expression")
        end
    elseif ast.head == :curly
        f = typedConvertAST(ast.children[1])
        args = [typedConvertAST(i) for i in ast.children[2:end]]
        return JuExpr(CurlyCall(ast, f, args))
    elseif ast.head == :call
        f = typedConvertAST(ast.children[1])
        args = [typedConvertAST(i) for i in ast.children[2:end] if i.head != :kw]
        kwargs = [typedConvertAST(i) for i in ast.children[2:end] if i.head == :kw]
        return JuExpr(FunCall(ast, f, args, kwargs))
    elseif ast.head == :(=)
        rhs = typedConvertAST(ast.children[2])
        lhs_ = ast.children[1]
        if lhs_.head == :(.) || lhs_.head == :ref
            p_ = lhs_.children[1]
            x_ = lhs_.children[2]
            p = typedConvertAST(p_)
            if lhs_.head == :(.)
                if x_.head == :quote
                    c = x_.children[1]
                    return JuExpr(SetProperty(ast, p, c.val::Symbol, rhs))
                else
                    error("Not a quote for get property")
                end
            else
                return JuExpr(ArraySet(ast, p, typedConvertAST(x_), rhs))
            end
        else
            lhs = typedConvertAST(lhs_)
            if lhs.val isa Var
                return JuExpr(Assign(ast, lhs.val, rhs))
            else
                error("bad assign")
            end
        end
    elseif ast.head == :ref
        lhs = typedConvertAST(ast.children[1])
        rhs = typedConvertAST(ast.children[2])
        return JuExpr(ArrayRef(ast, lhs, rhs))
    elseif ast.head == :(.)
        lhs = typedConvertAST(ast.children[1])
        rhs = ast.children[2]
        if rhs.head == :quote
            x_ = rhs.children[1] 
            return JuExpr(GetProperty(ast, lhs, x_.val::Symbol))
        else
            error("Not a quote for get property")
        end
    elseif ast.head == :if
        return flattenIf(ast)
    elseif ast.head == :block
        return JuExpr(Block(ast, [typedConvertAST(i) for i in ast.children]))
    elseif ast.head == :return 
        if length(ast.children) == 1
            return JuExpr(Return(ast, typedConvertAST(ast.children[1])))
        else
            return JuExpr(Return(ast, nothing))
        end
    elseif ast.head == :toplevel
        stmts = [typedConvertAST(i) for i in ast.children]
        return JuExpr(Block(ast, stmts))
    elseif ast.head == :while
        cond = ast.children[1]
        body = ast.children[2]
        return JuExpr(WhileStmt(ast, typedConvertAST(cond), typedConvertAST(body)))
    elseif ast.head == :for
        cond = ast.children[1]
        body = ast.children[2]
        if cond.head != :(=)
            error("Invalid for expression")
        end
        var = typedConvertAST(cond.children[1])
        varval = var.val
        if varval isa Var
            iter = cond.children[2]
            return JuExpr(ForStmt(ast, varval, typedConvertAST(iter), typedConvertAST(body)))
        else
            error("iterate variable is not a symbol")
        end
    elseif ast.head == :string
        return JuExpr(Literal(ast, makeConstVal(ast.children[1].val)))
    elseif ast.head == :function
        if length(ast.children) < 2
            error("Unsupported empty function body")
        end
        body = typedConvertAST(ast.children[2])
        fast = ast.children[1]
        if fast.head == :where
            println(fast.children[2:end])
            # TODO : support subtyping contraint here...
            params = [extractSymbol(i) for i in fast.children[2:end]]
            fast = fast.children[1]
        else
            params = Symbol[]
        end
        local rt::Union{Nothing, JuExpr}
        if fast.head == :(::)
            rt = typedConvertAST(fast.children[2])
            fast = fast.children[1]
        else
            rt = nothing
        end
        if fast.head == :call
            f = typedConvertAST(fast.children[1])
            args = [collectArgs(i) for i in fast.children[2:end] if i.head != :kw]
            for i in fast.children[2:end] 
                if i.head == :kw
                    error("kwargs is unsupported")
                end
            end
            fval = f.val
            if fval isa Var
                return JuExpr(FunDef(ast, fval.id, args, Pair{Symbol, Union{TypedAST, Nothing}}[], params, rt, body))
            else
                error("Only support named function definition")
            end
        else
            error("Only support named function definition")
        end
    else
        println(ast.head)
        error()
    end
end



#=
We consider here a simple programming language with following constructions:
<exprs> := list of <expr>
<expr> :=
    | <var> = <expr>                              // assignment to a single variable
    | <expr>(<expr>...)                           // function call
    | <expr>[expr...]                             // indexing call
    | <expr>.<expr>                               // property call
    | <expr>{expr...}                             // type application (ignored currently)
    | if <expr> elseif <exprs> else <expr> end    // if clause
    | begin <exprs> end                           // sequential operation
    | for <var> in <expr> <exprs>  end            // for loop
    | while <expr> <exprs> end                    // while loop
    | continue                                    // control flow
    | break                                       // control flow
    | local <var>                                 // untyped variable declaration, actually it can only occur at a stmt position
    | local <var>::<expr>                         // typed variable declaration 
    | <literal>                                   // literal
=#

function inferExpr(eng::Engine, ctx::Context, ast::JuExpr)::InferResult
    val::Any = ast.val
    if val isa Literal
        return inferLiteral(eng, ctx, val)
    elseif val isa Assign
        return inferAssign(eng, ctx, val)
    elseif val isa FunCall
        return inferFunCall(eng, ctx, val)
    elseif val isa Block
        return inferBlock(eng, ctx, val)
    elseif val isa Var
        return inferVar(eng, ctx, val)
    elseif val isa CurlyCall
        return inferCurlyCall(eng, ctx, val)
    elseif val isa IfStmt
        return inferIfStmt(eng, ctx, val)
    elseif val isa Return
        return inferReturn(eng, ctx, val)
    elseif val isa ArrayRef
        return inferArrayRef(eng, ctx, val)
    elseif val isa GetProperty
        return inferGetField(eng, ctx, val)
    elseif val isa ForStmt
        return inferForStmt(eng, ctx, val)
    elseif val isa WhileStmt
        return inferWhileStmt(eng, ctx, val)
    else
        error("Unimplemented $ast")
    end
end

#=
struct GetProperty
    ast::JuAST
    x::TypedAST
    p::Symbol
end
=#

function inferGetField(eng::Engine, ctx::Context, ast::GetProperty)::InferResult
    rel = inferExpr(eng, ctx, ast.x)
    tt = fieldtype(rel.node.typ.val, ast.p)
    newnode = FlowNode(ast.ast, GetPropertyNode, [rel.node], makeNonConstVal(), makeJuType(tt))
    InferResult(rel.ctx, newnode)
end

function inferArrayRef(eng::Engine, ctx::Context, ast::ArrayRef)::InferResult
    rel1 = inferExpr(eng, ctx, ast.arr)
    rel2 = inferExpr(eng, rel1.ctx, ast.i)
    mm = Base.code_typed_by_type(Tuple{typeof(Base.getindex), rel1.node.typ.val, rel2.node.typ.val})
    if length(mm) >= 2
        error("More than one matching method")
    elseif length(mm) == 0
        error("No matching method for array indexing operation")
    end
    tt = mm[1][2]
    newnode = FlowNode(ast.ast, ArrayRefNode, [rel1.node, rel2.node], makeNonConstVal(), makeJuType(tt))
    InferResult(rel2.ctx, newnode)
end

function inferArraySet(eng::Engine, ctx::Context, ast::ArraySet)::InferResult

end

function inferSetField(eng::Engine, ctx::Context, ast::SetProperty)::InferResult

end

function inferVar(eng::Engine, ctx::Context, ast::Var)::InferResult
    if hasvar(ctx, ast.id)
        ctxval = lookup(ctx, ast.id)
        if ctxval.curtyp.nodeKind == ConditionalFlowNode
            error("Variable is conditionally defined")
        end
        node = makeVarFlowNode(ast, ctxval.curtyp)
        return InferResult(ctx, node)
    elseif isdefined(@__MODULE__, ast.id)
        val_::Any = getproperty(@__MODULE__, ast.id)
        val = makeConstVal(val_)
        # fix this, using a module here
        node = makeGlobalVarFlowNode(ast, val)
        return InferResult(ctx, node)
    else
        loc = ast.ast.span
        println(formatLocation(loc))
        code = loc.file.code[loc.span[1]:loc.span[2]]
        println("  Variable $(ast.id) is undefined.")
        error()
    end
end

#=
function inferIf(eng::Engine, ctx::Context, ast::IfStmt)::InferResult

end
=#
#=
function inferFor(eng::Engine, ctx::Context, ast::ForStmt)::InferResult
    # TODO : when we infer for loop
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant
end
=#
function inferFunCall(eng::Engine, ctx::Context, ast::FunCall)::InferResult
    if length(ast.kwargs) != 0
        error()
    else
        # infer from left to right
        ret = inferExpr(eng, ctx, ast.f)
        ctx = ret.ctx
        nodes = FlowNode[ret.node]
        for i in 1:length(ast.args)
            ret = inferExpr(eng, ctx, ast.args[i])
            ctx = ret.ctx
            push!(nodes, ret.node)
        end
        argtyps = Any[]
        for i in 1:length(nodes)
            n = nodes[i]
            ttypval = n.typ.val
            if isconcretetype(ttypval) || ttypval isa Type
                push!(argtyps, ttypval)
            else
                println(ast)
                error("Argument type $i-th $ttypval is not a concrete type or a singleton type")
            end
        end
        # TODO : perform constant propergation here
        matches = Base.code_typed_by_type(Tuple{argtyps...})
        if length(matches) > 1
            printError(ast.ast)
            println(argtyps)
            error("Function call not unique")
        elseif length(matches) == 0
            printError(ast.ast)
            println(argtyps)
            error("No matched function call")
        end
        # TODO : we should add constant propagation for pure builtin function !!!
        # this is again a terminal node
        node = makeFunCallFlowNode(ast, FlowNode[], makeNonConstVal(), makeJuType(matches[1][2]))
        return InferResult(ctx, node)
    end
end

function inferCurlyCall(eng::Engine, ctx::Context, ast::CurlyCall)::InferResult
    # infer from left to right
    ret = inferExpr(eng, ctx, ast.f)
    ctx = ret.ctx
    nodes = FlowNode[ret.node]
    for i in 1:length(ast.args)
        ret = inferExpr(eng, ctx, ast.args[i])
        ctx = ret.ctx
        push!(nodes, ret.node)
    end
    argtyps = Any[]
    for i in 1:length(nodes)
        n = nodes[i]
        tcurval::JuVal = n.val
        if !isConstVal(tcurval)
            error("Apply type use a non-constant, which is disallowed")
        else
            push!(argtyps, tcurval.val)
        end
    end
    tt = Core.apply_type(argtyps...)
    node = makeCurlyCallFlowNode(ast, FlowNode[], makeConstVal(tt), makeJuType(Core.Typeof(tt)))
    return InferResult(ctx, node)
end

function inferAssign(eng::Engine, ctx::Context, ast::Assign)::InferResult
    rel = inferExpr(eng, ctx, ast.rhs)
    retctx = rel.ctx
    retnode = rel.node
    var = ast.lhs
    if hasvar(retctx, var.id)
        # this variable is already assigned before, we check type compatibility
        oldval = lookup(retctx, var.id)
        # storage type is unchanged, update current type 
        if !tryMergeFlowType(oldval.typ, retnode)
            error("Incompatble type is assigned")
        end
        newnode = makeAssignFlowNode(ast, retnode)
        # the primary assignment is unchanged
        val = ContextValue(oldval.typ, newnode)
    else
        newnode = makeAssignFlowNode(ast, retnode)
        val = ContextValue(newnode, newnode)
    end
    newctx = update(retctx, var.id, val)
    return InferResult(newctx, newnode)
end

function inferBlock(eng::Engine, ctx::Context, ast::Block)::InferResult
    if length(ast.stmts) == 0
        newnode = makeEmptyBlockFlowNode(ast)
        return InferResult(ctx, newnode)
    else
        for i in 1:length(ast.stmts)-1
            ret = inferExpr(eng, ctx, ast.stmts[i])
            ctx = ret.ctx
        end
    end
    ret = inferExpr(eng, ctx, ast.stmts[end])
    newnode = makeBlockFlowNode(ast, ret.node)
    return InferResult(ret.ctx, newnode)
end

function inferLiteral(eng::Engine, ctx::Context, ast::Literal)::InferResult
    node = makeLiteralFlowNode(ast, ast.val)
    return InferResult(ctx, node)
end

# TODO : we should terminate after we return (we need to cut the value here)
function inferReturn(eng::Engine, ctx::Context, ast::Return)::InferResult
    aste = ast.e
    engn = eng.retVal
    if aste isa Nothing
        newnode = makeEmtpyReturnFlowNode(ast)
        if engn isa Nothing
            eng.retVal = newnode
        else
            tryMergeFlowNode(ast.ast, [engn, newnode], true)
        end
        return ctx
    else
        rel = inferExpr(eng, ctx, ast.e)
        newnode = makeReturnFlowNode(ast, rel.node)
        if engn isa Nothing
            eng.retVal = newnode
        else
            tryMergeFlowNode(ast.ast, [engn, newnode], true)
        end
        return rel
    end
end

function collectUnion(x)::Vector{Any}
    if x isa Union
        return vcat(collectUnion(x.a), collectUnion(x.b))
    else
        return Any[x]
    end
end

function splitUnion(x, y)::Any
    u1 = collectUnion(x) 
    u2 = collectUnion(y)
    Union{setdiff(u1, u2)...}
end

function tryNarrowType(eng::Engine, ctx::Context, ast::JuExpr)::Tuple{InferResult, Context}
    e = ast.val
    if e isa FunCall
        args = e.args
        if length(e.args) == 2 && length(e.kwargs) == 0
            f = e.f.val
            x = e.args[1].val
            ex = e.args[2]
            # TODO : we should assign flow node to isa and var c2 here
            if f isa Var
                if f.id == :isa  
                    if x isa Var
                        rel = inferExpr(eng, ctx, ex)
                        newtt = rel.node.val
                        if !isConstVal(newtt)
                            error("isa can't be used in a non-constant environment")
                        end
                        newttval = newtt.val
                        if !(newttval isa Type)
                            error("rhs of isa is not a type")
                        end
                        ctxval = lookup(ctx, x.id)
                        curt = ctxval.curtyp.typ.val
                        pinode = FlowNode(ex.val.ast, PiNode, [ctxval.curtyp, rel.node], makeNonConstVal(), makeJuType(newttval))
                        newctxval = ContextValue(ctxval.typ, pinode)
                        newctx = update(rel.ctx, x.id, newctxval)
                        # TODO : maybe we should use a cond type here
                        condnode = FlowNode(e.ast, FunCallNode, [], makeNonConstVal(), makeJuType(Bool))
                        newrel = InferResult(newctx, condnode)
                        if curt == Any
                            return newrel, rel.ctx
                        elseif curt isa Union
                            negtt = splitUnion(curt, newttval)
                            negpinode = FlowNode(e.ast, NegPiNode, [ctxval.curtyp, rel.node], makeNonConstVal(), makeJuType(negtt))
                            newctxval = ContextValue(ctxval.typ, negpinode)
                            negctx = update(rel.ctx, x.id, newctxval)
                            return newrel, negctx
                        else
                            error("Not here")
                        end
                    else
                        error("`isa` is not in a predicative form")
                    end
                end
            end
        end
    end
    rel = inferExpr(eng, ctx, ast)
    return rel, rel.ctx
end
    

function inferIfStmt(eng::Engine, ctx::Context, ast::IfStmt)::InferResult
    # no pi projection
    rels = Vector{InferResult}()
    prectx = ctx
    for (cond, body) in ast.branches
        rel1, ctx = tryNarrowType(eng, ctx, cond)
        rel2 = inferExpr(eng, rel1.ctx, body)
        # ctx here is the ctx foe negative branch
        push!(rels, rel2)
    end
    el = ast.else_
    if el isa Nothing
        node = FlowNode(ast.ast, EmptyElseFlowNode, FlowNode[], makeConstVal(nothing), makeJuType(Nothing))
        push!(rels, InferResult(prectx, node))
    else
        push!(rels, inferExpr(eng, ctx, el))
    end
    # then we need to join the result
    # TODO
    # we need to consider reacheable and unreachable here
    # Case 1 : variable defined before if
    defined = Dict{Symbol, ContextValue}()
    for (name, ctxval) in prectx.mapping.data
        value = tryMergeFlowNode(ast.ast, [lookup(rel.ctx, name).curtyp for rel in rels], true)
        defined[name] = ContextValue(ctxval.typ, value)
    end
    # Case 2 : variable defined in if, maybe conditionally
    newnames = Set{Symbol}()
    for rel in rels
        for i in keys(rel.ctx.mapping.data)
            if hasvar(prectx, i)
                continue
            end
            push!(newnames, i)
        end
    end
    non_defined = Dict{Symbol, ContextValue}()
    for name in newnames
        condDef::Bool = false
        for rel in rels
            if !hasvar(rel.ctx, name)
                condDef = true
                break
            end
        end
        node = tryMergeFlowNode(ast.ast, [lookup(rel.ctx, name).curtyp for rel in rels if hasvar(rel.ctx, name)], false)
        if condDef
            node = FlowNode(ast.ast, ConditionalFlowNode, FlowNode[node], node.val, node.typ)
        end
        value = ContextValue(node, node)
        non_defined[name] = value
    end
    newctx = Context(ImmutableDict(Dict{Symbol, ContextValue}(merge(defined, non_defined))))
    retnode = tryMergeFlowNode(ast.ast, [i.node for i in rels], true)
    return InferResult(newctx, retnode)
end

function inferFor(eng::Engine, ctx::Context, ast::ForStmt)::InferResult
    # TODO : when we infer for loop
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant
end


function testInfer(s::String)
    eng = Engine(nothing)
    ast = typedConvertAST(parseJuAST(s))
    rel = inferExpr(eng, Context(), ast)
    displayReturn(stdout, eng)
    displayResult(stdout, rel)
end

function displayResult(io::IO, rel::InferResult)
    displayContext(io,rel.ctx)
end

function displayContext(io::IO, ctx::Context)
    all = sort(collect(ctx.mapping.data), by = x->x[1])
    cons_v = [i for i in all if isConstVal(i[2].curtyp.val)]
    other_v = [i for i in all if !isConstVal(i[2].curtyp.val) && i[2].curtyp.nodeKind != ConditionalFlowNode]
    other_v_cond = [i for i in all if !isConstVal(i[2].curtyp.val) && i[2].curtyp.nodeKind == ConditionalFlowNode]
    if !isempty(cons_v)
        println(io, "Constant :")
        for (k,v) in cons_v
            println(io, "  $k : $(v.curtyp.val.val)")
        end
    end
    if !isempty(other_v)
        println(io, "Variable : (Slot type and Current type)")
        for (k,v) in other_v
            a = v.typ.typ.val
            b = v.curtyp.typ.val
            if (a == b)
                println(io, "  $k::$a")
            else
                println(io, "  $k::$a | $b")
            end
        end
    end
    if !isempty(other_v_cond)
        println(io, "Conditionally Defined Variable : (Slot type and Current type)")
        for (k,v) in other_v_cond
            a = v.typ.typ.val
            b = v.curtyp.typ.val
            if (a == b)
                println(io, "  $k::$a")
            else
                println(io, "  $k::$a | $b")
            end
        end
    end
    if isempty(all)
        println(io, "(Empty Context)")
    end
end


function displayReturn(io::IO, eng::Engine)
    if eng.retVal isa Nothing
    else
        println(io, "Return Type : $(eng.retVal.typ.val)")
    end
end


function convert2line(v::Vector{Int64}, i)
    @assert i >= v[1] "Invalid line"
    if i >= v[end]
        return lastindex(v)
    else
        l = 1
        r = lastindex(v)
        while ((r-l) > 1)
            mid = div(l + r, 2)
            if v[mid] > i
                r = mid
            elseif v[mid] == i
                return (mid, i - v[mid] + 1)
            else
                l = mid
            end
        end
        return (l, i - v[l] + 1)
    end
end

function testInferForFunction(ast::FunDef, f, tt)
    mis = Core.Compiler.method_instances(f, tt)
    if length(mis) != 1
        error("Not unique match")
    end
    mi = mis[1]
    smapping = Dict{Symbol, ContextValue}()
    if length(mi.sparam_vals) != length(ast.params)
        error("mismatched param size")
    end
    for i in 1:length(mi.sparam_vals)
        tt = mi.sparam_vals[i]
        # TODO : change the definition here...
        node = FlowNode(ast.ast, SparamNode, FlowNode[], makeNonConstVal(), makeJuType(tt))
        smapping[ast.params[i]] = ContextValue(node, node)
    end
    mapping = Dict{Symbol, ContextValue}()
    argtts = mi.specTypes.parameters
    if length(argtts) != length(ast.args) + 1
        error("mismatched argument size")
    end
    for i in 1:length(ast.args)
        tt = argtts[i+1]
        # TODO : change the definition here...
        node = FlowNode(ast.ast, ParamNode, FlowNode[], makeNonConstVal(), makeJuType(tt))
        mapping[ast.args[i][1]] = ContextValue(node, node)
    end
    ctx = Context(ImmutableDict(merge(smapping, mapping)))
    eng = Engine(nothing)
    rel = inferExpr(eng, ctx, ast.body)
    displayReturn(stdout, eng)
    displayResult(stdout, rel)
end

function extractFunDef(ast::JuExpr)::Vector{FunDef}
    e = ast.val
    if e isa Block
        res = Vector{FunDef}()
        for i in e.stmts
            append!(res, extractFunDef(i))
        end
        return res
    elseif e isa FunDef
        res = Vector{FunDef}()
        push!(res, e)
        return res
    else
        error("Not valid func def")
    end
end

end
#=
    Given a JuAST, convert it to the typed representation of AST
=#