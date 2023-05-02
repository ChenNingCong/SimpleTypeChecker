module Interface
include("datastruct.jl")
include("type.jl")
include("utility.jl")
include("adapter.jl")
include("error.jl")

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

@nocheck function tryMergeValue(v1::JuVal, v2::JuVal)::Bool
    if isConstVal(v1) && isConstVal(v2)
        if v1.val == v2.val
            return true
        end
    elseif !isConstVal(v1) && !isConstVal(v2)
        return true
    end
    return false
end

@nocheck function tryMergeType(v1::JuType, v2::JuType, allowUnion::Bool)
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
    val= ast.val
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
    elseif val isa ArraySet
        return inferArraySet(eng, ctx, val)
    elseif val isa SetProperty
        return inferSetField(eng, ctx, val)
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

# TODO : make the logic correct here
# TODO : fix me here !!!
@nocheck function evalGetField(ast::JuAST, node::FlowNode, v::JuVal, p::Symbol)::FlowNode
    vval = v.val
    if vval isa Module
        if isdefined(vval, p)
            # TODO handle non-const global here
            v = getproperty(vval, p)
            return FlowNode(ast, GetPropertyNode, FlowNode[node], makeConstVal(v), makeJuType(Core.Typeof(v)))
        else
            error("Undefined global $p in module $vval")
        end
    else
        if hasfield(Core.Typeof(vval), p)
            tt = fieldtype(Core.Typeof(vval), p)
            newnode = FlowNode(ast, GetPropertyNode, FlowNode[node], makeNonConstVal(), makeJuType(tt))
            return newnode
        else
            error("type $(Core.Typeof(vval)) has no property $p")
        end
    end
end

@nocheck function inferGetField(eng::Engine, ctx::Context, ast::GetProperty)::InferResult
    rel = inferExpr(eng, ctx, ast.x)
    # we need to distinguish getproperty on module and other types...
    if isConstVal(rel.node.val)
        newnode = evalGetField(ast.ast, rel.node, rel.node.val, ast.p)
        return InferResult(rel.ctx, newnode)
    else
        if hasfield(rel.node.typ.val, ast.p)
            tt = fieldtype(rel.node.typ.val, ast.p)
            newnode = FlowNode(ast.ast, GetPropertyNode, FlowNode[rel.node], makeNonConstVal(), makeJuType(tt))
            return InferResult(rel.ctx, newnode)
        else
            error("type $(rel.node.typ.val) has no property $(ast.p)")
        end
    end
end

@nocheck function inferSetField(eng::Engine, ctx::Context, ast::SetProperty)::InferResult
    rel = inferExpr(eng, ctx, ast.x)
    # TODO : check property before
    if !hasfield(rel.node.typ.val, ast.p)
        error("type $(rel.node.typ.val) has no property $(ast.p)")
    end
    tt = fieldtype(rel.node.typ.val, ast.p)
    rel1 = inferExpr(eng, rel.node, ast.v)
    if !(el1.node.typ.val <: tt)
        error("Invalid field assignment!")
    end
    newnode = FlowNode(ast.ast, SetPropertyNode, FlowNode[rel.node, rel1.node], makeNonConstVal(), makeJuType(tt))
    InferResult(rel1.ctx, newnode)
end

# TODO : perform constant propagation for tuple type and pair here...
# actually, we need a general way to perform constant and type propagation here
# maybe some kind of dependent type ???
function inferArrayRef(eng::Engine, ctx::Context, ast::ArrayRef)::InferResult
    rels = Vector{InferResult}(undef, length(ast.i) + 1)
    for i in 1:length(rels)
        if i == 1
            rels[i] = inferExpr(eng, ctx, ast.arr)
        else
            rels[i] = inferExpr(eng, rels[i-1].ctx, ast.i[i-1])
        end
    end
    tts = Vector{FlowNode}(undef, length(rels))
    for i in 1:length(rels)
        tts[i] = rels[i].node
    end
    mm = getMethodMatches(Base.getindex, tts)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArrayRef(eng, ast.ast, tts, length(mm))
    end
    tt = extractUniqueMatch(mm)
    newnode = FlowNode(ast.ast, ArrayRefNode, tts, makeNonConstVal(), makeJuType(tt))
    InferResult(last(rels).ctx, newnode)
end

function inferArraySet(eng::Engine, ctx::Context, ast::ArraySet)::InferResult
    rels = Vector{InferResult}(undef, length(ast.i) + 2)
    for i in 1:length(rels)
        if i == 1
            rels[i] = inferExpr(eng, ctx, ast.arr)
        elseif i == lastindex(rels)
            rels[i] = inferExpr(eng, rels[i-1].ctx, ast.v)
        else
            rels[i] = inferExpr(eng, rels[i-1].ctx, ast.i[i-1])
        end
    end
    tts = Vector{FlowNode}(undef, length(rels))
    for i in 1:length(rels)
        if i == 1
            j = 1
        elseif i == 2
            j = lastindex(rels)
        else
            j = i - 1
        end
        tts[i] = rels[j].node
    end
    mm = getMethodMatches(Base.setindex!, tts)
    # TODO : check the tts here
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArraySet(eng, ast.ast, tts, length(mm))
    end
    # TODO make this type stable by using a Wrapper here
    tt = extractUniqueMatch(mm)
    newnode = FlowNode(ast.ast, ArraySetNode, tts, makeNonConstVal(), makeJuType(tt))
    InferResult(last(rels).ctx, newnode)
end

function inferVar(eng::Engine, ctx::Context, ast::Var)::InferResult
    # not a good idea, currently we treat them like function...
    if hasvar(ctx, ast.id)
        ctxval = lookup(ctx, ast.id)
        if ctxval.curtyp.nodeKind == ConditionalFlowNode
            error("Variable is conditionally defined")
        end
        node = makeVarFlowNode(ast, ctxval.curtyp)
        return InferResult(ctx, node)
    elseif isdefined(eng.mod, ast.id)
        val_ = getproperty(eng.mod, ast.id)
        val = makeConstVal(val_)
        # fix this, using a module here
        node = makeGlobalVarFlowNode(ast, val)
        return InferResult(ctx, node)
    elseif ast.id == :&& || ast.id == :||
        if ast.id == :&&
            val = makeConstVal(&)
        else
            val = makeConstVal(|)
        end
        # fix this, using a module here
        node = makeGlobalVarFlowNode(ast, val)
        return InferResult(ctx, node)
    else
        loc = ast.ast.span
        println(formatLocation(loc))
        code = loc.file.code[loc.span[1]:loc.span[2]]
        println("In module $(eng.mod), variable $(ast.id) is undefined.")
        error()
    end
end

end