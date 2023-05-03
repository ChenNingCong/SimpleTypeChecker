#=
    We directly perform type inference algorithm on JuAST
    Previously we firstly convert JuAST -> JuExpr, then we perform type inference on JuExpr
    Unfortunately, JuAST is not one-to-one mapped to JuExpr
    For example, x.v is mapping to Assign(x, :v), here Expr(:quote, :v) is translated to a symbol :v
    So that information is lost, which means some JuAST has no corresponding JuExpr and thus no corresponding debug information!
=#


#=
    When we encounter a Union, we eagerly leave type inference
    Union{...} is generally a misplaced expression or failed type assert
=#

#=
@nocheck function tryMergeFlowType(v1::FlowNode, v2::FlowNode)
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

@nocheck function tryMergeType(eng::Engine, v1::JuType, v2::JuType, allowUnion::Bool)
    @assert !isPoisonType(v1) && !isPoisonType(v2)
    newv = Union{v1.val, v2.val}
    if !allowUnion
        if newv != v1.val
            reportErrorIfEnlargeType(eng, v1, v2)
        end
    end
    return makeJuType(newv)
end
=#
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

using ..Utility
function inferExpr(eng::Engine, ctx::Context, ex::JuExpr)::InferResult
    val= ex.val
    if val isa Literal
        return inferLiteral(eng, ctx, ex, val)
    elseif val isa Assign
        return inferAssign(eng, ctx, ex, val)
    elseif val isa FunCall
        return inferFunCall(eng, ctx, ex, val)
    elseif val isa Block
        return inferBlock(eng, ctx, ex, val)
    elseif val isa Var
        return inferVar(eng, ctx, ex, val)
    elseif val isa CurlyCall
        return inferCurlyCall(eng, ctx, ex, val)
    elseif val isa IfStmt
        return inferIfStmt(eng, ctx, ex, val)
    elseif val isa Return
        return inferReturn(eng, ctx, ex, val)
    elseif val isa ArrayRef
        return inferArrayRef(eng, ctx, ex, val)
    elseif val isa ArraySet
        return inferArraySet(eng, ctx, ex, val)
    elseif val isa SetProperty
        return inferSetField(eng, ctx, ex, val)
    elseif val isa GetProperty
        return inferGetField(eng, ctx, ex, val)
    elseif val isa ForStmt
        return inferForStmt(eng, ctx, ex, val)
    elseif val isa WhileStmt
        return inferWhileStmt(eng, ctx, ex, val)
    elseif val isa DeclarationList
        return inferDeclarationList(eng, ctx, ex, val)
    elseif val isa Declaration
        return inferDeclaration(eng, ctx, ex, val)
    elseif val isa BreakStmt
        return inferBreakStmt(eng, ctx, ex, val)
    elseif val isa ContinueStmt
        return inferContinueStmt(eng, ctx, ex, val)
    elseif val isa TupleAssign
        return inferTupleAssign(eng, ctx, ex, val)
    elseif val isa TupleLiteral
        return inferTupleLiteral(eng, ctx, ex, val)
    elseif val isa UpdateAssign
        return inferUpdateAssign(eng, ctx, ex, val)
    else
        reportErrorUnimplementedAST(eng, ex.ast)
    end
end

@nocheck function makeTupleType(tt::Vector{FlowNode})::CompileType
    makeType(Tuple{[i.typ.typ for i in tt]...})
end

function inferTupleLiteral(eng::Engine, ctx::Context, ex::JuExpr, tast::TupleLiteral)::InferResult
    nodes = Vector{FlowNode}(undef, length(tast.parameters))
    for i in eachindex(tast.parameters)
        local iast = tast.parameters[i]
        rel = inferExpr(eng, ctx, iast)
        ctx = rel.ctx
        if isBottomType(rel.node.typ)
            reportErrorTupleBottom(eng, iast.ast, i)
        end
        nodes[i] = rel.node
    end
    node = FlowNode(ex, TupleLiteralFlowNode, nodes, makeTupleType(nodes))
    addFlowMapping!(eng, ex, node)
    return InferResult(ctx, node) 
end

@nocheck function tryDestructTuple(ex::JuExpr, node::FlowNode)::Union{Nothing, Vector{FlowNode}}
    t = node.typ.typ
    if (t <: Tuple && t isa DataType) || (t <: Pair && t isa DataType)
        return FlowNode[FlowNode(ex, DestructFlowNode, FlowNode[node], makeType(i)) for i in t.parameters]
    else
        return nothing
    end
end

function inferTupleAssign(eng::Engine, ctx::Context, ex::JuExpr, tast::TupleAssign)::InferResult
    ast = ex.ast
    rel = inferExpr(eng, ctx, tast.rhs)
    ctx = rel.ctx
    rhsnode = rel.node
    tts = tryDestructTuple(ex, rhsnode)
    if tts isa Nothing
        reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, not a pair or tuple")
    end
    if length(tts) != length(tast.lhss)
        reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, length mismatched")
    end
    for i in eachindex(tast.lhss)
        local newnode::FlowNode
        varid = tast.lhss[i]
        node = tts[i]
        if hasvar(ctx, varid)
            # this variable is already assigned before, we check type compatibility
            oldval = lookup(ctx, varid)
            # storage type is unchanged, update current type 
            if !tryMergeFlowType(oldval.typ, node)
                reportErrorAssignIncompatible(eng, oldval.typ, node)
            end
            newnode = makeAssignFlowNode(ex, node)
            # the primary assignment is unchanged
            val = ContextValue(oldval.typ, newnode)
        else
            newnode = makeAssignFlowNode(ex, node)
            val = ContextValue(newnode, newnode)
        end
        ctx = update(ctx, varid, val)
    end
    newnode = makeAssignFlowNode(ex, rhsnode)
    addFlowMapping!(eng, ex, newnode)
    return InferResult(ctx, newnode)
end

@nocheck function makeUpdateOp(op::Symbol)::CompileType
    return makeConstVal(getproperty(Base, op))
end

#=
function inferAssignHelper(ctx::Context, ids::Vector{Symbol}, nodes::Vector{FlowNode})::InferResult
    for i in eachindex(ids)
        varid = ids[i]
        node = nodes[i]
        if hasvar(ctx, varid)
            # this variable is already assigned before, we check type compatibility
            oldval = lookup(ctx, varid)
            # storage type is unchanged, update current type 
            if !tryMergeFlowType(oldval.typ, node)
                reportErrorAssignIncompatible(eng, oldval.typ, node)
            end
            newnode = makeAssignFlowNode(ex, node)
            # the primary assignment is unchanged
            val = ContextValue(oldval.typ, newnode)
        else
            newnode = makeAssignFlowNode(ex, node)
            val = ContextValue(newnode, newnode)
        end
        ctx = update(ctx, varid, val)
        addFlowMapping!(eng, node.ex, newnode)
    end
end
=#
function inferUpdateAssign(eng::Engine, ctx::Context, ex::JuExpr, uast::UpdateAssign)::InferResult
    # TODO : the ast is incorrect here...
    # TODO : add flow mapping here?
    ast = ex.ast
    rel = inferExpr(eng, ctx, JuExpr(Var(uast.lhs), ast))
    ctx = rel.ctx
    lhsnode = rel.node
    rel = inferExpr(eng, ctx, uast.rhs)
    ctx = rel.ctx
    rhsnode = rel.node
    # todo : check the node here
    argnodes = FlowNode[makeLiteralFlowNode(ex, makeUpdateOp(uast.op)), lhsnode, rhsnode]
    mm = getMethodMatches(eng, argnodes)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorFunCall(eng, ast, argnodes, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorAssignBottom(eng, ast, uast.lhs)
    end
    node = makeFunCallFlowNode(ex, argnodes, tt)
    varid = uast.lhs
    if hasvar(ctx, varid)
        # this variable is already assigned before, we check type compatibility
        oldval = lookup(ctx, varid)
        # storage type is unchanged, update current type 
        if !tryMergeFlowType(oldval.typ, node)
            reportErrorAssignIncompatible(eng, oldval.typ, node)
        end
        newnode = makeAssignFlowNode(ex, node)
        # the primary assignment is unchanged
        val = ContextValue(oldval.typ, newnode)
    else
        newnode = makeAssignFlowNode(ex, node)
        val = ContextValue(newnode, newnode)
    end
    ctx = update(ctx, varid, val)
    addFlowMapping!(eng, ex, newnode)
    return InferResult(ctx, newnode)
end

function inferBreakStmt(eng::Engine, ctx::Context, ex::JuExpr, val::BreakStmt)::InferResult
    InferResult(ctx, makeBreakStmtNode(ex))
end

function inferContinueStmt(eng::Engine, ctx::Context, ex::JuExpr, val::ContinueStmt)::InferResult
    InferResult(ctx, makeContinueStmtNode(ex))
end

function inferDeclarationList(eng::Engine, ctx::Context, ex::JuExpr, dl::DeclarationList)::InferResult
    nodes = similar(dl.declares, FlowNode)
    for i in eachindex(dl.declares)
        rel = inferExpr(eng, ctx, dl.declares[i])
        nodes[i] = rel.node
        ctx = rel.ctx
    end
    # todo : fix the flownode here, it should be all the f;
    node = FlowNode(ex, DeclarationListFlowNode, nodes, makeType(Nothing))
    addFlowMapping!(eng, ex, node)
    return InferResult(ctx, node)
end

function inferDeclaration(eng::Engine, ctx::Context, ex::JuExpr, d::Declaration)::InferResult
    ast = ex.ast
    rhs = d.rhs
    var = d.id
    typ = d.typ
    if !isNone(rhs)
        rel = inferExpr(eng, ctx, castJust(rhs))
        rhsnode = rel.node
        ctx = rel.ctx
        if isNone(typ)
            # like a assignment
            node = makeAssignFlowNode(ex, rhsnode)
            ctx = update(ctx, var, ContextValue(node, node))
        else
            rel = inferExpr(eng, ctx, castJust(typ))
            ctx = rel.ctx
            # storage type
            # we need to check conversion here
            # no, I choose to disallow convertion
            declaretyp = lift(rel.node.typ)
            if !tryMergeCompileValue(declaretyp, rhsnode.typ)
                reportErrorAssignInitIncompatible(eng, ast, declaretyp, rhsnode.typ)
            end
            node1 = FlowNode(ex, AssignFlowNode, FlowNode[rhsnode], declaretyp)
            ctx = update(ctx, var, ContextValue(node1, node1))
            node = makeLiteralFlowNode(ex, makeType(Nothing))
        end
    else
        # no initializer
        if isNone(typ)
            # no type declaration and no initializer
            # do nothing
            node = makeLiteralFlowNode(ex, makeType(Nothing))
        else
            # uninitialized FlowNode
            rel = inferExpr(eng, ctx, castJust(typ))
            ctx = rel.ctx
            node = FlowNode(ex, UninitializedFlowNode, FlowNode[rel.node], lift(rel.node.typ), false)
            ctx = update(rel.ctx, var, ContextValue(node, node))
            
        end
    end
    addFlowMapping!(eng, ex, node)
    return InferResult(ctx, node)
end


function inferGetField(eng::Engine, ctx::Context, ex::JuExpr, gpex::GetProperty)::InferResult
    ast = ex.ast
    rel = inferExpr(eng, ctx, gpex.x)
    node = rel.node
    ctx = rel.ctx
    ft = node.typ
    p = gpex.p
    # we need to distinguish getproperty on module and other types...
    if isBottomType(ft)
        reportErrorGetFieldOfBottom(eng, ast)
    end
    if isConstVal(ft)
        vval = ft.val
        if vval isa Module
            if isModuleDefined(vval, p)
                v = getFromModule(vval, p)
                node = makeGetPropertyFlowNode(ex, node, v)
                addFlowMapping!(eng, ex, node)
                return InferResult(ctx, node)
            else
                reportErrorNoField(eng, ast, node, p)
            end
        end
    end
    if !isConcreteType(ft)
        reportErrorFieldType(eng, ast, node, true)
    end
    if hasField(ft, p)
        tt = getFieldType(ft, p)
        newnode = makeGetPropertyFlowNode(ex, node, tt)
        addFlowMapping!(eng, ex, newnode)
        return InferResult(ctx, newnode)
    else
        reportErrorNoField(eng, ast, node, p)
    end
end

function inferSetField(eng::Engine, ctx::Context, ex::JuExpr, spex::SetProperty)::InferResult
    ast = ex.ast
    rel = inferExpr(eng, ctx, spex.x)
    ctx = rel.ctx
    p = spex.p
    xnode = rel.node
    # we need to distinguish getproperty on module and other types...
    if isBottomType(xnode.typ)
        reportErrorSetFieldOfBottom(eng, ast)
    end
    if !hasField(xnode.typ, p)
        reportErrorNoField(eng, ast, xnode, p)
    end
    tt = getFieldType(xnode.typ, p)
    rel1 = inferExpr(eng, ctx, spex.v)
    ctx = rel1.ctx
    vnode = rel1.node
    if isBottomType(vnode.typ)
        reportErrorSetFieldWithBottom(eng, ast)
    end
    if !checkFieldCompatibility(tt, vnode.typ)
        reportErrorSetFieldTypeIncompatible(eng, ast, xnode, vnode, tt, p)
    end
    newnode = makeSetPropertyFlowNode(ex, xnode, vnode, vnode.typ)
    addFlowMapping!(eng, ex, newnode)
    return InferResult(ctx, newnode)
end

# TODO : perform constant propagation for tuple type and pair here...
# actually, we need a general way to perform constant and type propagation here
# maybe some kind of dependent type ???


# TODO : infer end here
# we need to store an array context chain here, which is not hard, but just remember to do so !!!
function inferArrayRef(eng::Engine, ctx::Context, ex::JuExpr, refast::ArrayRef)::InferResult
    ast = ex.ast
    rels = Vector{InferResult}(undef, length(refast.i) + 1)
    for i in 1:length(rels)
        if i == 2
            push!(eng.arrayContext, rels[1].node)
        end
        if i == 1
            newrel = inferExpr(eng, ctx, refast.arr)
        else
            newrel = inferExpr(eng, rels[i-1].ctx, refast.i[i-1])
            if i == lastindex(rels)
                pop!(eng.arrayContext)
            end
        end
        if isBottomType(newrel.node.typ)
            reportErrorArrayRefBottom(eng, ast, i, false)
        end
        rels[i] = newrel
    end
    args = Vector{FlowNode}(undef, length(rels))
    for i in 1:length(rels)
        args[i] = rels[i].node
    end
    mm = getMethodMatches(eng, Base.getindex, args)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArrayRef(eng, ast, args, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorIndexReturnBottom(eng, ast, args)
    end
    newnode = makeArrayRefNode(ex, args, tt)
    addFlowMapping!(eng, ex, newnode)
    return InferResult(last(rels).ctx, newnode)
end


function inferArraySet(eng::Engine, ctx::Context, ex::JuExpr, setast::ArraySet)::InferResult
    ast = ex.ast
    rels = Vector{InferResult}(undef, length(setast.i) + 2)
    for i in 1:length(rels)
        if i == 2
            push!(eng.arrayContext, rels[1].node)
        end
        if i == 1
            newrel = inferExpr(eng, ctx, setast.arr)
        elseif i == lastindex(rels)
            pop!(eng.arrayContext)
            newrel = inferExpr(eng, rels[i-1].ctx, setast.v)
        else
            newrel = inferExpr(eng, rels[i-1].ctx, setast.i[i-1])
        end
        rels[i] = newrel
        if isBottomType(newrel.node.typ)
            reportErrorArrayRefBottom(eng, ast, i, i == lastindex(rels))
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
    mm = getMethodMatches(eng, Base.setindex!, tts)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArraySet(eng, ast, tts, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorIndexReturnBottom(eng, ast, tts)
    end
    newnode = makeArraySetNode(ex, tts, tt)
    addFlowMapping!(eng, ex, newnode)
    InferResult(last(rels).ctx, newnode)
end

function inferVar(eng::Engine, ctx::Context, ex::JuExpr, var::Var)::InferResult
    ast = ex.ast
    id = var.id
    # not a good idea, currently we treat them like function...
    if hasvar(ctx, id)
        ctxval = lookup(ctx, id)
        if !ctxval.curtyp.isInitialized
            reportErrorUnitializedVar(eng, ast, var.id)
        end
        node = makeVarFlowNode(ex, ctxval.curtyp)
        addFlowMapping!(eng, ex, node)
        return InferResult(ctx, node)
    elseif isModuleDefined(eng.mod, id)
        val = getFromModule(eng.mod, id)
        # fix this, using a module here

        node = makeGlobalVarFlowNode(ex, val)
        addFlowMapping!(eng, ex, node)
        return InferResult(ctx, node)
    elseif id == :&& || id == :||
        if id == :&&
            val = makeConstVal(&)
        else
            val = makeConstVal(|)
        end

        node = makeGlobalVarFlowNode(ex, val)
        addFlowMapping!(eng, ex, node)
        return InferResult(ctx, node)
    elseif id == :end
        # lastindex
        # we only support one-dimentional array!!!
        # this is incorrect
        node = eng.arrayContext[end]
        m = getMethodMatches(eng, Base.lastindex, FlowNode[node])
        # TODO : check method !!!
        tt = extractUniqueMatch(m)
        node = FlowNode(ex, VarFlowNode, FlowNode[], tt)
        addFlowMapping!(eng, ex, node)
        return InferResult(ctx, node)
    elseif id == :begin
        # firstindex
        node = eng.arrayContext[end]
        m = getMethodMatches(eng, Base.firstindex, FlowNode[node])
        # TODO : check method !!!
        tt = extractUniqueMatch(m)
        # TODO : check node here
        node = FlowNode(ex, VarFlowNode, FlowNode[], tt)
        addFlowMapping!(eng, ex, node)
        return InferResult(ctx, node)
    else
        reportErrorUndefinedVar(eng, ast, eng.mod, id)
    end
end

function inferFunCall(eng::Engine, ctx::Context, ex::JuExpr, funex::FunCall)::InferResult
    ast = ex.ast
    if length(funex.kwargs) != 0
        error("$(formatLocation(ast.loc)) Internal error : keyword function not supported yet")
    else
        # infer from left to right
        tts = Vector{FlowNode}(undef, length(funex.args) + 1)
        for i in 1:length(tts)
            if i == 1
                newrel = inferExpr(eng, ctx, funex.f)
            else
                newrel = inferExpr(eng, ctx, funex.args[i-1])
            end
            if isBottomType(newrel.node.typ)
                reportErrorFunCallUseBottom(eng, ast, i)
            end
            ctx = newrel.ctx
            tts[i] = newrel.node
        end
        argtyps = CompileType[]
        imprecise = Int[]
        for i in 1:length(tts)
            n = tts[i]
            ttypval = n.typ
            if isConcreteType(ttypval)
                push!(argtyps, ttypval)
            else
                push!(imprecise, i)
            end
        end
        if length(imprecise) > 0
            reportErrorFunCallArgs(eng, ast, tts, imprecise)
        end
        checkConstructor = false
        if isConstructor(argtyps[1])
            checkConstructor = true
        end
        # TODO : perform constant propergation here
        mm = getMethodMatches(eng, tts)
        # TODO : check the tts here
        if length(mm) >= 2 || length(mm) == 0
            reportErrorFunCall(eng, ast, tts, length(mm))
        end
        # TODO make this type stable by using a Wrapper here
        tt = extractUniqueMatch(mm)
        if isBottomType(tt) && checkConstructor
            reportErrorNoConstructor(eng, ast, tts)
        end
        # TODO : we should add constant propagation for pure builtin function !!!
        # this is again a terminal node
        node = makeFunCallFlowNode(ex, tts, tt)
        addFlowMapping!(eng, ex, node)
        return InferResult(ctx, node)
    end
end

function inferCurlyCall(eng::Engine, ctx::Context, ex::JuExpr, curlyex::CurlyCall)::InferResult
    # infer from left to right
    ast = ex.ast
    tts = Vector{FlowNode}(undef, length(curlyex.args) + 1)
    for i in 1:length(tts)
        if i == 1
            newrel = inferExpr(eng, ctx, curlyex.f)
        else
            newrel = inferExpr(eng, ctx, curlyex.args[i-1])
        end
        if isBottomType(newrel.node.typ)
            reportErrorCurlyCallUseBottom(eng, ast, i)
        end
        ctx = newrel.ctx
        tts[i] = newrel.node
    end
    argtyps = CompileType[]
    imprecise = Int[]
    for i in 1:length(tts)
        nodetyp = tts[i].typ
        if !isConstVal(nodetyp)
            push!(imprecise, i)
        else
            push!(argtyps, nodetyp)
        end
    end
    if length(imprecise) > 0
        reportErrorApplyTypeNonconst(eng, ast, imprecise)
    end
    mtt = tryApplyType(argtyps)
    if isNone(mtt)
        reportErrorApplyTypeFailure(eng, ast, tts)
    end
    tt = castJust(mtt)
    node = makeCurlyCallFlowNode(ex, tts, tt)
    addFlowMapping!(eng, ex, node)
    return InferResult(ctx, node)
end

function inferAssign(eng::Engine, ctx::Context, ex::JuExpr, assex::Assign)::InferResult
    ast = ex.ast
    rel = inferExpr(eng, ctx, assex.rhs)
    retctx = rel.ctx
    retnode = rel.node
    varid = assex.lhs
    if isBottomType(retnode.typ)
        reportErrorAssignBottom(eng, ast, varid)
    end
    if hasvar(retctx, varid)
        # this variable is already assigned before, we check type compatibility
        oldval = lookup(retctx, varid)
        # storage type is unchanged, update current type 
        if !tryMergeFlowType(oldval.typ, retnode)
            reportErrorAssignIncompatible(eng, oldval.typ, retnode)
        end
        newnode = makeAssignFlowNode(ex, retnode)
        # the primary assignment is unchanged
        val = ContextValue(oldval.typ, newnode)
    else
        newnode = makeAssignFlowNode(ex, retnode)
        val = ContextValue(newnode, newnode)
    end
    ctx = update(retctx, varid, val)
    addFlowMapping!(eng, ex, newnode)
    return InferResult(ctx, newnode)
end


function inferBlock(eng::Engine, ctx::Context, ex::JuExpr, ast::Block)::InferResult
    if length(ast.stmts) == 0
        newnode = makeEmptyBlockFlowNode(ex)
        return InferResult(ctx, newnode)
    else
        for i in 1:length(ast.stmts)-1
            ret = inferExpr(eng, ctx, ast.stmts[i])
            ctx = ret.ctx
            if isBottomType(ret.node.typ)
                reportErrorPrematureUnreachable(eng, ast.stmts[i].ast)
            end
        end
    end
    ret = inferExpr(eng, ctx, last(ast.stmts))
    newnode = makeBlockFlowNode(ex, ret.node)
    addFlowMapping!(eng, ex, newnode)
    return InferResult(ret.ctx, newnode)
end

function inferLiteral(eng::Engine, ctx::Context, ex::JuExpr, ast::Literal)::InferResult
    node = makeLiteralFlowNode(ex, ast.val)
    addFlowMapping!(eng, ex, node)
    return InferResult(ctx, node)
end

# TODO : we should terminate after we return (we need to cut the value here)
function inferReturn(eng::Engine, ctx::Context, ex::JuExpr, rast::Return)::InferResult
    ast = ex.ast
    aste = rast.e
    engn = eng.retVal
    if isNone(aste)
        # TODO : We create a literal node here!!!
        rettnode = makeLiteralFlowNode(ex, makeType(Nothing))
        newnode = makeEmptyReturnFlowNode(ex)
    else
        rel = inferExpr(eng, ctx, castJust(aste))
        rettnode = rel.node
        newnode = makeReturnFlowNode(ex, rettnode)
        ctx = rel.ctx
    end
    if !isNone(engn)
        if !tryMergeReturnFlowNode(castJust(engn), rettnode)
            reportErrorReturnEnlargeType(eng, castJust(engn), rettnode)
        end
    end
    eng.retVal = Just(rettnode)
    addFlowMapping!(eng, ex, newnode)
    # after this expression, all declaration is cleared
    return InferResult(Context(), newnode)
end

function tryNarrowType(eng::Engine, ctx::Context, ex::JuExpr)::Pair{InferResult, Context}
    e = ex.val
    ast = ex.ast
    if e isa FunCall
        args = e.args
        if length(e.args) == 2 && length(e.kwargs) == 0
            f = e.f.val
            x = e.args[1].val
            rhsex = e.args[2]
            if f isa Var
                if f.id == :isa  
                    if x isa Var
                        # TODO : we need to add FlowNode to all the JuExpr here
                        isanode = makeGlobalVarFlowNode(e.f, makeConstVal(isa))
                        addFlowMapping!(eng, e.f, isanode)

                        rel = inferExpr(eng, ctx, rhsex)
                        ctx = rel.ctx
                        rhsnode = rel.node
                        rhstt = rhsnode.typ
                        if !isConstVal(rhstt)
                            reportErrorBadIsa(eng, ast, "rhs of isa is not a constant value")
                        end
                        rhstt = lift(rhstt)
                        # TODO : lift the constant here
                        # x isa XXX type, we life XXX to a type before we perform comparsion
                        if !isaType(rhstt)
                            reportErrorIsaLHSBadType(eng, ast, rhstt)
                        end
                        # then we lookup lhs of isa
                        rel = inferExpr(eng, ctx, e.args[1])
                        ctx = rel.ctx
                        xnode = rel.node
                        # rhs must be a subtype of lhs
                        # we can't use tryMergeFlowType here, because it's not lifted here
                        if !tryMergeCompileValue(xnode.typ, rhstt)
                            reportErrorMismatchedSplit(eng, ast, xnode, rhsnode)
                        end
                        condnode = makeFunCallFlowNode(ex, FlowNode[isanode, xnode, rhsnode], makeType(Bool))
                        addFlowMapping!(eng, ex, condnode)
                        # must hasvar here
                        ctxval = lookup(ctx, x.id)
                        curtt = ctxval.typ.typ
                        storetypenode = ctxval.curtyp
        
                        pinode = makePiNode(ex, condnode, rhstt)
                        # narrow type to rhs
                        ctxval = ContextValue(storetypenode, pinode)
                        newctx = update(ctx, x.id, ctxval)
                        # TODO : maybe we should use a cond type here

                        newrel = InferResult(newctx, condnode)
                        if isaAny(curtt)
                            # on another branch, ctx is not narrowed
                            return newrel => ctx
                        elseif isaUnion(curtt)
                            negtt = splitUnion(curtt, rhstt)
                            negpinode = makeNegPiNode(ex, condnode, negtt)
                            negctx = update(ctx, x.id, ContextValue(storetypenode, negpinode))
                            return newrel => negctx
                        else
                            reportErrorIsaLHSBadType(eng, ast, rhstt)
                        end
                    else
                        reportErrorIsaBadForm(eng, ast, "`isa` is not in a predicative form")
                    end
                end
            end
        end
    end
    rel = inferExpr(eng, ctx, ex)
    if !isaBool(rel.node.typ)
        reportErrorCondNotBool(eng, rel.node)
    end
    return rel => rel.ctx
end

function inferIfStmt(eng::Engine, ctx::Context, ex::JuExpr, ifex::IfStmt)::InferResult
    ast = ex.ast
    # no pi projection
    rels = Vector{InferResult}()
    prectx = ctx
    for branch in ifex.branches
        tmp1 = tryNarrowType(eng, ctx, branch.first)
        rel1 = tmp1.first
        # ctx here is the ctx for negative branch
        ctx = tmp1.second
        rel2 = inferExpr(eng, rel1.ctx, branch.second)
        push!(rels, rel2)
    end
    el = ifex.else_
    if isNone(el)
        node = makeEmptyElseFlowNode(ex)
        # use the context of negative branch
        push!(rels, InferResult(ctx, node))
    else
        push!(rels, inferExpr(eng, ctx, castJust(el)))
    end

    # firstly, we detect which branch is reachable
    reachflags = similar(rels, Bool)
    fill!(reachflags, true)
    for i in eachindex(reachflags)
        if isBottomType(rels[i].node.typ)
            reachflags[i] = false
        end
    end
    if !any(reachflags)
        # no branch is reachable, we directly skip the function
        return InferResult(Context(), makePhiFlowNode(ex, FlowNode[], makeBottomType()))
    end

    # we need to consider reacheable and unreachable here
    # Case 1 : variable defined before if
    defined = Dict{Symbol, ContextValue}()
    for pair in prectx.mapping.data
        name = pair.first
        typs = FlowNode[]
        for i in eachindex(rels)
            if reachflags[i]
                push!(typs, lookup(rels[i].ctx, name).curtyp)
            end
        end
        condDef = false
        for i in typs
            if !i.isInitialized
                condDef = true
                break
            end
        end
        if condDef
            curnode = FlowNode(ex, ConditionalFlowNode, FlowNode[], makeBottomType(), false) 
        else
            curnode = tryMergeFlowNode(eng, ex, typs, true)
        end
        ctxval = pair.second
        defined[name] = ContextValue(ctxval.typ, curnode)
    end
    # Case 2 : variable defined in if, maybe conditionally
    newnames = Set{Symbol}()
    for rel in rels
        for i in keys(rel.ctx.mapping.data)
            if !hasvar(prectx, i)
                push!(newnames, i)
            end
        end
    end
    non_defined = Dict{Symbol, ContextValue}()
    for name in newnames
        condDef = false
        for i in eachindex(rels)
            rel = rels[i]
            # TODO : check whether the variable is also unintialized in the if
            if reachflags[i] && !hasvar(rel.ctx, name) 
                condDef = true
                break
            end
        end

        storetyps = FlowNode[]
        for i in eachindex(rels)
            rel = rels[i]
            if hasvar(rel.ctx, name)
                push!(storetyps, lookup(rel.ctx, name).curtyp)
            end
        end

        curtyps = FlowNode[]
        for i in eachindex(rels)
            rel = rels[i]
            if hasvar(rel.ctx, name) && reachflags[i]
                push!(curtyps, lookup(rel.ctx, name).curtyp)
            end
        end
        # check whether the type is compatible in every branch, even some of them is unreachable
        storenode = tryMergeFlowNode(eng, ex, storetyps, false)
        # TODO : this is incorrect
        # even if a definition is killed, we need to check whether its type is compatiable in each branch
        if length(curtyps) != 0
            node = tryMergeFlowNode(eng, ex, curtyps, false)
            if condDef
                node = FlowNode(ex, ConditionalFlowNode, FlowNode[node], node.typ, false)
            end
            value = ContextValue(node, node)
            non_defined[name] = value
        else
            # even if a variable's definition is killed in every branch
            # we need to record it here
            #=
            if y
                x = 1
                error()
            else
                x = 2
                error()
            end
            # here x appears as no definition
            print(x)
            =#
            # it appes
            node = FlowNode(ex, ConditionalFlowNode, FlowNode[], makeBottomType(), false) 
            value = ContextValue(node, node)
            non_defined[name] = value
        end
    end
    newctx = Context(ImmutableDict(Dict{Symbol, ContextValue}(merge(defined, non_defined))))
    relnodes = similar(rels, FlowNode)
    for i in eachindex(relnodes)
        relnodes[i] = rels[i].node
    end
    retnode = tryMergeFlowNode(eng, ex, relnodes, true)
    return InferResult(newctx, retnode)
end


function decideScopeVariable!(rel::Set{Symbol}, mod::Set{Symbol}, shadow::Set{Symbol}, ctx::Context, ast::JuExpr)::Nothing
    val = ast.val
    if val isa Literal
        return
    elseif val isa Assign
        id = val.lhs
        if id in shadow
            return
        end
        if hasvar(ctx, id)
            # variable is modified
            push!(mod, id)
            return
        else
            # a new local variable is created
            push!(rel, id)
            return
        end
    elseif val isa FunCall
        decideScopeVariable!(rel, mod, shadow, ctx, val.f)
        for i in val.args
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        for i in val.kwargs
            decideScopeVariable!(rel, mod, shadow, ctx, i.second)
        end
        return
    elseif val isa Block
        for i in val.stmts
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
    elseif val isa Var
        return
    elseif val isa CurlyCall
        decideScopeVariable!(rel, mod, shadow, ctx, val.f)
        for i in val.args
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa IfStmt
        for i in val.branches
            decideScopeVariable!(rel, mod, shadow, ctx, i[1])
            decideScopeVariable!(rel, mod, shadow, ctx, i[2])
        end
        else_e = val.else_
        if isNone(else_e)
            return
        else
            decideScopeVariable!(rel, mod, shadow, ctx, castJust(else_e))
        end
    elseif val isa Return
        eval = val.e
        if isNone(eval)
            return
        else
            decideScopeVariable!(rel, mod, shadow, ctx, castJust(eval))
            return
        end
    elseif val isa GetProperty
        decideScopeVariable!(rel, mod, shadow, ctx, val.x)
        return 
    elseif val isa SetProperty
        decideScopeVariable!(rel, mod, shadow, ctx, val.x)
        decideScopeVariable!(rel, mod, shadow, ctx, val.v)
        return
    elseif val isa ArrayRef
        decideScopeVariable!(rel, mod, shadow, ctx, val.arr)
        for i in val.i
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa ArraySet
        decideScopeVariable!(rel, mod, shadow, ctx, val.arr)
        for i in val.i
            decideScopeVariable!(rel, mod, shadow, ctx,i)
        end
        decideScopeVariable!(rel, mod, shadow, ctx, val.v)
        return
    elseif val isa TypedAssert
        decideScopeVariable!(rel, mod, shadow, ctx, val.lhs)
        decideScopeVariable!(rel, mod, shadow, ctx, val.rhs)
        return
    elseif val isa LetStmt
        # TODO check this
        for pair in val.declares
            push!(shadow, pair.first)
        end
        # we prevent modification of rel
        # because this is a subscope
        decideScopeVariable!(Set{Symbol}(), mod, shadow, ctx, val.body)
        for pair in val.declares
            pop!(shadow)
        end
        return 
    elseif val isa ForStmt
        decideScopeVariable!(rel, mod, shadow, ctx, val.iter)
        push!(shadow, val.var)
        # we prevent modification of rel
        # because this is a subscope
        decideScopeVariable!(Set{Symbol}(), mod, shadow, ctx, val.body)
        pop!(shadow)
        return 
    elseif val isa WhileStmt
        decideScopeVariable!(rel, mod, shadow, ctx, val.cond)
        # we prevent modification of rel
        # because this is a subscope
        decideScopeVariable!(Set{Symbol}(), mod, shadow, ctx, val.body)
    elseif val isa ContinueStmt
        return
    elseif val isa BreakStmt
        return
    elseif val isa DeclarationList
        for i in val.declares
            l = i.val
            if l isa Declaration
                push!(rel, l.id)
            else
                error("Internal Error")
            end
        end
    else
        error("Unimplemented $ast")
    end
end

function decideScopeVariable(ctx::Context, ast::JuExpr)
    rel = Set{Symbol}()
    mod = Set{Symbol}()
    shadow = Set{Symbol}()
    decideScopeVariable!(rel, mod, shadow, ctx, ast)
    return rel => mod
end

function iterateCheck(eng::Engine, node::FlowNode)::FlowNode
    # TODO : check Union here, all the input should be concrete type
    matches = getMethodMatches(eng, Base.iterate, FlowNode[node])
    if length(matches) != 1
        error("iterate fails to match")
    end
    # TODO check another part of iterate, iterate(iterator, val)
    tt = extractUniqueMatch(matches)
    if !isaUnion(tt)
        error("Not a union for iterator")
    end
    tt = splitUnion(tt, makeType(Nothing))
    if isaTuple(tt)
        tt = getFirstParameter(tt)
    else
        error("Not a tuple for iterator")
    end
    node = FlowNode(node.ex, ForStmtVarNode, FlowNode[node], tt)
    return node
end

function inferForStmt(eng::Engine, ctx::Context, ex::JuExpr, forast::ForStmt)::InferResult
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant
    ast = ex.ast
    rel = inferExpr(eng, ctx, forast.iter)
    ctx = rel.ctx
    varid = forast.var
    varnode = iterateCheck(eng, rel.node)
    tmp = decideScopeVariable(ctx, forast.body)
    newvars = tmp.first
    modvars = tmp.second
    newmapping = Dict{Symbol, ContextValue}()
    newmapping[varid] = ContextValue(varnode, varnode)
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if k in modvars && k != varid
            z = FlowNode(ex, ForUpdateNode, FlowNode[v.curtyp], v.curtyp.typ, v.curtyp.isInitialized) 
            newmapping[k] = ContextValue(v.typ, z)
        elseif k in newvars || k == varid
            # k in newvars means k is shadowed, we skip them here
            # new variable 
            # continue
        else
            # unchanged
            newmapping[k] = v
        end
    end
    newctx = Context(ImmutableDict(newmapping))
    re2 = inferExpr(eng, newctx, forast.body)
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if k in modvars && k != varid
            if !newmapping[k].curtyp.isInitialized
                newmapping2[k] = ContextValue(newmapping[k].typ, FlowNode(ex, ConditionalFlowNode, FlowNode[], makeBottomType(), false))
            else
                newmapping2[k] = ContextValue(newmapping[k].typ, tryMergeFlowNode(eng, ex, FlowNode[lookup(ctx, k).curtyp, lookup(re2.ctx, k).curtyp], false))
            end
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end
    retnode = FlowNode(ex, ForStmtFlowNode, FlowNode[], makeType(Nothing))
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end

function inferWhileStmt(eng::Engine, ctx::Context, ex::JuExpr, whileast::WhileStmt)::InferResult
    # infer of while is almost identical to forstmt
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant

    # scope of while is tricky
    # what's the scope of x?
    # while (x > 0)
    #   local x
    # end
    # currently we assume this is invalid...
    # TODO (FIX THIS)
    ast = ex.ast
    rel = inferExpr(eng, ctx, whileast.cond)
    ctx = rel.ctx
    tmp = decideScopeVariable(ctx, whileast.body)
    newvars = tmp.first
    modvars = tmp.second
    newmapping = Dict{Symbol, ContextValue}()
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if k in modvars
            z = FlowNode(ex, ForUpdateNode, FlowNode[v.curtyp], v.curtyp.typ, v.curtyp.isInitialized) 
            newmapping[k] = ContextValue(v.typ, z)
        elseif k in newvars
            continue
        else
            # unchanged
            newmapping[k] = v
        end
    end
    newctx = Context(ImmutableDict(newmapping))
    re2 = inferExpr(eng, newctx, whileast.body)
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if k in modvars
            if !newmapping[k].curtyp.isInitialized
                newmapping2[k] = ContextValue(newmapping[k].typ, FlowNode(ex, ConditionalFlowNode, FlowNode[], makeBottomType(), false))
            else
                newmapping2[k] = ContextValue(newmapping[k].typ, tryMergeFlowNode(eng, ex, FlowNode[lookup(ctx, k).curtyp, lookup(re2.ctx, k).curtyp], false))
            end
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end
    retnode = FlowNode(ex, ForStmtFlowNode, FlowNode[], makeType(Nothing))
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end

@nocheck function displayResult(io::IO, rel::InferResult)
    displayContext(io, rel.ctx)
end

@nocheck function displayContext(io::IO, ctx::Context)
    all = sort(collect(ctx.mapping.data), by = x->x[1])
    cons_v = [i for i in all if isConstVal(i[2].curtyp.typ)]
    other_v = [i for i in all if !isConstVal(i[2].curtyp.typ) && i[2].curtyp.nodeKind != ConditionalFlowNode]
    other_v_cond = [i for i in all if !isConstVal(i[2].curtyp.typ) && i[2].curtyp.nodeKind == ConditionalFlowNode]
    if !isempty(cons_v)
        println(io, "Constant :")
        for (k,v) in cons_v
            println(io, "  $k : $(v.curtyp.typ.val)")
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
    if isempty(all)
        println(io, "(Empty Context)")
    end
end


function displayReturn(io::ErrorLogger, eng::Engine)
    val = eng.retVal
    if isNone(val)
    else
        println(io, "Return Type : $(toString(castJust(val).typ))")
    end
end

@nocheck function testInferForFunction(globalctx::GlobalContext, ex::JuExpr, sourceMapping::SourceMapping, mi::Core.MethodInstance)
    ast = ex.ast
    funast = ex.val
    if !(funast isa FunDef)
        error()
    end
    smapping = Dict{Symbol, ContextValue}()
    if length(mi.sparam_vals) != length(funast.params)
        error("mismatched param size")
    end
    for i in 1:length(mi.sparam_vals)
        local tt = mi.sparam_vals[i]
        # TODO : change the definition here...
        # not make type, but make constant value
        node = FlowNode(ex, SparamNode, FlowNode[], makeConstVal(tt))
        smapping[funast.params[i]] = ContextValue(node, node)
    end
    mapping = Dict{Symbol, ContextValue}()
    argtts = mi.specTypes.parameters
    if length(argtts) != length(funast.args) + 1
        println(length(funast.args))
        println(funast.args)
        error("mismatched argument size")
    end
    for i in 1:length(funast.args)
        local tt = argtts[i+1]
        # TODO : change the definition here...
        node = FlowNode(ex, ParamNode, FlowNode[], makeType(tt))
        mapping[funast.args[i].first] = ContextValue(node, node)
    end
    ctx = Context(ImmutableDict(merge(smapping, mapping)))
    eng = Engine(globalctx, mi, mi.def.module, sourceMapping)
    # Firstly, infer return type
    astrt = funast.rt 
    if !isNone(astrt)
        # TODO : fix the flow node here
        rel = inferExpr(eng, ctx, castJust(astrt))
        # TODO : use a interface functon for val -> typ conversion
        eng.retVal = Just(FlowNode(ex, ExpectedReturnNode, FlowNode[], makeType(rel.node.typ.val)))
        ctx = rel.ctx
    end
    rel = inferExpr(eng, ctx, funast.body)
    newnode = rel.node
    # we need to return the value here
    engn = eng.retVal
    if isNone(engn)
        eng.retVal = Just(newnode)
    else
        if !tryMergeReturnFlowNode(castJust(engn), newnode)
            reportErrorReturnEnlargeType(eng, castJust(engn), newnode)
        end
    end
    return InferReport(ex, mi, eng, rel)
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