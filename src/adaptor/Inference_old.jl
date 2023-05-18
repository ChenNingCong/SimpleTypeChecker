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



function checkScopeVariable!(rel::Set{Symbol}, mod::Set{Symbol}, shadow::Set{Symbol}, ctx::Context, id::Symbol)::Nothing
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
end
function decideScopeVariable!(rel::Set{Symbol}, mod::Set{Symbol}, shadow::Set{Symbol}, ctx::Context, ast::JuExpr)::Nothing
    val = ast.val
    if val isa Literal
        return
    elseif val isa TupleLiteral
        for i in val.parameters
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa TupleAssign
        for i in val.lhss
            checkScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa UpdateAssign
        return checkScopeVariable!(rel, mod, shadow, ctx, val.lhs)
    elseif val isa Assign
        return checkScopeVariable!(rel, mod, shadow, ctx, val.lhs)
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