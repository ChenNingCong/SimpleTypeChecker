module Interface
include("datastruct.jl")
include("type.jl")
include("utility.jl")
include("adapter.jl")
include("error.jl")

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
    newnode = FlowNode(ast.ast, ArrayRefNode, tts, makeNonConstVal(), tt)
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
    newnode = FlowNode(ast.ast, ArraySetNode, tts, makeNonConstVal(), tt)
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


function inferFunCall(eng::Engine, ctx::Context, ast::FunCall)::InferResult
    if length(ast.kwargs) != 0
        error()
    else
        # infer from left to right
        ret = inferExpr(eng, ctx, ast.f)
        ctx = ret.ctx
        tts = FlowNode[ret.node]
        for i in 1:length(ast.args)
            ret = inferExpr(eng, ctx, ast.args[i])
            ctx = ret.ctx
            push!(tts, ret.node)
        end
        argtyps = Any[]
        imprecise = Int[]
        for i in 1:length(tts)
            n = tts[i]
            ttypval = n.typ.val
            if isconcretetype(ttypval) || ttypval <: Type
                push!(argtyps, ttypval)
            else
                push!(imprecise, i)
            end
        end
        if length(imprecise) > 0
            reportErrorFunCallArgs(eng, ast.ast, tts, imprecise)
        end
        # TODO : perform constant propergation here
        mm = getMethodMatches(tts)
        # TODO : check the tts here
        if length(mm) >= 2 || length(mm) == 0
            reportErrorFunCall(eng, ast.ast, tts, length(mm))
        end
        # TODO make this type stable by using a Wrapper here
        tt = extractUniqueMatch(mm)
        # TODO : we should add constant propagation for pure builtin function !!!
        # this is again a terminal node
        node = makeFunCallFlowNode(ast, FlowNode[], makeNonConstVal(), tt)
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
            println(ast.ast, "non constant apply type")
            error()
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
        newnode = makeEmptyReturnFlowNode(ast)
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
                        condnode = FlowNode(e.ast, FunCallNode, FlowNode[], makeNonConstVal(), makeJuType(Bool))
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
    #=
    println("Debug If stmt")
    if length(ast.branches) == 0
        println(ast)
        error()
    end
    for i in 1:length(ast.branches)
        println("Context after $i-th branch")
        displayResult(stdout, rels[i])
    end
    if el isa Nothing
        println("Context fall through branch")
        displayResult(stdout, last(rels))
    else
        println("Context after else branch")
        displayResult(stdout, last(rels))
    end
    =#
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

function decideScopeVariable!(rel::Set{Symbol}, mod::Set{Symbol}, shadow::Set{Symbol}, ctx::Context, ast::JuExpr)::Nothing
    val = ast.val
    if val isa Literal
        return
    elseif val isa Assign
        id = val.lhs.id
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
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa Block
        for i in val.stmts
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
    elseif val isa Var
        return
    elseif val isa CurlyCall
        decideScopeVariable!(rel, mod, ctx, val.f)
        for i in f.args
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa IfStmt
        for i in val.branches
            decideScopeVariable!(rel, mod, shadow, ctx, i[1])
            decideScopeVariable!(rel, mod, shadow, ctx, i[2])
        end
        else_e = val.else_
        if else_e isa Nothing
            return
        else
            decideScopeVariable!(rel, mod, shadow, ctx, else_e)
        end
    elseif val isa Return
        eval = val.e
        if eval isa Nothing
            return
        else
            decideScopeVariable!(rel, mod, shadow, ctx, eval)
            return
        end
    elseif val isa ArrayRef
        decideScopeVariable!(rel, mod, shadow, ctx, val.arr)
        for i in val.i
            decideScopeVariable!(rel, mod, shadow, ctx, i)
        end
        return
    elseif val isa GetProperty
        decideScopeVariable!(rel, mod, shadow, ctx, val.x)
        return 
    elseif val isa SetProperty
    elseif val isa ArraySet
        decideScopeVariable!(rel, mod, shadow, ctx, val.arr)
        for i in val.i
            decideScopeVariable!(rel, mod, shadow, ctx,i)
        end
        decideScopeVariable!(rel, mod, shadow, ctx, val.v)
        return
    elseif val isa TypedAssert
        decideScopeVariable!(rel, mod, shadow, ctx, lhs)
        decideScopeVariable!(rel, mod, shadow, ctx, rhs)
        return
    elseif val isa ForStmt
        decideScopeVariable!(rel, mod, shadow, ctx, val.iter)
        push!(mod, val.var.id)
        # we prevent modification of rel
        # because this is a subscope
        decideScopeVariable!(Set{Symbol}(), mod, shadow, ctx, val.body)
        pop!(mod)
        return 
    elseif val isa WhileStmt
        decideScopeVariable!(rel, mod, shadow, ctx, val.cond)
        # we prevent modification of rel
        # because this is a subscope
        decideScopeVariable!(Set{Symbol}(), mod, shadow, ctx, val.body)
    else
        error("Unimplemented $ast")
    end
end

function decideScopeVariable(ctx::Context, ast::JuExpr)
    rel = Set{Symbol}()
    mod = Set{Symbol}()
    shadow = Set{Symbol}()
    decideScopeVariable!(rel, mod, shadow, ctx, ast)
    return rel, mod
end

function iterateCheck(node::FlowNode)::FlowNode
    # TODO : check Union here, all the input should be concrete type
    matches = Base.code_typed_by_type(Tuple{typeof(Base.iterate), node.typ.val})
    if length(matches) != 1
        error("iterate fails to match")
    end
    # TODO check another part of iterate, iterate(iterator, val)
    tt = matches[1][2]
    if !(tt isa Union)
        error("Not a union for iterator")
    end
    tt = splitUnion(tt, Nothing)
    if tt <: Tuple
        tt = tt.parameters[1]
    else
        error("Not a tuple for iterator")
    end
    return FlowNode(node.ast, ForVarNode, [node], makeNonConstVal(), makeJuType(tt))
end

function inferForStmt(eng::Engine, ctx::Context, ast::ForStmt)::InferResult
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant
    var = ast.var
    rel = inferExpr(eng, ctx, ast.iter)
    ctx = rel.ctx
    varnode = iterateCheck(rel.node)
    newvars, modvars = decideScopeVariable(ctx, ast.body)
    newmapping = Dict{Symbol, ContextValue}()
    newmapping[var.id] = ContextValue(varnode, varnode)
    for tmp in ctx.mapping.data
        k = tmp[1]
        v = tmp[2]
        if k in modvars && k != var.id
            z = FlowNode(ast.ast, ForUpdateNode, [v.curtyp], makeNonConstVal(), v.curtyp.typ) 
            newmapping[k] = ContextValue(v.typ, z)
        elseif k in newvars || k == var.id
            # k in newvars means k is shadowed, we skip them here
            # new variable 
            continue
        else
            # unchanged
            newmapping[k] = v
        end
    end
    newctx = Context(ImmutableDict(newmapping))
    re2 = inferExpr(eng, newctx, ast.body)
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp[1]
        v = tmp[2]
        if k in modvars && k != var.id
            newmapping2[k] = ContextValue(newmapping[k].typ, tryMergeFlowNode(ast.ast, [lookup(ctx, k).curtyp, lookup(re2.ctx, k).curtyp], false))
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end
    #=
    println("---Debug here---")
    println("Context after iterate")
    displayContext(stdout, ctx)
    println("Context after body")
    displayContext(stdout, re2.ctx)
    println("Context after merge")
    displayContext(stdout, Context(ImmutableDict(newmapping2)))
    =#
    retnode = FlowNode(ast.ast, ForEndNode, FlowNode[], makeConstVal(nothing), makeJuType(Nothing))
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end

function inferWhileStmt(eng::Engine, ctx::Context, ast::WhileStmt)::InferResult
    error()
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
    eng = Engine(typeof(f).name.module)
    # Firstly, infer return type
    astrt = ast.rt 
    if astrt != nothing
        # TODO : fix the flow node here
        rel = inferExpr(eng, ctx, astrt)
        # TODO : use a interface functon for val -> typ conversion
        eng.retVal = FlowNode(ast.ast, ExpectedReturnNode, FlowNode[], makeNonConstVal(), makeJuType(rel.node.val.val))
        ctx = rel.ctx
    end
    rel = inferExpr(eng, ctx, ast.body)
    # we need to return the value here
    engn = eng.retVal
    if engn isa Nothing
        eng.retVal = rel.node
    else
        tryMergeFlowNode(ast.ast, [engn, rel.node], true)
    end
    return InferReport(ast, tt, eng, rel)
end

function displayReport(io::IO, r::InferReport)
    println(io, '\u2500'^64)
    println(io, "Inference Result for Function $(r.f.f)")
    println(io, "  argtype : $(r.tt)")
    displayReturn(stdout, r.eng)
    displayResult(stdout, r.rel)
    println(io, '\u2500'^64)
    return
end

function extractFunDef!(d, curmod, rel::Vector{FunDef}, ast::JuExpr)
    e = ast.val
    if e isa ModDef
        push!(curmod, e.name)
        rel = FunDef[]
        extractFunDef!(d, curmod, rel, e.stmts)
        d[copy(curmod)] = rel
        pop!(curmod)
    elseif e isa Block
        for i in e.stmts
            extractFunDef!(d, curmod, rel, i)
        end
    elseif e isa FunDef
        push!(rel, e)
    end
    return
end

function decomposeModule(defmod::Core.Module)
    rel = Symbol[nameof(defmod)]
    while Base.parentmodule(defmod) != defmod
        defmod = Base.parentmodule(defmod)
        push!(Symbol[nameof(defmod)])
    end
    return rel
end

function extractFunDef(ast::JuExpr, defmod::Core.Module)
    curmod = decomposeModule(defmod)
    d = Dict{Vector{Symbol}, Vector{FunDef}}()
    rel = FunDef[]
    extractFunDef!(d, curmod, rel, ast)
    d[copy(curmod)] = rel
    return d
end

end
#=
    Given a JuAST, convert it to the typed representation of AST
=#