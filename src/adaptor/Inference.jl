import ..SyntaxDefinition.JuAST
import ..SyntaxDefinition.JuASTVal
import ..SyntaxDefinition.formatLocation
import ..SyntaxDefinition.makeConstJuASTVal
import ..SyntaxAdaptor.parseJuAST
struct InternalError
    msg::String
end

struct SyntaxError
    ast::JuAST
    msg::String
end

struct UnimplementSyntaxError
    ast::JuAST
    msg::String
end

function reportASTError(eng::Engine, ast::JuAST, msg::String)::Union{}
    Base.throw(SyntaxError(ast, msg))
end

function reportASTError(ctx::GlobalContext, ast::JuAST, msg::String)::Union{}
    Base.throw(SyntaxError(ast, msg)) 
end


function reportUnimplementedASTError(eng::Engine, ast::JuAST, msg::String)::Union{}
    Base.throw(SyntaxError(ast, msg))
end

#=
Some utility functions to handle AST
=#

function cast2Symbol(val::JuASTVal)::Symbol
    if !val.isconst
        throw(InternalError("Failed to cast value : not a valid constant value"))
    end
    valval = val.val
    if valval isa Symbol
        return valval
    else
        throw(InternalError(("Failed to cast value : not a symbol")))
        
    end
end


@nocheck function isaJuASTVal(val::JuASTVal, typ)::Bool
    if !val.isconst
        throw(InternalError("Not a valid literal value"))
    end
    return val.val isa typ
end

@nocheck function castJuASTVal(typ::Type{T}, val::JuASTVal)::T where T
    if val.isconst
        valval = val.val
        if valval isa T
            return valval
        else
            throw(InternalError("Failed to cast value : type mismatched"))
        end
    else
        throw(InternalError(("Failed to cast value : not a valid constant value")))
    end
end

function assertASTKind(ast::JuAST, typ::Symbol)::Nothing
    if ast.head != typ
        throw(InternalError("AST head is mismatched: expected $typ, given $(ast.head)"))
    end
    return nothing
end

function inferLiteral(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    node = makeLiteralFlowNode(ast, ast.val)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferCharLiteral(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :char)
    if length(ast.args) == 1 && ast.args[1].head == :literal
        charval = ast.args[1].val
        if isaJuASTVal(charval, Char)
            node = makeLiteralFlowNode(ast, charval)
            addFlowMapping!(eng, ast, node)
            addFlowMapping!(eng, ast.args[1], node)
            return InferResult(ctx, node)
        end
    end
    reportASTError(eng, ast, "Not a valid character literal")
end

function pushdownFlowNode(eng::Engine, ast::JuAST, node::FlowNode)::Nothing
    addFlowMapping!(eng, ast, node)
    for i in ast.args
        pushdownFlowNode(eng, i, node)
    end
end

function inferQuoteLiteral(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :quote)
    if length(ast.args) != 1
        reportASTError(eng, ast, "Quote expression should only have one child")
    end
    qast = ast.args[1]
    local node::FlowNode
    if qast.head == :identifier
        node = makeLiteralFlowNode(ast, qast.val)
        addFlowMapping!(eng, ast, node)
        addFlowMapping!(eng, qast, node)
    elseif qast.head == :block
        node = makeLiteralExprFlowNode(ast) 
        pushdownFlowNode(eng, ast, node)
    else
        reportUnimplementedASTError(eng, ast, "Quoted expression only supports for symbol (:x) and block expressions")
    end
    return InferResult(ctx, node)
end

function inferMacroCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    #= TODO : add a macro explander here !!!=#
    node = makeMacroCallFlowNode(ast)
    pushdownFlowNode(eng, ast, node)
    return InferResult(ctx, node)
end

function inferCurlyCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :curly)
    tts = Vector{FlowNode}(undef, length(ast.args))
    for i in 1:length(tts)
        rel = inferExpr(eng, ctx, ast.args[i])
        ctx = rel.ctx
        tts[i] = rel.node
        if isBottomType(rel.node.typ)
            reportErrorCurlyCallUseBottom(eng, ast, i)
        end 
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
    node = makeCurlyCallFlowNode(ast, tts, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferDotCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :dotcall)
    return inferFunCall(eng, ctx, ast, true)
end

function inferFunCall(eng::Engine, ctx::Context, ast::JuAST, isDotcall::Bool)::InferResult
    # This function doesn't handle function definition
    args = ast.args
    fargs = FlowNode[]
    kwargs = Pair{Symbol, FlowNode}[]
    local rel::InferResult
    # TODO : we should add FlowNode for keyword assignment???
    for i in 1:length(args)
        # parameters can only occur at last position
        local iast = args[i]
        if iast.head == :parameters
            if i != length(args)
                reportASTError(eng, iast, "Keywords must be the last argument!")
            else
                for index in eachindex(iast.args)
                    local i = iast.args[index]
                    # implict keywords
                    if i.head == :identifier
                        rel = inferVar(eng, ctx, i)
                        sym = cast2Symbol(i.val)
                    elseif i.head == :(=)
                        if length(i.args) == 2
                            if i.args[1].head == :identifier
                                sym = cast2Symbol(i.args[1].val)
                                rel = inferExpr(eng, ctx, i.args[2])
                            else
                                reportASTError(eng, iast, "Invalid keywords assignment : disallow destruction")
                            end
                        else
                            reportASTError(eng, iast, "Invalid keywords assignment")
                        end
                    else
                        reportASTError(eng, iast, "Invalid keywords assignment")
                    end
                    ctx = rel.ctx
                    if isBottomType(rel.node.typ)
                        reportErrorKeywordUseBottom(eng, ast, sym)
                    end
                    push!(kwargs, sym=>rel.node)
                end
            end
        elseif iast.head != :kw && iast.head != :(=)
            rel = inferExpr(eng, ctx, iast)
            ctx = rel.ctx
            if isBottomType(rel.node.typ)
                reportErrorFunCallUseBottom(eng, ast, i)
            end
            push!(fargs, rel.node)
        else
            # iast.head == :kw or iast.head == :(=)
            #=
            f(x, y = 1) is the same thing as f(x;y=1)
            Maybe just force the users to use the latter one
            =#
            reportASTError(eng, iast, "Use semi-colon to seperate keyword arguments instead of comma. Write f(x;y=1) instead f(x, y=1)")
        end
    end
    if isDotcall && length(kwargs) > 0
        reportASTError(eng, ast, "Unsupported dotted call with keyword arguments")
    end
    imprecise = Int[]
    kwimprecise = Int[]
    # we only support Julia 1.9, because Julia handles keyword arguments differently across versions
    for i in 1:length(fargs)
        n = fargs[i]
        ttypval = n.typ
        if !isConcreteType(ttypval)
            push!(imprecise, i)
        end
    end
    
    checkConstructor = false
    if isConstructor(fargs[1].typ)
        checkConstructor = true
    end

    isKwCall = false
    if length(kwargs) != 0
        isKwCall = true
        for i in eachindex(kwargs)
            pair = kwargs[i]
            n = pair.second
            ttypval = n.typ
            if !isConcreteType(ttypval)
                push!(kwimprecise, i)
            end
        end
    end
    ms = MethodCallStruct(fargs, kwargs)
    if length(imprecise) > 0 || length(kwimprecise) > 0
        reportErrorFunCallArgs(eng, ast, ms, imprecise, kwimprecise, fargs, kwargs, isDotcall)
    end
    if isDotcall
        # kwargs is empty here
        fargs = vcat(makeLiteralFlowNode(ast, makeConstJuASTVal(Base.broadcasted)), fargs)
        ms = MethodCallStruct(fargs, kwargs)
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            # TODO : report mismatched method table here
            reportErrorFunCall(eng, ast, ms, length(mm), false)
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(tt)
            reportErrorBroadcastBottom(eng, ast, ms)
        end
        bnode = makeFunCallFlowNode(ast, ms, tt)
        fargs = FlowNode[makeLiteralFlowNode(ast, makeConstJuASTVal(Base.materialize)), bnode]
        ms = MethodCallStruct(fargs, kwargs)
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            # TODO : report mismatched method table here
            reportErrorFunCall(eng, ast, ms, length(mm), false)
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(tt)
            reportErrorBroadcastBottom(eng, ast, ms)
        end
    else
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            # TODO : report mismatched method table here
            reportErrorFunCall(eng, ast, ms, length(mm), isDotcall)
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(tt) && checkConstructor
            reportErrorNoConstructor(eng, ast, ms)
        end
    end
    # TODO : we should add constant propagation for pure builtin function !!!
    # this is again a terminal node
    node = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

# TODO : we should terminate after we return (we need to cut the value here)
function inferReturn(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :return)
    if length(ast.args) == 0
        rettnode = makeLiteralFlowNode(ast, makeConstJuASTVal(nothing))
        newnode = makeEmptyReturnFlowNode(ast)
    elseif length(ast.args) == 1
        rel = inferExpr(eng, ctx, ast.args[1])
        rettnode = rel.node
        newnode = makeReturnFlowNode(ast, rettnode)
        ctx = rel.ctx
    else
        reportASTError(eng, ast, "Return should have only zero or one parameter")
    end
    engn = eng.retVal
    if isNone(engn)
        eng.retVal = Just(rettnode)
    else
        engnn = castJust(engn)
        if !tryMergeReturnFlowNode(engnn, rettnode)
            reportErrorReturnEnlargeType(eng, engnn, rettnode)
        end
    end
    addFlowMapping!(eng, ast, newnode)
    # we retain all the variables, because if-else needs to check them
    return InferResult(ctx, newnode)
end

function inferExpr(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    if ast.head == :(=)
        return inferAssign(eng, ctx, ast)
    elseif ast.head == :literal
        return inferLiteral(eng, ctx, ast)
    elseif ast.head == :identifier
        return inferVar(eng, ctx, ast)
    elseif ast.head == :block
        return inferBlock(eng, ctx, ast)
    elseif ast.head == :tuple
        return inferTupleValue(eng, ctx, ast)
    elseif ast.head == :char
        return inferCharLiteral(eng, ctx, ast)
    elseif ast.head == :quote
        return inferQuoteLiteral(eng, ctx, ast)
    elseif ast.head == :macrocall
        return inferMacroCall(eng, ctx, ast)
    elseif ast.head == :curly
        return inferCurlyCall(eng, ctx, ast)
    elseif ast.head == :call
        return inferFunCall(eng, ctx, ast, false)
    elseif ast.head == :dotcall
        return inferFunCall(eng, ctx, ast, true)
    elseif ast.head == :return
        return inferReturn(eng, ctx, ast)
    elseif ast.head == :break
        return inferBreakStmt(eng, ctx, ast)
    elseif ast.head == :continue
        return inferContinueStmt(eng, ctx, ast)
    elseif ast.head == :(.)
        return inferGetField(eng, ctx, ast)
    elseif ast.head == :ref
        return inferArrayRef(eng, ctx, ast)
    elseif ast.head == :if || ast.head == :? || ast.head == :(&&) || ast.head == :(||)
        return inferIfStmt(eng, ctx, ast, ast.head == :(||), ast.head == :(&&))
    elseif ast.head == :string
        return inferStringValue(eng, ctx, ast)
    elseif ast.head == :for
        return inferForStmt(eng, ctx, ast)
    elseif ast.head == :local
        return inferLocal(eng, ctx, ast)
    elseif ast.head == :global
        return inferGlobal(eng, ctx, ast)
    elseif ast.head == :let
        return inferLet(eng, ctx, ast)
    elseif ast.head == :while
        return inferWhileStmt(eng, ctx, ast)
    elseif ast.head == :(<:) || ast.head == :where
        reportUnimplementedASTError(eng, ast, "Unsupported type construction <: or where")
    elseif ast.head == :vect
        reportASTError(eng, ast, "Don't use untyped array construction. Use Type[...] instead of [...]")
    elseif ast.head == :(::)
        reportUnimplementedASTError(eng, ast, "type assertion is unsupported")
    elseif ast.head == :function || ast.head == :(->)
        reportUnimplementedASTError(eng, ast, "Nested function is unsupported")
    elseif ast.head == :comparison
        return inferComparison(eng, ctx, ast)
    else
        str = String(ast.head)
        if last(str) == '='
            return inferAssign(eng, ctx, ast)
        end
        if ast.head in Symbol[:kw, :parameters]
            reportASTError(eng, ast, "Invalid AST construction")
        end
        reportUnimplementedASTError(eng, ast, "Unsupported AST type $(ast.head)")
    end
    #=
    elseif ast.head == :module
        nameast = ast.args[2]
        if ast.args[3].head != :block
            err = InvalidSyntaxError("Invalid module expression")
            reportError(result, err, ast)
            return ex
        end
        if nameast.head == :identifier
            sym = cast2Symbol(nameast.val)
            stmts = Vector{JuExpr}()
            modex = JuExpr(ModDef(sym, stmts), ast)
            addSourceMapDerived!(result, ast.args[1], DerivedExpr(DerivedExprModuleName(modex)))
            addSourceMapDerived!(result, nameast, DerivedExpr(DerivedExprModuleName(modex)))
            addSourceMap!(result, ast, modex)
            # entering module
            push!(result.modules, sym)
            for i in ast.args[3].args
                if i.head == :function
                    ex = constructJuExprFunDef!(result, i)
                elseif i.head == :module
                    ex = constructJuExpr!(result, i)
                else
                    # ignore all other toplevel constructions
                    ex = JuExpr(Literal(makeConstVal(nothing)), i)
                end
                # ignore all invalid expressions here
                if isValidJuExpr(ex)
                    push!(stmts, ex)
                end
            end
            # leaving module
            pop!(result.modules)
            return modex
        else
            err = InvalidSyntaxError("Invalid module definition")
            reportError(result, err, nameast)
        end
        return ex
    elseif ast.head == :(<:) || ast.head == :(&&) || ast.head == :(||)
        # subtyping lowered as function call
        # this is actually incorrect, because <: can also appear in type application
        # like Array{<:Int, 1}, we need to check this case more carefully, currently we ignore compilicated type definition
        # TODO : please check this 
        f = JuExpr(Var(ast.head), ast)
        # TODO : we map the var to the whole ast, which is incorrect
        # what to do here ???
        if length(ast.args) != 2
            err = InvalidSyntaxError("<: in typing context is unsupported")
            reportError(result, err, ast)
            return ex
        end
        stmts = Vector{JuExpr}(undef, length(ast.args))
        for i in 1:length(ast.args)
            stmts[i] = constructJuExpr!(result, ast.args[i])
        end
        ex = JuExpr(FunCall(f, stmts, Pair{Symbol, JuExpr}[]), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :(::)
        lhs = constructJuExpr!(result, ast.args[1])
        rhs = constructJuExpr!(result, ast.args[2])
        ex = JuExpr(TypedAssert(lhs, rhs), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :toplevel
        err = InvalidSyntaxError("Misplaced toplevel block")
        reportError(result, err, ast)
        return ex
    elseif ast.head == :function || ast.head == :(->)
        err = InvalidSyntaxError("Nested function definition is not supported")
        reportError(result, err, ast)
        return ex
    elseif ast.head in Symbol[:const, :struct, :import, :using, :export]
        ex = JuExpr(Literal(makeConstVal(nothing)), ast)
        return ex

        ex = JuExpr(DeclarationList(exs), ast)
        addSourceMap!(result, ast, ex)
        return ex
    =#
end

function inferStringValue(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :string)
    # todo : handle the logic here...DON'T copy everywhere
    args = Vector{FlowNode}(undef, length(ast.args))
    for i in 1:length(args)
        newrel = inferExpr(eng, ctx, ast.args[i])
        ctx = newrel.ctx
        if isBottomType(newrel.node.typ)
            reportErrorStringUseBottom(eng, ast.args[i])
        end
        args[i] = newrel.node
    end
    opnode = makeLiteralFlowNode(ast, makeConstJuASTVal(Base.string))
    ms = MethodCallStruct(opnode, args)
    # TODO : report imprecise method here
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorStringConstructor(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorStringReturnBottom(eng, ast, ms)
    end
    newnode = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferVar(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :identifier)
    id = cast2Symbol(ast.val)
    if hasVar(ctx, id)
        ctxval = lookup(ctx, id)
        node = ctxval.curtyp
        if !mustInitialized(node)
            reportErrorUnitializedVar(eng, ast, id)
        end
        node = makeVarFlowNode(ast, ctxval.curtyp)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif isModuleDefined(eng.mod, id)
        val = getFromModule(eng.mod, id)
        # fix this, using a module here
        node = makeGlobalVarFlowNode(ast, val)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif id == :&& || id == :||
        if id == :&&
            val = makeConstVal(&)
        else
            val = makeConstVal(|)
        end
        node = makeGlobalVarFlowNode(ast, val)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif id == :end
        # lastindex
        # we only support one-dimentional array!!!
        # this is incorrect
        node = eng.arrayContext[end]
        ms = MethodCallStruct(makeLiteralFlowNode(ast, makeConstJuASTVal(Base.lastindex)), FlowNode[node])
        m = getMethodMatches(eng, ms)
        # TODO : check method !!!
        tt = extractUniqueMatch(m)
        node = makeFunCallFlowNode(ast, ms, tt)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif id == :begin
        # firstindex
        node = eng.arrayContext[end]
        ms = MethodCallStruct(makeLiteralFlowNode(ast, makeConstJuASTVal(Base.firstindex)), FlowNode[node])
        m = getMethodMatches(eng, ms)
        # TODO : check method !!!
        tt = extractUniqueMatch(m)
        # TODO : check node here
        node = makeFunCallFlowNode(ast, ms, tt)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    else
        reportErrorUndefinedVar(eng, ast, eng.mod, id)
    end
end

function inferGlobal(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    return InferResult(ctx, makeLiteralFlowNode(ast, makeConstJuASTVal(nothing)))
end

function inferLocal(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    for i in ast.args
        if i.head == :(=)
            ctx = inferAssign(eng, ctx, i).ctx
        else
            if i.head == :(::)
                ctx = inferAssignLHSTypedAssert(eng, ctx, i, None(FlowNode)).ctx
            elseif i.head == :identifier
                sym = cast2Symbol(i.val)
                if hasVar(ctx, sym)
                    error("Internal error : a variable shouldn't be declared here")
                end
                node = makeAssignLHSLocalVarFlowNode(ast)
                ctx = update(ctx, sym, ContextValue(node, node))
            else
                reportASTError(eng, i, "Invalid local declaration, must be `local x`` or `local x, y = 1, z::Int`")
            end
        end
    end
    return InferResult(ctx, makeLiteralFlowNode(ast, makeConstJuASTVal(nothing)))
end

function inferAssignUpdateHelper(eng::Engine, ctx::Context, ast::JuAST, lhsnode::FlowNode, rhsnode::FlowNode, op::UpdateOp)::FlowNode
    if isModuleDefined(eng.mod, op.op)
        opval = getFromModule(eng.mod, op.op)
        # fix this, using a module here
        opnode = makeGlobalVarFlowNode(op.ast, opval)
        addFlowMapping!(eng, op.ast, opnode)
        argnodes = FlowNode[opnode, lhsnode, rhsnode]
        ms = MethodCallStruct(argnodes)
        mm = getMethodMatches(eng, ms)
        if length(mm) != 1
            reportErrorFunCall(eng, ast, ms, length(mm))
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(tt)
            reportErrorUpdateAssignReturnBottom(eng, ast)
        end
        rhsnode = makeFunCallFlowNode(ast, ms, tt)
        addFlowMapping!(eng, op.ast, rhsnode)
        return rhsnode
    else
        throw(InternalError("All the update assignment operators should be defined in module $(eng.mod)"))
        
    end
end

function inferAssignLHSVarBroadcast(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    # x .+= 1
    rel = inferVar(eng, ctx, ast)
    ctx = rel.ctx
    lhsnode = rel.node
    # lhsnode shouldn't be a bottom
    if isBottomType(lhsnode.typ)
        throw(InternalError("Variable should never be inferred as bottom type!"))
    end
    return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
end

function inferAssignLHSBroadcastHelper(eng::Engine, ctx::Context, ast::JuAST, lhsnode::FlowNode, rhsnode::FlowNode, op::UpdateOp)::InferResult
    if op.isUpdate
        if isModuleDefined(eng.mod, op.op)
            opval = getFromModule(eng.mod, op.op)
            # fix this, using a module here
            opnode = makeGlobalVarFlowNode(op.ast, opval)
            addFlowMapping!(eng, op.ast, opnode)
            fargs = FlowNode[makeLiteralFlowNode(ast, makeConstJuASTVal(Base.broadcasted)), opnode, lhsnode, rhsnode]
        else
            throw(InternalError("All the update assignment operators should be defined in module $(eng.mod)"))
        end
    else
        opnode = makeGlobalVarFlowNode(ast, makeConstVal(Base.identity))
        fargs = FlowNode[makeLiteralFlowNode(ast, makeConstJuASTVal(Base.broadcasted)), opnode, rhsnode]
    end
    ms = MethodCallStruct(fargs)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        # TODO : report mismatched method table here
        reportErrorFunCall(eng, ast, ms, length(mm), false)
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorBroadcastBottom(eng, ast, ms)
    end
    bnode = makeFunCallFlowNode(ast, ms, tt)
    fargs = FlowNode[makeLiteralFlowNode(ast, makeConstJuASTVal(Base.materialize!)), lhsnode, bnode]
    ms = MethodCallStruct(fargs)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        # TODO : report mismatched method table here
        reportErrorFunCall(eng, ast, ms, length(mm), false)
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorBroadcastBottom(eng, ast, ms)
    end
    node = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferAssignLHSVar(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    assertASTKind(ast, :identifier)
    if op.isDotcall
        return inferAssignLHSVarBroadcast(eng, ctx, ast, rhsnode, op)
    end
    varid = cast2Symbol(ast.val)
    # rhsnode is not a bottom, ensured by caller of inferLHSSingpleVar
    if op.isUpdate
        rel = inferVar(eng, ctx, ast)
        ctx = rel.ctx
        lhsnode = rel.node
        # lhsnode shouldn't be a bottom
        if isBottomType(lhsnode.typ)
            throw(InternalError("Variable should never be inferred as bottom type!"))
        end
        if isModuleDefined(eng.mod, op.op)
            opval = getFromModule(eng.mod, op.op)
            # fix this, using a module here
            opnode = makeGlobalVarFlowNode(op.ast, opval)
            addFlowMapping!(eng, op.ast, opnode)
            argnodes = FlowNode[opnode, lhsnode, rhsnode]
            ms = MethodCallStruct(argnodes)
            mm = getMethodMatches(eng, ms)
            if length(mm) != 1
                reportErrorFunCall(eng, ast, ms, length(mm))
            end
            tt = extractUniqueMatch(mm)
            if isBottomType(tt)
                reportErrorUpdateAssignReturnBottom(eng, ast)
            end
            rhsnode = makeFunCallFlowNode(ast, ms, tt)
            addFlowMapping!(eng, op.ast, rhsnode)
        else
            throw(InternalError("All the update assignment operators should be defined in module $(eng.mod)"))
        end
    end

    # TODO : fix assignment here
    if hasVar(ctx, varid)
        # this variable is already assigned before, we check type compatibility
        oldval = lookup(ctx, varid)
        if !isDeclarationFlowNode(oldval.curtyp)
            # storage type is unchanged, update current type 
            if !tryMergeFlowType(oldval.typ, rhsnode)
                reportErrorAssignIncompatible(eng, oldval.typ, rhsnode)
            end
            newnode = makeAssignLHSVarFlowNode(ast, rhsnode, op)
            # the primary assignment is unchanged
            val = ContextValue(oldval.typ, newnode)
        else
            # the variable is only declared with `local x`
            # so it has no storage type
            newnode = makeAssignLHSVarFlowNode(ast, rhsnode, op)
            val = ContextValue(newnode, newnode)
        end
    else
        # the variable is not assigned before
        newnode = makeAssignLHSVarFlowNode(ast, rhsnode, op)
        val = ContextValue(newnode, newnode)    
    end
    # TODO : this ast assigned is incorrect !
    # we lacks the assign to the whole assignment expression
    ctx = update(ctx, varid, val)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

#=
function inferGenerator(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :generator)
    # we disallow generator modifying variable from outer scope
    body = ast.args[1]
    info = eng.scopeInfos[ast]
    if !isempty(info.modified)
        reportASTError(eng, ast, "Generator should not modify variables from outer scope")
    end
    # shadow variable
    # since we don't modify any variable, there is no need to update ctx
    newmapping = Dict{Symbol, ContextValue}()
    scopeInfo = eng.scopeInfos[ast]
    for pair in ctx.mapping.data
        k, v = pair
        if !hasVar(scopeInfo, k)
            newmapping[k] = v
        end
    end
    newctx = ContextValue(ImmutableDict(newmapping)())
    if length(ast.args) > 2
        reportASTError(eng, ast, "Only support one variable in generator expression")
    end
    rhs = ast.args[2]
    if rhs.head == :filter
        reportASTError(eng, ast, "Filter not supported")
    end
    elseif rhs.head == :(=)
    rel = inferExpr(eng, ast, newctx)
    newctx = rel.ctx
    varnode = iterateCheck(eng, rel.node)
    newctx = inferAssignLHS(eng, newctx, assign, varnode, UpdateOp()).ctx
    
end
=#

function inferAssignLHSSetField(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    # TODO : getproperty and setproperty on module...
    assertASTKind(ast, :(.))
    if length(ast.args) != 2
        reportASTError(eng, ast, "Empty setproperty")
    end 
    fieldast = ast.args[2]
    if fieldast.head == :quote && fieldast.args[1].head == :identifier
        p = cast2Symbol(fieldast.args[1].val)
    else
        reportASTError(eng, ast, "Not a valid field")
    end
    refast = ast.args[1]
    rel = inferExpr(eng, ctx, ast.args[1])
    ctx = rel.ctx
    xnode = rel.node
    # we need to distinguish getproperty on module and other types...
    if isBottomType(xnode.typ)
        reportErrorSetFieldOfBottom(eng, ast)
    end
    if !isConcreteType(xnode.typ)
        reportErrorFieldType(eng, ast, xnode, false)
    end
    if !hasField(xnode.typ, p)
        reportErrorNoField(eng, ast, xnode, p)
    end
    
    # here, it should be dotgetproperty for broadcast call
    # or getproperty for other non-broadcast call
    ft = getFieldType(xnode.typ, p)
    if !isConcreteType(ft)
        if op.isDotcall || op.isUpdate
            reportErrorFieldType(eng, ast, xnode, false)
        end
    end
    lhsnode = makeGetPropertyFlowNode(refast, xnode, ft)
    
    if op.isDotcall
        return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
    end

    if op.isUpdate
        if isModuleDefined(eng.mod, op.op)
            # TODO : add correct arguments here!!!
            opval = getFromModule(eng.mod, op.op)
            # fix this, using a module here
            opnode = makeGlobalVarFlowNode(op.ast, opval)
            addFlowMapping!(eng, op.ast, opnode)
        else
            throw(InternalError("All the update assignment operators should be defined in module $(eng.mod)"))
        end
        argnodes = FlowNode[opnode, lhsnode, rhsnode]
        ms = MethodCallStruct(argnodes)
        mm = getMethodMatches(eng, ms)
        if length(mm) != 1
            reportErrorFunCall(eng, ast, ms, length(mm))
        end
        rt = extractUniqueMatch(mm)
        if isBottomType(rt)
            reportErrorUpdateAssignReturnBottom(eng, ast)
        end
        rhsnode = makeFunCallFlowNode(ast, ms, rt)
        addFlowMapping!(eng, op.ast, rhsnode)
    end
    
    opnode = makeLiteralFlowNode(ast, makeConstJuASTVal(Base.setproperty!))
    # TODO : this is incorrect...
    # ms should be setproperty!(x, v, i)
    ms = MethodCallStruct(opnode, FlowNode[xnode, rhsnode])
    if !checkFieldCompatibility(ft, rhsnode.typ)
        reportErrorSetFieldTypeIncompatible(eng, ast, xnode, rhsnode, ft, p)
    end
    newnode = makeSetPropertyFlowNode(ast, xnode, rhsnode, rhsnode.typ)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferAssignLHSTypedAssert(eng::Engine, ctx::Context, ast::JuAST, mrhsnode::Maybe{FlowNode})::InferResult
    assertASTKind(ast, :(::))
    lhs = ast.args[1]
    # we only support x::Int = 1 for single variable
    # notice that this is not a scope declaration, but we check it in the scope checker
    rel = inferExpr(eng, ctx, ast.args[2])
    ctx = rel.ctx
    if lhs.head == :identifier
        var = cast2Symbol(lhs.val)
        typ = rel.node.typ
        if !isConstVal(typ)
            reportErrorTypeAssertNonconst(eng, ast)
        else
            declaretyp = lift(typ)
            if isNone(mrhsnode)
                # TODO : distinguish bewteen these two kinds of node!!!
                node1 = makeAssignLHSVarAssertFlowNode(ast, rel.node, declaretyp, false)
                ctx = update(ctx, var, ContextValue(node1, node1))
                node = makeLiteralFlowNode(ast, makeConstJuASTVal(nothing))
                return InferResult(ctx, node)
            else
                rhsnode = castJust(mrhsnode)
                if !tryMergeCompileValue(declaretyp, rhsnode.typ)
                    reportErrorAssignInitIncompatible(eng, ast, declaretyp, rhsnode.typ)
                end
                node1 = makeAssignLHSVarAssertFlowNode(ast, rhsnode, declaretyp, true)
                ctx = update(ctx, var, ContextValue(node1, node1))
                node = makeAssignExprFlowNode(ast, rhsnode)
                return InferResult(ctx, node)
            end
        end
    else
        reportASTError(eng, ast, "Typed assert can only declared on a single variable")
    end
end

function inferAssignLHS(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    if ast.head == :tuple
        if op.isUpdate
            reportASTError(eng, ast, "update assignment can't be used while lhs is a tuple destruct")
        elseif op.isDotcall
            reportASTError(eng, ast, "Don't use broadcast to assign a tuple on lhs")
        end
        return inferAssignLHSTupleDestruct(eng, ctx, ast, rhsnode)
    elseif ast.head == :identifier
        return inferAssignLHSVar(eng, ctx, ast, rhsnode, op)
    elseif ast.head == :(.)
        return inferAssignLHSSetField(eng, ctx, ast, rhsnode, op)
    elseif ast.head == :ref
        return inferAssignLHSArrayRef(eng, ctx, ast, rhsnode, op)
    elseif ast.head == :(::)
        if op.isUpdate
            reportASTError(eng, ast, "update assignment can't be used while lhs is a typed assert")
        elseif op.isDotcall
            reportASTError(eng, ast, "Don't use broadcast to assign a variable with tyed assert")
        end
        return inferAssignLHSTypedAssert(eng, ctx, ast, Just(rhsnode))
    else
        reportASTError(eng, ast, "LHS of assignment must be a variable, tuple, property or arrayref")
    end
end

@nocheck function tryDestructTuple(eng::Engine, ctx::Context, ast::JuAST, node::FlowNode)::Union{Nothing, Vector{FlowNode}}
    t = node.typ.typ
    if t isa DataType 
        if length(t.parameters) >= length(ast.args)
            if ((t <: Tuple) || (t <: Pair))
                nodes = FlowNode[makeAssignLHSTupleDestructFlowNode(ast.args[i], node, makeType(t.parameters[i])) for i in 1:length(ast.args)]
                if length(t.parameters) > length(ast.args)
                    # TODO : add warning here to disallow such behaviour!!!
                end
                return nodes
            else
                reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, not a pair or tuple. Got a $(toString(node.typ))")
            end
        else
            reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, length mismatched")
        end
    else
        reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, not a pair or tuple. Got a $(toString(node.typ))")
    end
end

function inferAssignLHSArrayRef(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    assertASTKind(ast, :ref)
    if length(ast.args) == 0
        reportASTError(eng, ast, "Empty ref")
    end 
    args = similar(ast.args, FlowNode)
    for i in 1:length(args)
        if i == 2
            push!(eng.arrayContext, args[1])
        end
        newrel = inferExpr(eng, ctx, ast.args[i])
        ctx = newrel.ctx
        args[i] = newrel.node
        if isBottomType(newrel.node.typ)
            reportErrorArrayRefBottom(eng, ast, i, i == lastindex(args))
        end
        if i >= 2 && i == lastindex(args)
            pop!(eng.arrayContext)
        end
    end
    if op.isUpdate || op.isDotcall
        # TODO : extract this to a helper function
        if op.isDotcall
            opnode = makeLiteralFlowNode(ast, makeConstJuASTVal(Base.dotview))
        else
            opnode = makeLiteralFlowNode(ast, makeConstJuASTVal(Base.getindex))
        end
        ms = MethodCallStruct(opnode, args)
        # TODO : report imprecise method here
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            reportErrorArrayRef(eng, ast, ms, length(mm))
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(tt)
            reportErrorIndexReturnBottom(eng, ast, ms)
        end
        lhsnode = makeArrayRefFlowNode(ast, ms, tt)
        addFlowMapping!(eng, ast, lhsnode)
        if op.isDotcall
            return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
        else
            rhsnode = inferAssignUpdateHelper(eng, ctx, ast, lhsnode, rhsnode, op)
        end
    end
    insert!(args, 2, rhsnode)
    opnode = makeLiteralFlowNode(ast, makeConstJuASTVal(Base.setindex!))
    ms = MethodCallStruct(opnode, args)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArraySet(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorIndexReturnBottom(eng, ast, ms)
    end
    newnode = makeAssignLHSArrayRefFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    InferResult(ctx, newnode)
end

function inferAssignLHSTupleDestruct(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode)::InferResult
    assertASTKind(ast, :tuple)
    tts = tryDestructTuple(eng, ctx, ast, rhsnode)
    for i in eachindex(ast.args)
        ctx = inferAssignLHS(eng, ctx, ast.args[i], tts[i], UpdateOp()).ctx
    end
    newnode = makeAssignLHSTupleDestructFlowNode(ast, rhsnode, rhsnode.typ)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferAssign(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    opstr = String(ast.head)
    isValid = false
    isDotcall = (ast.flag & 0x0002) != 0
    op = UpdateOp(ast, isDotcall)
    # JuliaSyntax.DOTOP_FLAG == 0x0002
    isUpdate = false
    if length(opstr) >= 1
        opchar = opstr[end]
        if opchar == '='
            isValid = true
            if length(opstr) > 1
                op = UpdateOp(ast, Symbol(opstr[1:end-1]), isDotcall)
            end
        end
    end
    if !isValid
        reportASTError(eng, ast, "Not a valid assignment $opstr")
    end
    if length(ast.args) != 2
        reportASTError(eng, ast, "Assignment expression should only have 2 children, $(length(ast.args)) found")
    end
    if ast.args[1].head == :call
        reportASTError(eng, ast, "Shorthand function definition not supported")
    end
    rel = inferExpr(eng, ctx, ast.args[2])
    ctx = rel.ctx
    rhsnode = rel.node
    if isBottomType(rhsnode.typ)
        # error(rhs) is a bottom
        reportErrorAssignBottom(eng, ast)
    end
    # logic to dispatch lhs
    lhs = ast.args[1]
    rel = inferAssignLHS(eng, ctx, lhs, rhsnode, op)
    if !op.isUpdate && !op.isDotcall
        # the value of the whole assignment expression is the value of RHS
        anode = makeAssignExprFlowNode(ast, rhsnode)
        addFlowMapping!(eng, ast, anode)
        return InferResult(rel.ctx, anode)
    end
    return rel
end

function inferBlock(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :block)
    if length(ast.args) == 0
        newnode = makeEmptyBlockFlowNode(ast)
        return InferResult(ctx, newnode)
    else
        for i in 1:length(ast.args)-1
            ret = inferExpr(eng, ctx, ast.args[i])
            ctx = ret.ctx
            if isBottomType(ret.node.typ)
                reportErrorPrematureUnreachable(eng, ast.args[i])
            end
        end
    end
    ret = inferExpr(eng, ctx, last(ast.args))
    newnode = makeBlockFlowNode(ast, ret.node)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ret.ctx, newnode)
end

function inferLet(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :let)
    vars = ast.args[1]
    newmapping = Dict{Symbol, ContextValue}()
    shadowmapping = Dict{Symbol, ContextValue}()
    info = eng.scopeInfos[ast]
    for pair in ctx.mapping.data
        name = pair.first
        value = pair.second
        if hasVar(info, name)
            shadowmapping[name] = value
        else
            newmapping[name] = value
        end
    end
    oldctx = ctx
    ctx = Context(ImmutableDict(newmapping))
    for i in vars.args
        if i.head == :(=)
            ctx = inferExpr(eng, ctx, i).ctx
        end
    end
    rel = inferExpr(eng, ctx, ast.args[2])
    node = rel.node
    ctx = rel.ctx
    finalmapping = Dict{Symbol, ContextValue}()
    for pair in oldctx.mapping.data
        name = pair.first
        value = pair.second
        if hasVar(info, name)
            finalmapping[name] = shadowmapping[name]
        else
            finalmapping[name] = newmapping[name]
        end
    end
    ctx = Context(ImmutableDict(finalmapping))
    node = makeLetFlowNode(ast, node)
    return InferResult(ctx, node)
end

function inferTupleValue(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :tuple)
    nodes = Vector{FlowNode}(undef, length(ast.args))
    for i in eachindex(ast.args)
        local iast = ast.args[i]
        rel = inferExpr(eng, ctx, iast)
        ctx = rel.ctx
        if isBottomType(rel.node.typ)
            reportErrorTupleBottom(eng, iast, i)
        end
        nodes[i] = rel.node
    end
    node = makeTupleFlowNode(ast, nodes, makeTupleType(nodes))
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node) 
end

function inferBreakStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :break)
    # a forward edge that requires join
    push!(eng.loopContext[end], ctx)
    InferResult(ctx, makeBreakStmtNode(ast))
end

function inferContinueStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :continue)
    InferResult(ctx, makeContinueStmtNode(ast))
end

# TODO : perform constant propagation for tuple type and pair here...
# actually, we need a general way to perform constant and type propagation here
# maybe some kind of dependent type ???


# TODO : infer end here
# we need to store an array context chain here, which is not hard, but just remember to do so !!!

function inferArrayRef(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :ref)
    if length(ast.args) == 0
        reportASTError(eng, ast, "Indexing operation is empty")
    end
    args = Vector{FlowNode}(undef, length(ast.args))
    for i in 1:length(args)
        if i == 2
            push!(eng.arrayContext, args[1])
        end
        newrel = inferExpr(eng, ctx, ast.args[i])
        ctx = newrel.ctx
        if isBottomType(newrel.node.typ)
            reportErrorArrayRefBottom(eng, ast, i, false)
        end
        args[i] = newrel.node
        if i >= 2 && i == lastindex(args)
            pop!(eng.arrayContext)
        end
    end
    opnode = makeLiteralFlowNode(ast, makeConstJuASTVal(Base.getindex))
    ms = MethodCallStruct(opnode, args)
    # TODO : report imprecise method here
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArrayRef(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(tt)
        reportErrorIndexReturnBottom(eng, ast, ms)
    end
    newnode = makeArrayRefFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferGetField(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :(.))
    if length(ast.args) != 2
        reportASTError(eng, ast, "Not a valid getproperty call")
    end
    ap = ast.args[2]
    if ap.head == :quote && length(ap.args) == 1 && ap.args[1].head == :identifier
        p = cast2Symbol(ap.args[1].val)
    else
        reportASTError(eng, ast, "Property is not a constant symbol")
    end
    rel = inferExpr(eng, ctx, ast.args[1])
    node = rel.node
    ctx = rel.ctx
    ft = node.typ
    # we need to distinguish getproperty on module and other types...
    if isBottomType(ft)
        reportErrorGetFieldOfBottom(eng, ast)
    end
    if isConstVal(ft)
        vval = ft.val
        if vval isa Module
            if isModuleDefined(vval, p)
                v = getFromModule(vval, p)
                node = makeGetPropertyFlowNode(ast, node, v)
                addFlowMapping!(eng, ast, node)
                return InferResult(ctx, node)
            else
                reportErrorNoField(eng, ast, node, p)
            end
        end
    end
    if !isConcreteType(ft)
        reportErrorFieldType(eng, ast, node, false)
    end
    if hasField(ft, p)
        tt = getFieldType(ft, p)
        newnode = makeGetPropertyFlowNode(ast, node, tt)
        addFlowMapping!(eng, ast, newnode)
        return InferResult(ctx, newnode)
    else
        reportErrorNoField(eng, ast, node, p)
    end
end

function inferComparison(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    # TODO : the logic is incorrect somehow
    #=
    assertASTKind(ast, :comparison)
    args = ast.args
    if length(args) % 2 != 0
        reportASTError(eng, ast, "Chained comparison is invalid")
    end
    ops = FlowNode[]
    for i in 1:div(length(args) + 1, 2)
        rel = inferExpr(eng, ctx, args[2*i - 1])
        ctx = rel.ctx
        push!(ops, rel.node)
    end
    =#
    ms = MethodCallStruct(FlowNode[])
    return InferResult(ctx, makeFunCallFlowNode(ast, ms, makeType(Bool)))
end

function tryNarrowType(eng::Engine, ctx::Context, ast::JuAST)::Pair{InferResult, Context}
    if ast.head == :call
        args = ast.args
        if length(ast.args) == 3
            isa_ = ast.args[1]
            lhs = ast.args[2]
            rhs = ast.args[3]
            if isa_.head == :identifier && cast2Symbol(isa_.val) == :isa
                # TODO : we need to add FlowNode to all the JuExpr here
                isanode = makeGlobalVarFlowNode(isa_, makeConstVal(isa))
                addFlowMapping!(eng, isa_, isanode)
                if lhs.head == :identifier
                    lhsid = cast2Symbol(lhs.val)
                    rel = inferExpr(eng, ctx, rhs)
                    ctx = rel.ctx
                    rhsnode = rel.node
                    rhstt = rhsnode.typ
                    if !isConstVal(rhstt)
                        reportErrorIsaRHSNonconst(eng, ast)
                    end
                    rhstt = lift(rhstt)
                    # TODO : lift the constant here
                    # x isa XXX type, we life XXX to a type before we perform comparsion
                    if !isaType(rhstt)
                        reportErrorIsaLHSBadType(eng, ast, rhstt)
                    end
                    # then we lookup lhs of isa
                    rel = inferVar(eng, ctx, lhs)
                    ctx = rel.ctx
                    xnode = rel.node
                    # rhs must be a subtype of lhs
                    if !tryMergeCompileValue(xnode.typ, rhstt)
                        reportErrorMismatchedSplit(eng, ast, xnode, rhsnode)
                    end
                    ms = MethodCallStruct(FlowNode[isanode, xnode, rhsnode])
                    condnode = makeFunCallFlowNode(ast, ms, makeType(Bool))
                    addFlowMapping!(eng, ast, condnode)
                    # must hasVar here
                    ctxval = lookup(ctx, lhsid)
                    curtt = ctxval.curtyp.typ
                    # storage type must be typ instead of current type
                    storetypenode = ctxval.typ
        
                    pinode = makePiFlowNode(ast, condnode, rhstt)
                    # narrow type to rhs
                    ctxval = ContextValue(storetypenode, pinode)
                    newctx = update(ctx, lhsid, ctxval)
                    newrel = InferResult(newctx, condnode)
                    if isaAny(curtt)
                        return newrel => ctx
                    elseif isaUnion(curtt)
                        negtt = splitUnion(curtt, rhstt)
                        negpinode = makeNegPiFlowNode(ast, condnode, negtt)
                        negctx = update(ctx, lhsid, ContextValue(storetypenode, negpinode))
                        return newrel => negctx
                    else
                        reportErrorIsaLHSBadType(eng, ast, rhstt)
                    end
                else
                    reportErrorIsaBadForm(eng, ast, "`isa` is not in a predicative form, lhs of type splitting must be a single variable")
                end
            end
        end
    end
    rel = inferExpr(eng, ctx, ast)
    if !isaBool(rel.node.typ)
        reportErrorCondNotBool(eng, rel.node)
    end
    return rel => rel.ctx
end

function collectIfBranches!(eng::Engine, rel::Vector{Pair{JuAST, JuAST}}, ast::JuAST)::Maybe{JuAST}
    local ex::Maybe{JuAST}
    if length(ast.args) == 2 || length(ast.args) == 3
        cond = ast.args[1]
        body = ast.args[2]
        push!(rel, cond => body)
        if length(ast.args) == 3
            eb = ast.args[3]
            if eb.head == :elseif
                ex = collectIfBranches!(eng, rel, ast.args[3])
            else
                # else branch
                ex = Just(ast.args[3])
            end
        else
            # empty if clause
            ex = None(JuAST)
        end
    else
        reportASTError(eng, ast, "If statement should have at least 2 children")
    end
    return ex
end

function collectIfBranches(eng::Engine, ast::JuAST)
    rel = Pair{JuAST, JuAST}[]
    rel => collectIfBranches!(eng, rel, ast)
end

function inferWhileStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :while)
    #=
    inference of while loop is similar to for loop, except that 
    What's the scope rule of while statement?
    cond in while belongs to parent scope, while body belongs to child scope
        we disallow variable declared in cond...
    while (some assignment ; cond)
        body
    end
    =#
    scopeinfo = eng.scopeInfos[ast]
    cond = ast.args[1]
    body = ast.args[2]
    condctx = inferExpr(eng, ctx, cond).ctx
    # step 1 : mark all modified variables as non-constant and perform constant propagating...
    modified = Dict{Symbol, ContextValue}()
    # variable newly introduced in condition branch 

    # TODO : maybe fix this by supporting assignment in while loop
    for pair in condctx.mapping.data
        i = pair.first
        v = pair.second
        if !hasVar(ctx, i)
            reportASTError(eng, ast, "Assignment in while-loop condition is bad!")
        elseif haskey(scopeinfo.modified, i)
            modified[i] = v
        end
    end

    newmapping = Dict{Symbol, ContextValue}()
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(modified, k)
            # TODO : mark this as while update node
            z = makeForUpdateFlowNode(ast, v.curtyp)
            newmapping[k] = ContextValue(v.typ, z)
        elseif hasVar(scopeinfo, k)
            # this variable is shadowed in this scope
        else
            # unchanged
            newmapping[k] = v
        end
    end
    
    newctx = Context(ImmutableDict(newmapping))
    rel = inferExpr(eng, newctx, cond)
    if !isaBool(rel.node.typ)
        reportErrorCondNotBool(eng, rel.node)
    end
    # we enter loop when we infer body
    enterLoop(eng)
    newctx = rel.ctx
    newctx = inferExpr(eng, newctx, body).ctx
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(modified, k)
            ctxval = lookup(ctx, k)
            newctxval = lookup(newctx, k)
            # case 1: the variable is already initialized outside loop
            lctx = eng.loopContext[end]
            allnodes = Vector{FlowNode}(undef, length(lctx) + 2)
            allnodes[1] = ctxval.curtyp
            allnodes[end] = newctxval.curtyp
            for i in eachindex(lctx)
                # must have var
                allnodes[i + 1] = lookup(lctx[i], k).curtyp
            end
            # case 1: the variable is already initialized outside loop
            if mustInitialized(ctxval.curtyp)
                # this is incorrect, because break can include additional flownode!
                # other words, we also need to merge the variable definition before a break statement
                newmapping2[k] = ContextValue(ctxval.typ, tryMergeFlowNode(eng, ast, allnodes, true))
            else
                # variable is assigned at least once (it's modified), so it's impossible that the variable is mustUnIntialized?
                local storetyp::FlowNode = allnodes[1]
                isSet = false
                if !isDeclarationFlowNode(ctxval.curtyp)
                    isSet = true
                    storetyp = ctxval.typ
                end
                if !isDeclarationFlowNode(newctxval.curtyp)
                    isSet = true
                    storetyp = newctxval.typ
                end
                for i in eachindex(lctx)
                    # get the storage type of this variable
                    if !isDeclarationFlowNode(allnodes[i + 1])
                        storetyp = lookup(lctx[i], k).typ
                        isSet = true
                        break
                    end
                end
                node = makeConditionalFlowNode(ast)
                if !isSet
                    throw(InternalError("Union of Loop is incorrect, report this to SimpleTypeChecker's developer"))
                else
                    newmapping2[k] = ContextValue(storetyp, node)
                end
            end
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end
    leaveLoop(eng)
    retnode = makeWhileStmtFlowNode(ast)
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end


function inferIfStmt(eng::Engine, ctx::Context, ast::JuAST, isOr::Bool, isAnd::Bool)::InferResult
    # no pi projection
    rels = Vector{InferResult}()
    prectx = ctx
    if isOr
        #=
        x || y isa lowered to
        if x
            true
        else
            y
        end
        =#
        cond = ast.args[1]
        else_bast= ast.args[2]
        # we swap the context here
        rel1, ctx = tryNarrowType(eng, ctx, cond)
        push!(rels, InferResult(rel1.ctx, makeLiteralFlowNode(cond, makeConstJuASTVal(true))))
        rel2 = inferExpr(eng, ctx, else_bast)
        push!(rels, rel2)
    elseif isAnd
        #=
         x && y is lowered to 
         if x 
            y
         else
            false
         end
        =#
        cond = ast.args[1]
        b1 = ast.args[2]
        # we swap the context here
        rel1, ctx = tryNarrowType(eng, ctx, cond)
        rel2 = inferExpr(eng, rel1.ctx, b1)
        push!(rels, InferResult(ctx, makeLiteralFlowNode(cond, makeConstJuASTVal(false))))
        push!(rels, rel2)
    else
        (branches, el) = collectIfBranches(eng, ast)
        for branch in branches
            cond = branch.first
            body = branch.second
            rel1, ctx = tryNarrowType(eng, ctx, cond)
            rel2 = inferExpr(eng, rel1.ctx, body)
            push!(rels, rel2)
        end
        if isNone(el)
            node = makeEmptyElseFlowNode(ast)
            # use the context of negative branch
            push!(rels, InferResult(ctx, node))
        else
            push!(rels, inferExpr(eng, ctx, castJust(el)))
        end
    end
    # firstly, we detect which branch is reachable
    reachflags = similar(rels, Bool)
    fill!(reachflags, true)
    for i in eachindex(reachflags)
        if isBottomType(rels[i].node.typ)
            reachflags[i] = false
        end
    end
    
    newnames = Set{Symbol}()
    for rel in rels
        for i in keys(rel.ctx.mapping.data)
            push!(newnames, i)
        end
    end

    allVars = Set{Symbol}()
    for rel in rels
        for i in getAllVars(rel.ctx)
            push!(allVars, i)
        end
    end
    newctxmap = Dict{Symbol, ContextValue}()
    for var in allVars
        typs = FlowNode[]
        condDef = false
        # Case 1 : variable declared before if
        if hasVar(prectx, var)
            ctxval = lookup(prectx, var)
            prenode = ctxval.curtyp
            if !isDeclarationFlowNode(prenode)
                allUninit = false
                # case 1.1 : variable defined before if, but not initialized
                # local x::Int
                if mustUnInitialized(prenode)
                    allUninit = true
                    # we check whether this variable is assigned at least once in the if-else
                    for i in eachindex(rels)
                        rel = rels[i]
                        local ctxval
                        if reachflags[i]
                            ctxval = lookup(rel.ctx, var)
                            if !mustUnInitialized(ctxval.curtyp)
                                # in at least one reachable branch, the variable is touched
                                allUninit = false
                                break
                            end
                        end
                    end
                end
                if allUninit
                    # this variable is unassigned in the if-else, we retain its value to prevent 
                    newctxmap[var] = ctxval
                    continue
                end
                
                for i in eachindex(rels)
                    rel = rels[i]
                    local ctxval
                    if reachflags[i]
                        ctxval = lookup(rel.ctx, var)
                        if mustInitialized(ctxval.curtyp)
                            push!(typs, ctxval.curtyp)
                        else
                            condDef = true
                            break
                        end
                    end
                end
                if condDef
                    curnode = makeConditionalFlowNode(ast)
                elseif length(typs) == 0
                    # Case 1.2 : variable declared but potentially uninitialized
                    # or none of the branch is reachable for this variable
                    # we use bottom here, because a conditionally flow node should not be accessed
                    curnode = makeConditionalFlowNode(ast)
                else
                    # Case 1.3 : variable declared before if and initialized
                    curnode = tryMergeFlowNode(eng, ast, typs, true)
                end
                newctxmap[var] = ContextValue(ctxval.typ, curnode)
                #@info "At $(formatLocation(ast.loc)) Variable $var is inferred with type $(curnode.typ) sources $([i.typ for i in typs])"
                continue
            end
            # fall through
        end
        storetyps = FlowNode[]
        # Case 2 : variable undefined before if, we merge definition from each branches
        for i in eachindex(rels)
            rel = rels[i]
            # A variable is unconditionally defined
            # iff on every reachable branch, it has an initialized definition
            if hasVar(rel.ctx, var)
                ctxval = lookup(rel.ctx, var)
                if !isDeclarationFlowNode(ctxval.curtyp)
                    push!(storetyps, ctxval.typ)
                    if mustInitialized(ctxval.curtyp)
                        if reachflags[i]
                            push!(typs, ctxval.curtyp)
                        end
                        continue
                    end
                end
            end 
            if reachflags[i]
                condDef = true
            end
        end
        if length(storetyps) == 0
            # the variable is not touched, only declared with `local x`
            # we retain the declaration here
            if hasVar(prectx, var)
                newctxmap[var] = lookup(prectx, var)
            else
                error("Variable declared in if-else but not used")
            end
        end
        # check whether the type is compatible in every branch, even some of them is unreachable
        storenode = tryMergeFlowNode(eng, ast, storetyps, false)

        # TODO : record conditionally defined reason
        if length(typs) != 0
            # we allow union on current type, but not storage type
            if condDef
                node = makeConditionalFlowNode(ast)
            else
                node = tryMergeFlowNode(eng, ast, typs, true)
            end
            # even if a variable is conditionally defined, we need to set its storage type
            value = ContextValue(storenode, node)
            newctxmap[var] = value
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
            # here x has no definition
            print(x)
            =#
            node = makeConditionalFlowNode(ast)
            value = ContextValue(storenode, node)
            newctxmap[var] = value
        end
    end
    newctx = Context(ImmutableDict(Dict{Symbol, ContextValue}(newctxmap)))
    relnodes = FlowNode[]
    # only merge reachable branches
    # indeed, merge unreachable branches won't affect results...
    for i in eachindex(rels)
        if reachflags[i]
            push!(relnodes, rels[i].node)
        end
    end
    if length(relnodes) == 0
        retnode = makeIfStmtFlowNode(ast, makeBottomType())
    else
        # TODO : this is incorrect, perform ast mapping greatly here
        retnode = makeIfStmtFlowNode(ast, tryMergeFlowNode(eng, ast, relnodes, true).typ)
    end
    return InferResult(newctx, retnode)
end


function iterateCheck(eng::Engine, node::FlowNode)::FlowNode
    # TODO : check Union here, all the input should be concrete type
    ast = node.ast
    ms = MethodCallStruct(makeLiteralFlowNode(ast, makeConstJuASTVal(Base.iterate)), FlowNode[node])
    matches = getMethodMatches(eng, ms)
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
    node = makeIterateFlowNode(ast, tt)
    return node
end

function checkForStmt(eng::Engine, ast::JuAST)::Nothing
    if ast.head == :tuple
        for i in ast.args
            checkForStmt(eng, i)
        end
    elseif ast.head == :identifier
        return
    elseif ast.head == :(=)
        checkForStmt(eng, ast.args[1])
    elseif ast.head == :block
        for i in ast.args
            checkForStmt(eng, i)
        end
    else
        reportASTError(eng, ast, "In lhs of for, only variable or tuple destruct are allowed")
    end
end

function inferForStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :for)
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant
    vars = ast.args[1]
    checkForStmt(eng, vars)

    # step one : check outmost iterator
    if vars.head == :block
        firstvar = vars.args[1].args[1]
        iter = vars.args[1].args[2]
    else
        firstvar = vars.args[1]
        iter = vars.args[2]
    end
    rel = inferExpr(eng, ctx, iter)
    ctx = rel.ctx
    varnode = iterateCheck(eng, rel.node)

    # step two : remove shadowed variable
    newmapping = Dict{Symbol, ContextValue}()
    scopeinfo = eng.scopeInfos[ast]

    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(scopeinfo.modified, k)
            # Note : this is incorrect, we should add more check here!!!
            z = makeForUpdateFlowNode(ast, v.curtyp)
            newmapping[k] = ContextValue(v.typ, z)
        elseif hasVar(scopeinfo, k)
            # this variable is shadowed in this scope
        else
            # unchanged
            newmapping[k] = v
        end
    end
    newctx = Context(ImmutableDict(newmapping))
    newctx = inferAssignLHS(eng, newctx, firstvar, varnode, UpdateOp()).ctx
    # step three : add other variables
    if vars.head == :block
        for i in 2:length(vars.args)
            p = vars.args[i]
            firstvar = p.args[1]
            iter = p.args[2]
            rel = inferExpr(eng, newctx, iter)
            varnode = iterateCheck(eng, rel.node)
            newctx = inferAssignLHS(eng, newctx, firstvar, varnode, UpdateOp()).ctx
        end
    end
    body = ast.args[2]

    enterLoop(eng)

    rel2 = inferExpr(eng, newctx, body)
    newctx = rel2.ctx
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(scopeinfo.modified, k)
            ctxval = lookup(ctx, k)
            newctxval = lookup(newctx, k)
            # case 1: the variable is already initialized outside loop
            lctx = eng.loopContext[end]
            allnodes = Vector{FlowNode}(undef, length(lctx) + 2)
            allnodes[1] = ctxval.curtyp
            allnodes[end] = newctxval.curtyp
            for i in eachindex(lctx)
                # must have var
                allnodes[i + 1] = lookup(lctx[i], k).curtyp
            end
            # case 1: the variable is already initialized outside loop
            if mustInitialized(ctxval.curtyp)
                # this is incorrect, because break can include additional flownode!
                # other words, we also need to merge the variable definition before a break statement
                newmapping2[k] = ContextValue(ctxval.typ, tryMergeFlowNode(eng, ast, allnodes, true))
            else
                # variable is assigned at least once (it's modified), so it's impossible that the variable is mustUnIntialized?
                local storetyp::FlowNode = allnodes[1]
                isSet = false
                if !isDeclarationFlowNode(ctxval.curtyp)
                    isSet = true
                    storetyp = ctxval.typ
                end
                if !isDeclarationFlowNode(newctxval.curtyp)
                    isSet = true
                    storetyp = newctxval.typ
                end
                for i in eachindex(lctx)
                    # get the storage type of this variable
                    if !isDeclarationFlowNode(allnodes[i + 1])
                        storetyp = lookup(lctx[i], k).typ
                        isSet = true
                        break
                    end
                end
                node = makeConditionalFlowNode(ast)
                if !isSet
                    throw(InternalError("Union of Loop is incorrect, report this to SimpleTypeChecker's developer"))
                else
                    newmapping2[k] = ContextValue(storetyp, node)
                end
            end
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end

    leaveLoop(eng)
    retnode = makeLiteralFlowNode(ast, makeConstJuASTVal(nothing))
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end

function fasteval(mod::Module, x::JuAST, params::Dict{Symbol, TypeVar})
    return fasteval(mod, x, params, Vector{TypeVar}())
end

@nocheck function fasteval(mod::Module, x::JuAST, params::Dict{Symbol, TypeVar}, implicit::Vector{TypeVar})
    if x.head == :identifier
        var = cast2Symbol(x.val)
        if haskey(params, var)
            return params[var]
        end
        return getproperty(mod, var)
    elseif x.head == :literal
        return x.val.val
    elseif x.head == :(.)
        return getproperty(fasteval(mod, x.args[1], params, implicit), cast2Symbol(x.args[2].args[1].val))
    elseif x.head == :curly
        newimplicit = Vector{TypeVar}()
        ty = fasteval(mod, x.args[1], params, implicit)
        args = Any[]
        for i in 2:length(x.args)
            push!(args, fasteval(mod, x.args[i], params, newimplicit))
        end
        ty = Core.apply_type(ty, args...)
        for i in implicit
            ty = UnionAll(ty, i)
        end
        return ty
    elseif x.head == :(<:) && length(x.args) == 1
        var = TypeVar(gensym(), fasteval(mod, x.args[1], params, implicit))
        push!(implicit, var)
        return var
    else
        # dangerous, complicated type application is dangerous
        error("Not supported $(typeof(x)), type too complicated for analysis $(x)")
    end
end

function collectQualifiedName(ctx::GlobalContext, ast::JuAST)::Vector{Symbol}
    rel = Symbol[]
    while true
        if ast.head == :identifier
            push!(rel, cast2Symbol(ast.val))
            return rel
        elseif ast.head == :(.)
            lhs = ast.args[1]
            rhs = ast.args[2]
            if lhs.head == :identifier && rhs.head == :quote && rhs.args[1].head == :identifier
                push!(rel, cast2Symbol(lhs.val))
                ast = rhs.args[1]
            end
        else
            reportASTError(ctx, ast, "Not a valid function name, only support identifier (f) or qualified name (Base.f)")
        end
    end
    return rel
end

function constructParam(ctx::GlobalContext, ast::JuAST)::Argument
    initializer = None(JuAST)
    typ = None(JuAST)
    if ast.head == :(=)
        initializer = Just(ast.args[2])
        ast = ast.args[1]
    end
    if ast.head == :(::)
        if length(ast.args) == 1
            typ = Just(ast.args[1])
            # empty argument
            return Argument(Symbol("_"), typ, initializer)
        end
        typ = Just(ast.args[2])
        ast = ast.args[1]
    end
    if ast.head == :identifier
        return Argument(cast2Symbol(ast.val), typ, initializer)
    else
        reportASTError(ctx, ast, "Invalid argument definition")
    end
end

# We don't have an Engine this time, so we can only report error by using GlobalContext
function prepareToplevelFunction(ctx::GlobalContext, ast::JuAST)::FunDef
    #assertASTKind(ast, :function)
    if length(ast.args) == 0
        reportASTError(ctx, ast, "Function is empty")
    end
    # Note : we disallowed left to right dependency
    sparams = Pair{Symbol, Maybe{JuAST}}[]
    rt::Maybe{JuAST} = None(JuAST)
    args = Argument[]
    optargs = Argument[]
    kwargs = Argument[]
    f = ast.args[1]
    if length(ast.args) == 1
        reportASTError(ctx, ast, "Function is empty")
    end
    body = ast.args[2]
    # TODO ：handle multiple where ? or maybe just disallow it!!!
    if f.head == :where
        for i in 2:length(f.args)
            sparam = f.args[i]
            if sparam.head == :(<:)
                if length(sparam.args) != 2
                    reportASTError(ctx, f, "<: should contain two children")
                end
                if sparam.args[1].head != :identifier
                    reportASTError(ctx, f, "LHS of <: not an identifier")
                end
                push!(sparams, cast2Symbol(sparam.args[1].val)=>Just(sparam.args[2]))
            elseif sparam.head == :identifier
                push!(sparams, cast2Symbol(sparam.val)=>None(JuAST))
            else
                reportASTError(ctx, f, "Parameters (a.k.a where) in function definition can only contain symbol or a bound")
            end
        end
        f = f.args[1]
    end
    if f.head == :(::)
        rt = Just(f.args[2])
        f = f.args[1]
    end
    if f.head == :call
        fname = collectQualifiedName(ctx, f.args[1])
        for i in 2:length(f.args)
            iarg = f.args[i]
            if iarg.head == :parameters
                for kw in iarg.args
                    push!(kwargs, constructParam(ctx, kw))
                end
            elseif iarg.head == :(=)
                push!(optargs, constructParam(ctx, iarg))
            else
                push!(args, constructParam(ctx, iarg))
            end
        end
        return FunDef(ast, sparams, rt, fname, args, optargs, kwargs, body)
    else
        reportASTError(ctx, ast, "Not a valid function definition, function f end and anoymous function is not supported yet")
    end
end

@nocheck function makeSparamCompileType(mi::Core.MethodInstance, i::Int)::CompileType
    return makeConstVal(mi.sparam_vals[i])
end

@nocheck function makeArgCompileType(mi::Core.MethodInstance, i::Int)::CompileType
    return makeType(mi.specTypes.parameters[i])
end

@nocheck function specTypesLength(mi::Core.MethodInstance)::Int
    return length(mi.specTypes.parameters)
end

function makeKwargType(arg::Argument)::CompileType
    # hack ...
    makeType(Int)
end

function checkToplevelFunction(eng::Engine, 
                               fundef::FunDef,
                               kwargs::Dict{Symbol, CompileType})::InferReport
    ast = fundef.ast
    smapping = Dict{Symbol, ContextValue}()
    mi = eng.mi
    if length(mi.sparam_vals) != length(fundef.sparams)
        error("mismatched param size")
    end
    for i in 1:length(mi.sparam_vals)
        # TODO : the ast is incorrect here...
        # add mapping here!
        node = makeSparamFlowNode(ast, makeSparamCompileType(mi, i))
        smapping[fundef.sparams[i].first] = ContextValue(node, node)
    end
    mapping = Dict{Symbol, ContextValue}()
    for i in 1:length(fundef.args)
        # TODO : the ast is incorrect here...
        # add mapping here!
        node = makeParamFlowNode(ast, makeArgCompileType(mi, i+1))
        arg = fundef.args[i]
        if arg.name !== Symbol("_")
            mapping[arg.name] = ContextValue(node, node)
        end
    end
    ctx = Context(ImmutableDict(merge(smapping, mapping)))
    # firstly we fill in all the normal arguments, then we fill in the 
    for i in 1:length(fundef.optargs)
        # TODO : the ast is incorrect here...
        # add mapping here!
        index = i + length(fundef.args) + 1
        arg = fundef.optargs[i]
        # TODO : check input here
        if index <= specTypesLength(mi)
            initnode = makeParamFlowNode(ast, makeArgCompileType(mi, index))
        else
            if !isNone(arg.initializer)
                rel = inferExpr(eng, ctx, castJust(arg.initializer))
                initnode = rel.node
                ctx = rel.ctx
            else
                throw(InternalError("Optional parameter must have an initializer"))
            end
            initnode = makeParamFlowNode(ast, initnode.typ)
        end
        if !isNone(arg.typ)
            rel = inferExpr(eng, ctx, castJust(arg.typ))
            typnode = rel.node
            ctx = rel.ctx
            ttt = lift(typnode.typ)
            if !tryMergeCompileValue(ttt, initnode.typ)
                reportErrorAssignInitIncompatible(eng, ast, ttt, initnode.typ)
            end
        end
        ctx = update(ctx, arg.name, ContextValue(initnode, initnode))
    end

    for i in 1:length(fundef.kwargs)
        # TODO : the ast is incorrect here...
        # add mapping here!
        arg = fundef.kwargs[i]
        mtypnode = None(FlowNode)
        minitnode = None(FlowNode)
        if !isNone(arg.initializer)
            rel = inferExpr(eng, ctx, castJust(arg.initializer))
            minitnode = Just(rel.node)
            ctx = rel.ctx
        end
        if !isNone(arg.typ)
            rel = inferExpr(eng, ctx, castJust(arg.typ))
            mtypnode = Just(rel.node)
            ctx = rel.ctx
        end

        if haskey(kwargs, arg.name)
            inputtyp = kwargs[arg.name]
            if !isNone(mtypnode)
                typnode = castJust(mtypnode)
                ttt = lift(typnode.typ)
                if !tryMergeCompileValue(ttt, inputtyp)
                    reportErrorAssignInitIncompatible(eng, ast, ttt, inputtyp)
                end
            end
            node = makeParamFlowNode(ast, inputtyp)
        else
            if isNone(minitnode)
                reportErrorkeywordNotDefined(eng, ast, arg.name)
            else
                initnode = castJust(minitnode)
                if !isNone(mtypnode)
                    typnode = castJust(mtypnode)
                    ttt = lift(typnode.typ)
                    if !tryMergeCompileValue(ttt, initnode.typ)
                        reportErrorAssignInitIncompatible(eng, ast, ttt, initnode.typ)
                    end
                end
                node = makeParamFlowNode(ast, initnode.typ)
            end
        end
        ctx = update(ctx, arg.name, ContextValue(node, node))
    end
    # Firstly, infer return type
    astrt = fundef.rt 
    if !isNone(astrt)
        # TODO : fix the flow node here
        rel = inferExpr(eng, ctx, castJust(astrt))
        # TODO : use a interface functon for val -> typ conversion
        eng.retVal = Just(makeExpectedReturnFlowNode(ast, lift(rel.node.typ)))
        ctx = rel.ctx
    end
    rel = inferExpr(eng, ctx, fundef.body)
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
    return InferReport(ast, eng, rel)
end

function evalMods(parent::Core.Module, s::Vector{Symbol})::Core.Module
    val = evalGetPropertyChain(parent, s)
    if val isa Module
        return val
    else
        error("Not here")
    end
end

@nocheck function evalGetPropertyChain(parent::Core.Module, s::Vector{Symbol})
    base = parent
    for i in s
        base = getproperty(base, i)
    end
    return base
end

function collectToplevelFunction(ctx::GlobalContext, ast::JuAST)
    rel = Dict{Vector{Symbol}, Vector{FunDef}}()
    mod = Symbol[]
    rel[mod] = FunDef[]
    collectToplevelFunction!(ctx, rel, mod, ast)
    return rel
end

function readString(io)
    read(io, String)
end


@nocheck function mapAST2Method(mod::Module, fundef::FunDef)::Vector{Core.Method}
    # Given an JuAST, we try to select an appropriate method for its definition
    # Currently, keywords and optional functions are not handled
    # we handle them lately !!!
    paramMap = Dict{Symbol, TypeVar}()
    for (name, bound) in fundef.sparams
        if isNone(bound)
            boundv = Any
        else
            boundv = fasteval(mod, castJust(bound), paramMap)
        end
        paramMap[name] = TypeVar(name, boundv)
    end
    result = Any[]
    for arg in fundef.args
        tt = arg.typ
        if isNone(tt)
            v = Any
        else
            v = fasteval(mod, castJust(tt), paramMap)
        end
        push!(result, v)
    end
    f = evalGetPropertyChain(mod, fundef.fname)
    optresult = Any[]
    allmethods = Core.Method[]

    base = Tuple{Core.Typeof(f), result...}
    # construct from inside out    
    for i in reverse(fundef.sparams)
        base = UnionAll(paramMap[i.first], base)
    end
    push!(allmethods, Base.which(base))
    
    for arg in fundef.optargs
        tt = arg.typ
        if isNone(tt)
            v = Any
        else
            v = fasteval(mod, castJust(tt), paramMap)
        end
        push!(optresult, v)
        # iterate in reverse order
        base = Tuple{Core.Typeof(f), vcat(result, optresult)...}
        # construct from inside out
        for i in reverse(fundef.sparams)
            base = UnionAll(paramMap[i.first], base)
        end
        push!(allmethods, Base.which(base))
    end
    return allmethods
end

@nocheck function isConcreteMethod(meth::Core.Method)::Bool
    z = meth.sig
    if z isa UnionAll
        return false
    end
    if !(z <: Tuple)
        return false
    end
    for i in 2:length(z.parameters)
        if !Base.isconcretetype(z.parameters[i])
            return false
        end
    end
    return true
end

@nocheck function getCodeInfo(meth::Core.Method)::Any
    Base.code_typed_by_type(meth.sig)[1].first
end

@nocheck function collectToplevelFunction!(ctx::GlobalContext, rel::Dict{Vector{Symbol}, Vector{FunDef}}, mod::Vector{Symbol}, ast::JuAST)::Nothing
    isFunc = false
    if ast.head == :module
        mname = ast.args[2]
        if mname.head != :identifier
            reportASTError(ctx, ast, "Module name must be an identifier")
        end
        bodyast = ast.args[3]
        if bodyast.head != :block
            reportASTError(ctx, ast, "Module body must be an block")
        end
        newmod = vcat(mod, cast2Symbol(mname.val))
        rel[newmod] = FunDef[]
        for i in bodyast.args
            collectToplevelFunction!(ctx, rel, newmod, i)
        end
    elseif ast.head == :function
        isFunc = true
    elseif ast.head == :(=)
        f = ast.args[1]
        while true
            if f.head == :where || f.head == :(::)
                f = f.args[1]
            else
                break
            end
        end
        if f.head == :call
            isFunc = true
        end
    elseif ast.head == :toplevel
        for i in ast.args
            collectToplevelFunction!(ctx, rel, mod, i)
        end
    elseif ast.head == :macrocall 
        if length(ast.args) == 3
            n1 = ast.args[1]
            if n1.head == :identifier
                sym = cast2Symbol(n1.val)
                if sym == Symbol("core_@doc")
                    collectToplevelFunction!(ctx, rel, mod, ast.args[3])
                end
            end
        end
    end
    if isFunc
        try
            def = prepareToplevelFunction(ctx, ast)
            push!(rel[mod], def)
        catch e
            if e isa SyntaxError
                println("At $(formatLocation(e.ast.loc))\n", e.msg)
            else
                rethrow(e)
            end
        end
    end
    return
end

@nocheck function addFile!(ctx::GlobalContext, mod::Core.Module, filename::String)
    if !isabspath(filename)
        error("$filename is not a absolute path")
    end
    str = open(readString, filename)
    ast = parseJuAST(str, filename)
    for pair in collectToplevelFunction(ctx, ast)
        mods = pair.first
        mod = evalMods(mod, mods)
        fundefs = pair.second
        for fundef in fundefs
            try
                meths = mapAST2Method(mod, fundef)
                for i in meths
                    ctx.methodDefs[i] = fundef
                end
                meth = meths[end]
                # meth is the corresponding method definition for ex 
                if isConcreteMethod(meth)
                    ci = getCodeInfo(meth)
                    if ci isa Core.CodeInfo
                        mi = ci.parent
                        if mi isa Core.MethodInstance
                            push!(ctx.queue, KwFunCall(mi))
                        end
                    end
                end
            catch e
                println("CompilerError : At $(formatLocation(fundef.ast.loc)) : Failed to analyze this method, maybe the type signature too complicated, skip it")
            end
        end
    end
end

@nocheck function runCheck!(ctx::GlobalContext;checkSyntax::Bool = true)
    printTop = true
    while !isempty(ctx.queue)
        kmi = popfirst!(ctx.queue)
        # this specialization is checked
        mi = kmi.mi
        meth = mi.def
        if meth isa Module
            continue
        end
        # meth isa Method
        if !haskey(ctx.methodDefs, meth)
            continue
        end
        if haskey(ctx.hasChecked, mi)
            keys = ctx.hasChecked[mi]
            found = false
            for (k, v) in keys
                if k == kmi.kwargs
                    found = true
                end
            end
            if found
                continue
            end
        else
            ctx.hasChecked[mi] = Pair{KeywordSortList, Any}[]
        end
        push!(ctx.hasChecked[mi], kmi.kwargs=>nothing)
        fundef = ctx.methodDefs[meth]
        tts = mi.specTypes
        # mark the specialization in check
        try
            sictx = analyzeScopeVariable(ctx, fundef)
            eng = Engine(ctx, mi, sictx.infos)
            checkToplevelFunction(eng, fundef, Dict(kmi.kwargs))
        catch e
            if e isa InferenceError || e isa SyntaxError
                if !checkSyntax && e isa SyntaxError
                    continue
                end
                if printTop
                    println("────────────────────────────────────────────────────────────────")
                    printTop = false
                end
                println("Checking $mi\n")
                # replace with a error
                ctx.hasChecked[mi][end] = ctx.hasChecked[mi][end].first=>e
                if e isa SyntaxError
                    println("At $(formatLocation(e.ast.loc))\n", e.msg)
                else
                    typname = split(string(typeof(e)), '.')[end]
                    x = "displayError" * typname[length("InferenceError")+1:end]
                    f = eval(Symbol(x))
                    f(e)
                    println(String(take!(ctx.errio.io)))
                end
                println("────────────────────────────────────────────────────────────────")
            else
                rethrow(e)
            end
        finally
        end
    end
end