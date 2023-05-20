function hasVar(info::ScopeInfo, var::Symbol)::Bool
    haskey(info.locals, var) || haskey(info.implicitlocals, var) || haskey(info.globals, var)
end

function hasVar(ctx::ScopeInfoContext, var::Symbol)::Bool
    for i in reverse(eachindex(ctx.chains))
        info = ctx.chains[i]
        if hasVar(info, var)
            return true
        end
    end
    return false
end

function reportErrorASTAssignmentBeforeDeclaration(ctx::ScopeInfoContext, sym::Symbol, assign::JuAST, declare::JuAST)
    io = IOBuffer()
    println(io, "At $(formatLocation(assign.loc))\nDeclaration of variable $sym must be placed before an assignment")
    println(io, "Previously declared at $(formatLocation(declare.loc))")
    msg = String(take!(io))
    reportASTError(ctx.ctx, declare, msg)
end


function updateVar(ctx::ScopeInfoContext, sym::Symbol, ast::JuAST)::Nothing
    # firstly we search through the scope chain
    for i in reverse(eachindex(ctx.chains))
        local info = ctx.chains[i]
        # the variable is declared in this scope
        if hasVar(info, sym)
            # either the variable belongs to innermost scope
            # or the variable belongs to parent scope

            # If the variable belongs to innermost scope
            if i == lastindex(ctx.chains)
                #  has one declaration
                if haskey(info.locals, sym)
                    # we check whether we have encounter that declaration
                    if !(sym in info.curlocals)
                        reportErrorASTAssignmentBeforeDeclaration(ctx, sym, ast, info.locals[sym])
                    end
                elseif haskey(info.globals, sym)
                    if !(sym in info.curglobals)
                        reportErrorASTAssignmentBeforeDeclaration(ctx, sym, ast, info.globals[sym])
                    end
                end
                # has no declaration, the variable is implicitly defined
                push!(info.curimplicitlocals, sym)
                return
            else
                # If the variable belongs to outer scope
                # if the variable is updated
                # we require that the variable is already declared in outer scope
                if (sym in info.curlocals) || (sym in info.curglobals) || (sym in info.curimplicitlocals)
                    # if a local variable is updated, we record it in this scope
                    if !(sym in info.curglobals)
                        # we populate every scope in the chain
                        for j in i+1:lastindex(ctx.chains)
                            local info = ctx.chains[j]
                            if !haskey(info.modified, sym)
                                info.modified[sym] = ast
                            end
                        end
                    end
                    return
                else
                    if haskey(info.globals, sym)
                        declareast = info.globals[sym]
                    elseif haskey(info.locals, sym)
                        declareast = info.locals[sym]
                    elseif haskey(info.implicitlocals, sym)
                        declareast = info.implicitlocals[sym]
                    else
                        throw(InternalError("error"))
                    end
                    reportErrorASTAssignmentBeforeDeclaration(ctx, sym, ast, declareast)
                end
            end
        end
    end
    throw(InternalError("error"))
end

function analyzeScopeVariableAssignLHS(ctx::ScopeInfoContext, lhs::JuAST, isLocalDeclare::Bool, isGlobalDeclare::Bool)::ScopeInfoContext
    # isDeclare decides whether this is the lhs of a explicit scope declaration
    # or an implicit one
    walkTurn = ctx.walkTurn
    info = ctx.chains[end]
    if lhs.head == :identifier
        sym = cast2Symbol(lhs.val)
        if walkTurn == 1
            # the first time we meet a declaration, we record it
            # otherwise we raise an error
            if isLocalDeclare
                if haskey(info.globals, sym)
                    reportASTError(ctx.ctx, lhs, "Variable is already declared to be a global")
                end
                if haskey(info.locals, sym)
                    reportASTError(ctx.ctx, lhs, "Local declares more than once")
                end
                info.locals[sym] = lhs
            elseif isGlobalDeclare
                if haskey(info.locals, sym)
                    reportASTError(ctx.ctx, lhs, "Variable is already declared to be a local")
                end
                if haskey(info.globals, sym)
                    reportASTError(ctx.ctx, lhs, "twice defintion")
                end
                info.globals[sym] = lhs
            end
        elseif walkTurn == 2
            # the second time we meet a non-declaration, we record it
            if !isLocalDeclare && !isGlobalDeclare
                # if we haven't encounter this variable anywhere, then it belongs to innermost scope
                if !hasVar(ctx, sym)
                   info.implicitlocals[sym] = lhs
                end
            end
        elseif walkTurn == 3
            if isLocalDeclare
                push!(info.curlocals, sym)
            elseif isGlobalDeclare
                push!(info.curglobals, sym)
            else
                updateVar(ctx, sym, lhs)
            end
        end
    elseif lhs.head == :(::)
        # analyze type first
        analyzeScopeVariable(ctx, lhs.args[2])
        var = lhs.args[1]
        if var.head == :identifier
            sym = cast2Symbol(var.val)
            if walkTurn == 1
                if (!isLocalDeclare && !isGlobalDeclare) && hasVar(ctx, sym)
                    # Julia interpret this as an update operation
                    # while we expect it to be an local declaration
                    # report error here
                    if !hasVar(ctx.chains[end], sym)
                        reportASTError(ctx.ctx, lhs, "type assertion can only apply to variable in current scope")
                    else
                        reportASTError(ctx.ctx, lhs, "type assertion applies multiple times")
                    end
                end
                # we treat this like a local...
                info.locals[sym] = var
            elseif walkTurn == 3
                push!(info.curlocals, sym)
            end
        end
    elseif lhs.head == :(.) || lhs.head == :ref
        analyzeScopeVariable(ctx, lhs)
        # not an assignment
    elseif lhs.head == :tuple
        for i in lhs.args
            analyzeScopeVariableAssignLHS(ctx, i, isLocalDeclare, isGlobalDeclare)
        end
    end
    # refuse to analyze in other cases
    return ctx
end

function enterScope(ctx::ScopeInfoContext, parent::JuAST, body::JuAST)
    if parent.head == :for 
        iter = parent.args[1]
        if iter.head == :block
            iter = iter.args[1]
        end
        analyzeScopeVariable(ctx, iter.args[2])
    end
    if ctx.walkTurn == 1
        info = ScopeInfo(parent)
        addInfo!(ctx, parent, info)
    else
        info = ctx.infos[parent]
    end
    push!(ctx.chains, info)
    if parent.head == :for
        iter = parent.args[1]
        # multiple binding
        if iter.head == :block
            for i in iter.args
                analyzeScopeVariableAssignLHS(ctx, i.args[1], true, false)
            end
        else
            lhs = iter.args[1]
            analyzeScopeVariableAssignLHS(ctx, lhs, true, false)
        end
    elseif parent.head == :let
        for i in parent.args[1].args
            # assignment form
            analyzeScopeVariableAssign(ctx, i, true, false)
        end
    else
        error()
    end
    analyzeScopeVariable(ctx, body)
    pop!(ctx.chains)
    return
end


function enterWhileScope(ctx::ScopeInfoContext, parent::JuAST, body::JuAST)
    if ctx.walkTurn == 1
        info = ScopeInfo(parent)
        addInfo!(ctx, parent, info)
    else
        info = ctx.infos[parent]
    end
    push!(ctx.chains, info)
    ctx.infos[parent] = info
    analyzeScopeVariable(ctx, body)
    pop!(ctx.chains)
    return
end

function analyzeScopeVariableAssign(ctx::ScopeInfoContext, ast::JuAST, isLocalDeclare::Bool, isGlobalDeclare::Bool)::ScopeInfoContext
    if ast.head == :(=) && length(ast.args) == 2
        analyzeScopeVariable(ctx, ast.args[2])
        analyzeScopeVariableAssignLHS(ctx, ast.args[1], isLocalDeclare, isGlobalDeclare)
    else
        analyzeScopeVariableAssignLHS(ctx, ast, isLocalDeclare, isGlobalDeclare)
    end
end

# Every scope construct has an associating ScopeInfo
function analyzeScopeVariable(ctx::ScopeInfoContext, ast::JuAST)::ScopeInfoContext
    if ast.head == :(=)
        return analyzeScopeVariableAssign(ctx, ast, false, false)
    elseif ast.head == :global
        for i in ast.args
            analyzeScopeVariableAssign(ctx, i, false, true)
        end
        return ctx
    elseif ast.head == :local
        for i in ast.args
            analyzeScopeVariableAssign(ctx, i, true, false)
        end
        return ctx
    elseif (ast.head == :literal || 
           ast.head == :identifier || 
           ast.head == :char || 
           ast.head == :break ||
           ast.head == :continue
           )
        return ctx
    elseif ast.head == :generator
        reportASTError(ctx.ctx, ast, "Generator syntax doesn't support")
    elseif (ast.head == :block || 
           ast.head == :tuple ||
           ast.head == :quote || 
           ast.head == :curly || 
           ast.head == :return || 
           ast.head == :(.) || 
           ast.head == :ref ||
           ast.head == :string ||
           ast.head == :comprehension ||
           ast.head == :comparison||
           ast.head == :dotcall)
        for i in ast.args
            analyzeScopeVariable(ctx, i)
        end
        return ctx
    elseif ast.head == :macrocall
        # TODO : how we handle macro ???
        # this is incorrect, we need to analyze macro
        return ctx
    elseif ast.head == :call
        for i in ast.args
            if i.head == :(=)
                analyzeScopeVariable(ctx, i.args[2])
            elseif i.head == :parameters
                for ii in i.args
                    analyzeScopeVariable(ctx, ii)
                end
            else
                analyzeScopeVariable(ctx, i)
            end
        end 
    elseif (ast.head == :if || 
           ast.head == :elseif ||
           ast.head == :else)
        for i in ast.args
            analyzeScopeVariable(ctx, i)
        end
    elseif ast.head == :let
        enterScope(ctx, ast, ast.args[2])
    elseif ast.head == :for
        enterScope(ctx, ast, ast.args[2])
    elseif ast.head == :while
        cond = ast.args[1]
        body = ast.args[2]
        analyzeScopeVariable(ctx, cond)
        enterWhileScope(ctx, ast, body)
    else
        str = String(ast.head)
        if last(str) == '='
            if length(ast.args) == 2
                analyzeScopeVariable(ctx, ast.args[2])
                analyzeScopeVariableAssignLHS(ctx, ast.args[1], false, false)
            end
        end
    end
    return ctx
end

function analyzeScopeVariable(globalctx::GlobalContext, parent::FunDef)::ScopeInfoContext
    ctx = ScopeInfoContext(globalctx)
    ast = parent.ast
    info = ScopeInfo(ast)
    push!(ctx.chains, info)
    ctx.infos[ast] = info
    for i in parent.sparams
        info.locals[i.first] = ast
        push!(info.curlocals, i.first)
    end
    for i in parent.args
        info.locals[i.name] = ast
        push!(info.curlocals, i.name)
    end
    # TODO : analyze optional argument here...!!!
    for i in 1:3
        ctx.walkTurn = i
        analyzeScopeVariable(ctx, ast.args[2])
    end

    return ctx
end
