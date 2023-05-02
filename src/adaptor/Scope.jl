#=
Julia's scope behavior is extremely bad
For example, the variable t in following code is a local variable of the function instead of loop

function f()
    if true
        for i in 1:10
            t = 2
            println(t)
        end
        # t is 2 here
    else
        t = 1
    end
end

Also, Julia's if expression is not scoped, so following code is possible:
function g()
    if true
        local x = 1
    end
    x = 1.0
end

That's said, Julia's unscoped if and random declaration causes a lot of troubles.


We propose following restriction 
1. A variable can declared at most once
2. If a variable is declared somewhere, then all assignments to this variable must be dominated by that declaration
3. If the declaration is in an if clause, then that declaration has local scope (local in that branch of if)
   (otherwise, the user should promote that declaration outside of if).

This simplifies our type inference algorithm, there is no need to look forward to find the type declaration 
or perform join on the definition for a dead declaration in an if expression

Our analysis proceeds in several steps:
1. Firstly, we scan the AST to collect all declarations (typed or untyped) and make sure that a variable can declared at most once
    If there are any multiple definitions, then we quit immediately
2. Then we scan the AST again, but this time we also pay attention to assignment expressions
    a) When encounter an assignment expression, 
        Check whether the declaration is dead in step 3d)
        if so, then a declaration is used outside of a if, abort
    Then check whether the declaration is marked in step 2c), 
        if not, then a declaration is occured after an assignment, abort
    b) When encounter an expression other than if (or && || ), we walk subexpressions recursively (in evaluation order)
    c) When encounter a declaration, mark this declaration
    c) When encounter an if expression, 
            if (c1)
                b1
            elseif (c2)
                b2
            elseif (c3)
                b3
            end
        we check c1 then check b1, we mark the variable declared in b1 (that is, variables marked in b1 with step 3c) dead
        then we check c2 then b2, then we mark the variable declared in b2 dead
    
    function walkMark(v, markedInParent, markInChild, deadset)
        if v isa IfStmt
            walked!(cond, markedInParent, markInChild)
            newchild = []
            cur = merge(markedInParent, markInChild)
            walked!(body, cur, newchild)
            # newchild is discarded, so they are all dead
            append(deadset, newchild)
        elseif v isa Assign
            if v in markedInParent || v in markInChild
                error("Unmarked)
            end
            return []
        elseif v isa Declaration
            return [v]
        else
            for subexpression:
                walkMark(v, markedInParent, markInChild)
            end
        end
    end
=#
function preAnalyzeScope()
end
#=
    I decide to use a walker here, otherwise too repetitive...
=#
abstract type JuExprWalker end
function walkLiteral(walker::JuExprWalker{T}, ex::JuExpr)::T where T
end
function walkAssign(walker::JuExprWalker{T}, ex::JuExpr)::T where T
end
function walkTypedAssign(walker::JuExprWalker{T}, ex::JuExpr)::T where T
end
function walkJuExpr(walker::JuExprWalker{T}, ex::JuExpr)::T where T
       # parent and current is always disjoint, because we can have at most one definition
       val = ast.val
       if val isa Literal
           return
       elseif val isa Assign
           id = val.lhs.id
           if haskey(parent, id) || haskey(id, current)
               return 
           elseif haskey(parent, dead)
               error("Dead variable is used")
           else
               error("Assigned too early")
           end
       elseif val isa FunCall
           analyzeScope(val.f, parent, current, dead)
           for i in val.args
               analyzeScope(i, parent, current, dead)
           end
           for i in val.kwargs
               analyzeScope(i, parent, current, dead)
           end
           return
       elseif val isa Block
           for i in val.stmts
               analyzeScope(i, parent, current, dead)
           end
       elseif val isa Var
           if haskey(dead, val.id)
               error("Using dead variable outside of loop")
           end
           return
       elseif val isa CurlyCall
           analyzeScope(val.f, parent, current, dead)
           for i in val.args
               analyzeScope(i, parent, current, dead)
           end
           return
       elseif val isa IfStmt
           error()
           for i in val.branches
               analyzeScope(i, parent, current, i[1])
               analyzeScope(i, parent, current, i[2])
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
               analyzeScope(eval, parent, current, dead)
               return
           end
       elseif val isa ArrayRef
           analyzeScope(var.arr, parent, current, dead)
           for i in val.i
               analyzeScope(i, parent, current, dead)
           end
           return
       elseif val isa GetProperty
           analyzeScope(var.x, parent, current, dead)
           return 
       elseif val isa SetProperty
           analyzeScope(var.x, parent, current, dead)
           analyzeScope(var.v, parent, current, dead)
           return 
       elseif val isa ArraySet
           analyzeScope(var.x, parent, current, dead)
           for i in val.i
               analyzeScope(i, parent, current, dead)
           end
           analyzeScope(var.v, parent, current, dead)
           return
       elseif val isa TypedAssert
           error()
           id = val.lhs.id
           if haskey(parent, id) || haskey(id, current)
               return 
           elseif haskey(parent, dead)
               error("Dead variable is used")
           else
               error("Assigned too early")
           end
           return
       elseif val isa ForStmt
           analyzeScope(val.iter, parent, current, dead)
           newscope = enterScope(parent, current, val.scope)
           newscope[val.var] = true
           analyzeScope(val.body, newscope, DefMap(), dead)
       elseif val isa WhileStmt
           analyzeScope(val.cond, parent, current, dead)
           newscope = enterScope(parent, current, val.scope)
           analyzeScope(val.body, newscope, DefMap(), dead)
       elseif val isa FunDef
           newscope = enterScope(parent, current, val.scope)
           error()
       else
           error("Unimplemented $ast")
       end
    end
end

function enterScope(parent::DefMap, current::DefMap, scope::DefMap)
    res = DefMap()
    for i in keys(scope)
        res[i] = false
    end
    for i in keys(current)
        if !haskey(res, i)
            res[i] = current[i]
        end
    end
    for i in keys(parent)
        if !haskey(res, i)
            res[i] = parent[i]
        end
    end
    return res
end

const DefMap = Dict{Symbol, JuExpr}

function analyzeScope(ex::JuExpr, parent::DefMap, current::DefMap, dead::DefMap)
    # parent and current is always disjoint, because we can have at most one definition
    val = ast.val
    if val isa Literal
        return
    elseif val isa Assign
        id = val.lhs.id
        if haskey(parent, id) || haskey(id, current)
            return 
        elseif haskey(parent, dead)
            error("Dead variable is used")
        else
            error("Assigned too early")
        end
    elseif val isa FunCall
        analyzeScope(val.f, parent, current, dead)
        for i in val.args
            analyzeScope(i, parent, current, dead)
        end
        for i in val.kwargs
            analyzeScope(i, parent, current, dead)
        end
        return
    elseif val isa Block
        for i in val.stmts
            analyzeScope(i, parent, current, dead)
        end
    elseif val isa Var
        if haskey(dead, val.id)
            error("Using dead variable outside of loop")
        end
        return
    elseif val isa CurlyCall
        analyzeScope(val.f, parent, current, dead)
        for i in val.args
            analyzeScope(i, parent, current, dead)
        end
        return
    elseif val isa IfStmt
        error()
        for i in val.branches
            analyzeScope(i, parent, current, i[1])
            analyzeScope(i, parent, current, i[2])
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
            analyzeScope(eval, parent, current, dead)
            return
        end
    elseif val isa ArrayRef
        analyzeScope(var.arr, parent, current, dead)
        for i in val.i
            analyzeScope(i, parent, current, dead)
        end
        return
    elseif val isa GetProperty
        analyzeScope(var.x, parent, current, dead)
        return 
    elseif val isa SetProperty
        analyzeScope(var.x, parent, current, dead)
        analyzeScope(var.v, parent, current, dead)
        return 
    elseif val isa ArraySet
        analyzeScope(var.x, parent, current, dead)
        for i in val.i
            analyzeScope(i, parent, current, dead)
        end
        analyzeScope(var.v, parent, current, dead)
        return
    elseif val isa TypedAssert
        error()
        id = val.lhs.id
        if haskey(parent, id) || haskey(id, current)
            return 
        elseif haskey(parent, dead)
            error("Dead variable is used")
        else
            error("Assigned too early")
        end
        return
    elseif val isa ForStmt
        analyzeScope(val.iter, parent, current, dead)
        newscope = enterScope(parent, current, val.scope)
        newscope[val.var] = true
        analyzeScope(val.body, newscope, DefMap(), dead)
    elseif val isa WhileStmt
        analyzeScope(val.cond, parent, current, dead)
        newscope = enterScope(parent, current, val.scope)
        analyzeScope(val.body, newscope, DefMap(), dead)
    elseif val isa FunDef
        newscope = enterScope(parent, current, val.scope)
        error()
    else
        error("Unimplemented $ast")
    end
end
