using JuliaSyntax
function Base.show(io::IO, ast::JuAST)
    if ast.head == :literal
        Base.print(io, ast.val)
        return 
    end
    Base.print(io, "JuAST(")
    Base.print(io, ast.head)
    Base.print(io, ", ")
    for i in 1:length(ast.children)
        c = ast.children[i]
        Base.print(io, c)
        if i != length(ast.children)
            Base.print(io,", ")
        end
    end
    Base.print(io, ")")
    return
end

function convert2JuAST(node::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}, toInfix::Bool)::Any
    v = node.data.val
    start = UInt(node.data.position)
    span = Location(node.data.source, (start, start + UInt(node.data.raw.span)-1))
    if node.children isa Nothing
        return JuAST(:literal, v, span, [])
    end
    head = node.data.raw.head
    kind = Int64(reinterpret(Int16,head.kind))
    flag = head.flags
    if (flag != JuliaSyntax.EMPTY_FLAGS && 
        flag != JuliaSyntax.INFIX_FLAG && 
        flag != 1<<5 && # MUTABLE_FLAG
        flag != 1<<7 && # assignment
        flag != 1<<4) # ! call
        println(node, head)
        error("Bad flag $(head.kind) : $flag $(JuliaSyntax.untokenize(head;include_flag_suff=true))")
    end
    shead = Symbol(JuliaSyntax._kind_names[Int(kind)+1])
    children = Vector{Any}()
    for i in node.children
        push!(children, convert2JuAST(i, toInfix))
    end
    if flag == JuliaSyntax.INFIX_FLAG && toInfix && shead == :call 
        children = vcat(children[2], [children[i] for i in range(1,length(children),step=2)])
    end
    return JuAST(shead, nothing, span, children)
end

function validateFunction(node::JuAST, expr::Expr)
    nch = node.children
    param = [i for i in nch if i isa JuAST && i.head == :parameters]
    non_param = [i for i in nch if !(i isa JuAST && i.head == :parameters)]
    nch = vcat(non_param[1],param,non_param[2:end]) 
    node = JuAST(node.head, nothing, node.span, nch)
    # rearrange the parameter here
    validate(node, expr)
end

function validateFilter(node::JuAST, expr::Expr)
    nch = node.children
    @assert length(nch) == 2
    nch = [nch[2],nch[1]]
    node = JuAST(node.head, nothing, node.span, nch)
    # rearrange the parameter here
    validate(node, expr)
end

function validateQuoteNode(node::JuAST, expr::QuoteNode)
    if node.head == :quote && length(node.children) == 1
        ch = node.children[1]
        if ch.val == expr.value && ch.head == :literal
            return (true, node, expr)
        else
            return (false, ch, expr.value)
        end
    else
        return (false, node, expr)
    end
end

function validate(node::JuAST, expr::Expr)
    if node.head != expr.head
        if !(expr.head == :kw && node.head == :(=))
            return (false, node, expr)
        end
    end
    nch = node.children
    ech = expr.args
    if length(nch) != length(ech)
        return (false, node, expr)
    end
    for i in 1:length(nch)
        inch = nch[i]
        iech = ech[i]
        if iech isa QuoteNode
            res = validateQuoteNode(inch, iech)
            if !res[1]
                return res
            end
            continue
        end
        if iech isa String
            if inch.head == :string
                if inch.children[1].val == iech
                    continue
                end
            elseif inch.head == :literal
                if inch.val == iech
                    continue
                end
            end
            return (false, inch, iech)
        end
        if iech isa Expr
            if inch isa JuAST
                if inch.head == :call
                    res = validateFunction(inch, iech)
                elseif inch.head == :filter
                    res = validateFilter(inch, iech)
                else
                    res = validate(inch, iech) 
                end
                if !res[1]
                    return res
                end
                continue
            else
                return (false, inch, iech)
            end
        else
            if inch.head != :literal || inch.val != iech
                return (false, inch, iech)
            end
        end
    end
    return (true, node, expr)
end

function removeLineInfo(e::Expr)
    args = []
    for i in e.args
        if i isa Core.LineNumberNode
            continue
        elseif i isa Expr
            push!(args, removeLineInfo(i))
        else
            push!(args, i)
        end
    end
    if e.head == :if || e.head == :elseif 
        l = args[1]
        if l isa Expr && l.head == :block && length(l.args) == 1
            args[1] = l.args[1]
        end
    end
    return Expr(e.head, args...)
end

function parseJuAST(s::String;filename=nothing, valid = true)
    node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, s;filename)
    ast = convert2JuAST(node, true)
    expr = removeLineInfo(JuliaSyntax.parseall(Expr, s;filename))
    if valid
        res = validate(ast, expr)
        if res[1]
            return ast
        else
            Base.throw(res)
        end
    else
        return ast
    end
end

function flattenIf!(rel, ast::JuAST)::Union{JuExpr, Nothing}
    if length(ast.children) == 3 
        push!(rel, (typedConvertAST(ast.children[1]), typedConvertAST(ast.children[2])))
        if ast.children[3].head == :elseif
            flattenIf!(rel, ast.children[3])
        else
            return typedConvertAST(ast.children[3])
        end
    elseif length(ast.children) == 2
        push!(rel, (typedConvertAST(ast.children[1]), typedConvertAST(ast.children[2])))
        return nothing
    else
        error()
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
        if lhs_.head == :(.)
            p_ = lhs_.children[1]
            x_ = lhs_.children[2]
            p = typedConvertAST(p_)
            if x_.head == :quote
                c = x_.children[1]
                return JuExpr(SetProperty(ast, p, c.val::Symbol, rhs))
            else
                error("Not a quote for get property")
            end
        elseif lhs_.head == :ref
            p_ = lhs_.children[1]
            p = typedConvertAST(p_)
            xs = [typedConvertAST(i) for i in lhs_.children[2:end]]
            return JuExpr(ArraySet(ast, p, xs, rhs))
        else
            lhs = typedConvertAST(lhs_)
            lhsval = lhs.val
            if lhsval isa Var
                return JuExpr(Assign(ast, lhsval, rhs))
            elseif lhsval isa TypedAssert
                llval = lhsval.lhs.val
                if llval isa Var
                    return JuExpr(TypedAssign(ast, llval, lhsval.rhs, rhs))
                end
            end
            error("bad assign")
        end
    elseif ast.head == :ref
        lhs = typedConvertAST(ast.children[1])
        rhs = [typedConvertAST(i) for i in ast.children[2:end]]
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
    elseif ast.head == :module
        name = typedConvertAST(ast.children[2])
        nameval = name.val
        if nameval isa Var
            return JuExpr(ModDef(ast, nameval.id, typedConvertAST(ast.children[3])))
        else
            error()
        end
    elseif ast.head == :macrocall
        @warn "Ignore macrocall $(ast.children[1]) here"
        return JuExpr(Literal(ast, makeConstVal(nothing)))
    elseif ast.head == :(<:) || ast.head == :(&&) || ast.head == :(||)
        # subtyping lowered as function call
        # this is actually incorrect, because <: can also appear in type application
        # like Array{<:Int, 1}, we need to check this case more carefully, currently we ignore compilicated type definition
        f = JuExpr(Var(ast, ast.head))
        args = [typedConvertAST(i) for i in ast.children]
        kwargs = JuExpr[]
        return JuExpr(FunCall(ast, f, args, kwargs))
    elseif ast.head == :(::)
        return JuExpr(TypedAssert(ast, typedConvertAST(ast.children[1]),typedConvertAST(ast.children[2])))
    else
        println(ast)
        error()
    end
end
