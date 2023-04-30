using JuliaSyntax
@nocheck function Base.show(io::IO, ast::JuAST)
    if ast.head == :literal
        Base.show(io, ast.val)
        return 
    end
    Base.print(io, "JuAST(")
    Base.print(io, ast.head)
    Base.print(io, ", ")
    for i in 1:length(ast.children)
        c = ast.children[i]
        Base.show(io, c)
        if i != length(ast.children)
            Base.print(io,", ")
        end
    end
    Base.print(io, ")")
    return
end

function convert2JuAST(node::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}, toInfix::Bool)::JuAST
    v = node.data.val
    start = UInt(node.data.position)
    span = Location(node.data.source, tuple(start, start + UInt(node.data.raw.span)-1))
    if node.children isa Nothing
        return JuAST(:literal, v, span, JuAST[])
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

function removeLineInfo(e::Expr)
    args = Any[]
    for i in e.args
        if i isa Core.LineNumberNode
            #continue
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

struct InvalidSyntax
    msg::String
    e::JuAST
end

function flattenIf!(rel, ast::JuAST)::Union{JuExpr, Nothing}
    if length(ast.children) == 3 
        push!(rel, (typedConvertAST(ast.children[1], false, true), typedConvertAST(ast.children[2], false, true)))
        if ast.children[3].head == :elseif
            flattenIf!(rel, ast.children[3])
        else
            return typedConvertAST(ast.children[3], false, true)
        end
    elseif length(ast.children) == 2
        push!(rel, (typedConvertAST(ast.children[1], false, true), typedConvertAST(ast.children[2], false, true)))
        return nothing
    else
        error()
    end
end

function flattenIf(ast::JuAST)::JuExpr
    rel = Vector{Tuple{JuExpr, JuExpr}}()
    else_b = flattenIf!(rel, ast)
    if else_b isa Nothing
        return JuExpr(IfStmt(ast, rel, else_b))
    else
        return JuExpr(IfStmt(ast, rel, else_b))
    end
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
        return (var.val::Symbol) => typedConvertAST(ex, false, true)
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

function prettyInvalidSyntax(e::InvalidSyntax)
    ast = e.e
    println('\n', formatLocation(ast.span))
    println("  ", e.msg)
    println("  ")
    Base.show(stdout, ast)
    println('\n')
end

function tryConvertAST(ast::JuAST, isToplevel::Bool, inFunction::Bool)::JuExpr
    try
        return typedConvertAST(ast, isToplevel, inFunction)
    catch e
        if e isa InvalidSyntax
            prettyInvalidSyntax(e)
            return JuExpr(Literal(ast, makeConstVal(nothing)))
        else
            rethrow(e)
        end
    end
end

function convert2Expr(ast::JuAST)
    typedConvertAST(ast, true, false)
end

function argsConvert(args::Vector{JuAST}, isToplevel, inFunction)::Vector{JuExpr}
    rel = Vector{JuExpr}(undef, length(args) - 1)
    for i in 1:length(rel)
        rel[i] = typedConvertAST(args[i+1], isToplevel, inFunction)
    end
    return rel
end

function typedConvertAST(ast::JuAST, isToplevel::Bool, inFunction::Bool)::JuExpr
    # TODO : check whether this is correct!
    # if isToplevel && !(ast.head == :block || ast.head == :literal|| ast.head == :module || ast.head == :function || ast.head == :(=) || ast.head == :toplevel)
    #    # ignore all toplevel non-definiton clause
    #    return JuExpr(Literal(ast, makeConstVal(nothing)))
    # end 
    if ast.head == :char
        return JuExpr(Literal(ast, makeConstVal(ast.children[1].val::Char)))
    end
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
            throw(InvalidSyntax("Quoted expression is disallowed", ast))
        end
    elseif ast.head == :curly
        f = typedConvertAST(ast.children[1], isToplevel, inFunction)
        args = argsConvert(ast.children, isToplevel, inFunction)
        return JuExpr(CurlyCall(ast, f, args))
    elseif ast.head == :call
        f = typedConvertAST(ast.children[1], isToplevel, inFunction)
        args = [typedConvertAST(i, isToplevel, inFunction) for i in ast.children[2:end] if i.head != :kw]
        kwargs = [typedConvertAST(i, isToplevel, inFunction) for i in ast.children[2:end] if i.head == :kw]
        return JuExpr(FunCall(ast, f, args, kwargs))
    elseif ast.head == :(=)
        rhs = typedConvertAST(ast.children[2], isToplevel, inFunction)
        lhs_ = ast.children[1]
        if lhs_.head == :(.)
            p_ = lhs_.children[1]
            x_ = lhs_.children[2]
            p = typedConvertAST(p_, isToplevel, inFunction)
            if x_.head == :quote
                c = x_.children[1]
                return JuExpr(SetProperty(ast, p, c.val::Symbol, rhs))
            else
                throw(InvalidSyntax("Not a valid setproperty AST", lhs))
            end
        elseif lhs_.head == :ref
            p_ = lhs_.children[1]
            p = typedConvertAST(p_, isToplevel, inFunction)
            xs = argsConvert(lhs_.children, isToplevel, inFunction)
            return JuExpr(ArraySet(ast, p, xs, rhs))
        else
            lhs = typedConvertAST(lhs_, isToplevel, inFunction)
            lhsval = lhs.val
            if lhsval isa Var
                return JuExpr(Assign(ast, lhsval, rhs))
            elseif lhsval isa TypedAssert
                llval = lhsval.lhs.val
                if llval isa Var
                    return JuExpr(TypedAssign(ast, llval, lhsval.rhs, rhs))
                end
            end
            throw(InvalidSyntax("Unsupported complicated assignment", ast))
        end
    elseif ast.head == :ref
        lhs = typedConvertAST(ast.children[1], isToplevel, inFunction)
        rhs = argsConvert(ast.children, isToplevel, inFunction)
        return JuExpr(ArrayRef(ast, lhs, rhs))
    elseif ast.head == :(.)
        lhs = typedConvertAST(ast.children[1], isToplevel, inFunction)
        rhs = ast.children[2]
        if rhs.head == :quote
            x_ = rhs.children[1] 
            return JuExpr(GetProperty(ast, lhs, x_.val::Symbol))
        else
            throw(InvalidSyntax("Not a valid getproperty AST", ast))
        end
    elseif ast.head == :if
        return flattenIf(ast)
    elseif ast.head == :block
        stmts = JuExpr[]
        if isToplevel
            for i in ast.children
                push!(stmts, tryConvertAST(i, isToplevel, inFunction))
            end
        else
            for i in ast.children
                push!(stmts, typedConvertAST(i, isToplevel, inFunction))
            end
        end
        return JuExpr(Block(ast, stmts))
    elseif ast.head == :return 
        if length(ast.children) == 1
            return JuExpr(Return(ast, typedConvertAST(ast.children[1], isToplevel, inFunction)))
        else
            return JuExpr(Return(ast, nothing))
        end
    elseif ast.head == :toplevel
        stmts = JuExpr[]
        for i in ast.children
            push!(stmts, tryConvertAST(i, true, false))
        end
        return JuExpr(Block(ast, stmts))
    elseif ast.head == :while
        cond = ast.children[1]
        body = ast.children[2]
        return JuExpr(WhileStmt(ast, typedConvertAST(cond, isToplevel, inFunction), typedConvertAST(body, isToplevel, inFunction)))
    elseif ast.head == :for
        cond = ast.children[1]
        body = ast.children[2]
        if cond.head != :(=)
            throw(InvalidSyntax("Invalid for expression", ast))
        end
        var = typedConvertAST(cond.children[1], isToplevel, inFunction)
        varval = var.val
        if varval isa Var
            iter = cond.children[2]
            return JuExpr(ForStmt(ast, varval, typedConvertAST(iter, isToplevel, inFunction), typedConvertAST(body, isToplevel, inFunction)))
        else
            throw(InvalidSyntax("Iterate variable is not a symbol", ast))
        end
    elseif ast.head == :string
        return JuExpr(Literal(ast, makeConstVal(ast.children[1].val)))
    elseif ast.head == :function
        if inFunction
            throw(InvalidSyntax("Nested function (closure) is not supported", ast))
        end
        if length(ast.children) < 2
            throw(InvalidSyntax("Unsupported empty function body", ast))
        end
        body = typedConvertAST(ast.children[2], false, true)
        fast = ast.children[1]
        if fast.head == :where
            # TODO : support subtyping contraint here...
            params = Vector{Symbol}(undef, length(fast.children) - 1)
            for i in 1:length(params)
                params[i] = extractSymbol(fast.children[i+1])
            end
            fast = fast.children[1]
        else
            params = Symbol[]
        end
        local rt::Union{Nothing, JuExpr}
        if fast.head == :(::)
            rt = typedConvertAST(fast.children[2], isToplevel, true)
            fast = fast.children[1]
        else
            rt = nothing
        end
        if fast.head == :call
            f = typedConvertAST(fast.children[1], isToplevel, true)
            args = [collectArgs(i) for i in fast.children[2:end] if i.head != :kw]
            for i in fast.children[2:end] 
                if i.head == :kw
                    throw(InvalidSyntax("Keyword argument is unsupported", ast))
                end
            end
            fval = f.val
            if fval isa Var
                return JuExpr(FunDef(ast, fval.id, args, Pair{Symbol, Union{TypedAST, Nothing}}[], params, rt, body))
            else
                throw(InvalidSyntax("Only support named function definition", ast))
            end
        else
            throw(InvalidSyntax("Only support named function definition", ast))
        end
    elseif ast.head == :module
        name = typedConvertAST(ast.children[2], false, false)
        nameval = name.val
        if nameval isa Var
            return JuExpr(ModDef(ast, nameval.id, typedConvertAST(ast.children[3], true, false)))
        else
            throw(InvalidSyntax("Invalid module definition", ast))
        end
    elseif ast.head == :macrocall
        # @warn "$(formatLocation(ast.span)) : ignore macrocall $(ast.children[1]) here"
        return JuExpr(Literal(ast, makeConstVal(nothing)))
    elseif ast.head == :(<:) || ast.head == :(&&) || ast.head == :(||)
        # subtyping lowered as function call
        # this is actually incorrect, because <: can also appear in type application
        # like Array{<:Int, 1}, we need to check this case more carefully, currently we ignore compilicated type definition
        f = JuExpr(Var(ast, ast.head))
        if length(ast.children) != 2
            throw(InvalidSyntax("<: in typing context is unsupported", ast))
        end
        args = [typedConvertAST(i, isToplevel, inFunction) for i in ast.children]
        kwargs = JuExpr[]
        return JuExpr(FunCall(ast, f, args, kwargs))
    elseif ast.head == :(::)
        return JuExpr(TypedAssert(ast, typedConvertAST(ast.children[1], isToplevel, inFunction),
                     typedConvertAST(ast.children[2], isToplevel, inFunction)))
    elseif ast.head in (:const, :struct, :import, :using, :export)
        return JuExpr(Literal(ast, makeConstVal(nothing)))
    elseif ast.head == :vect
        throw(InvalidSyntax("Don't use untyped array construction. Use Type[...] instead of [...]", ast))
    else
        throw(InvalidSyntax("Unsupported AST kind", ast))
        return JuExpr(Literal(ast, makeConstVal(nothing)))
    end
end
