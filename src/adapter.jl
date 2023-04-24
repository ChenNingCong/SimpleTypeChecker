using JuliaSyntax

struct Location
    file::JuliaSyntax.SourceFile
    span::Tuple{UInt, UInt}
end

mutable struct JuAST
    const head::Symbol
    const val::Any
    const span::Location
    const children::Vector{JuAST}
end


function formatLocation(loc::Location)::String
    file = loc.file
    span = loc.span
    start = convert2line(file.line_starts, span[1])
    ff = file.filename 
    if ff isa Nothing
        filename = "(none)"
    else
        filename = file.filename
    end
    return "at file $(filename) line $(start[1]):$(start[2])"
end

function printError(ast::JuAST)
    loc = ast.span
    println(formatLocation(loc))
    code = loc.file.code[loc.span[1]:loc.span[2]]
    println("  $code")
    return
end

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

function parseJuAST(s::String;filename=nothing)
    node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, s;filename)
    ast = convert2JuAST(node, true)
    expr = removeLineInfo(JuliaSyntax.parseall(Expr, s;filename))
    res = validate(ast, expr)
    if res[1]
        return convert2JuAST(node, true)
    else
        return res
    end
end