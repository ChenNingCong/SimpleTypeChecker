using JuliaSyntax

function makeCodeSegment(raw::String, pos::Ref{Int}, ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})
    head = ast.head
    kind = head.kind
    start = pos[] + 1
    last = pos[] + ast.span
    data = raw[start:last]
    pos[] += ast.span
    range = "start = $start last = $last"
    if kind == JuliaSyntax.K"NewlineWs"
        s = join(map(x->escapseNewlineWs(String(x)), split(data, '\n')), """<span class="linebreak"></span>""")
        return s
    elseif kind == JuliaSyntax.K"Whitespace"
        data = escapseEmtpySpace(data)
        return """<span class="secondary">$data</span>"""
    else
        data = htmlEscape(data)
        if kind == JuliaSyntax.K"Comment"
            return """<span class="comment">$data</span>"""
        elseif JuliaSyntax.is_trivia(head) 
            return """<span class="delim text-Expr token" $range>$data</span>"""
        else
            return """<span class="mono text-Exp token" $range>$data</span>"""
        end
    end
    #=
    """<span class="mono text-Typ token">$s</span>"""
    """<span class="secondary">&nbsp;</span>"""
    """<span class="mono text-Pat token">$s</span>"""
    """<span class="delim text-Exp token">fun</span>"""
    """<span class="linebreak"></span>"""
    """<span class="delim text-Pat token">)</span>"""
    
    =#
end

function escapseEmtpySpace(s::String)::String
    replace(s, ' '=>"&nbsp;")
end

function escapseNewlineWs(s::String)::String
    replace(s, ' '=>"&nbsp;")
end

function htmlEscape(s::String)::String
    replace(s, '<'=>"&lt;", 
               '>'=>"&gt;", 
               '&'=>"&amp;", 
               '\''=>"&apos;", 
               '"'=> "&quot;",
               ' '=>"&nbsp;"
               )
end

function convertGreenNode!(rel, pos::Ref{Int}, raw::String, ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})
    children = ast.args
    if children isa Tuple{}
        push!(rel, makeCodeSegment(raw, pos, ast))
    else
        # a non-terminal
        for i in children
            convertGreenNode!(rel, pos, raw, i)
        end
    end
end

function convertGreenNode(raw::String, ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})
    rel = String[]
    pos::Ref{Int} = Ref{Int}(0)
    convertGreenNode!(rel, pos, raw, ast)
    return rel
end

function parseAndConvert(raw::String;filename::String)
    ast = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, raw;filename=filename)
    rel = convertGreenNode(raw, ast.data.raw)
    # we can't join them with '\n', which might introduce a 4px space between spans
    join(rel)
end


function wrap(x::String)
return """
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>title</title>
    <link rel="stylesheet" href="./style.css">
  </head>
  <body>
  <div id = "main">
  <div class = "editor single">
  <div class = "cell-container">
  <div class = "cell cell-item selected single">
  <div id = "code-container" class = "code-container">
  <div class = "code">
  <div class = "code-text">
  $x
  </div>
  </div>
  </div>
  </div>
  </div>
  </div>
  </div>
  <script src="./final.js"></script>
  </body>
</html>
"""
end



mutable struct MutableGreenNode
    const head::JuliaSyntax.SyntaxHead
    const span::UInt32
    const args::Union{Tuple{}, Vector{MutableGreenNode}}
    backpointer::Union{Nothing, JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}}
end

mutable struct MutableTreeNode
    const ast::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}
    const target::MutableGreenNode
end

function testNoDuplicatePointer!(rel::Set, ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})::Bool
    args = ast.args
    if args isa Tuple{}
        return true
    else
        if args in rel
            return false
        end
        push!(rel, args)
        for i in args
            if !testNoDuplicatePointer!(rel, i)
                return false
            end
        end
    end
    return true
end

function testNoDuplicatePointer(ast)::Bool
    rel = Set{Vector{JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}}}()
    testNoDuplicatePointer!(rel, ast)
end

function convert2MutableGreenNode!(mapping::Dict, childmapping::Dict, pos::Ref{UInt32}, ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})::MutableGreenNode
    args = ast.args
    if args isa Tuple{}
        s = Tuple{UInt32, UInt32}((pos[] + 1, pos[] + ast.span))
        m = MutableGreenNode(ast.head, ast.span, (), nothing)
        pos[] += ast.span
        childmapping[s] = m
    else
        margs = [convert2MutableGreenNode!(mapping, childmapping, pos, i) for i in args]
        m = MutableGreenNode(ast.head, ast.span, margs, nothing)
        # we establish a mapping here!
        mapping[args] = m
    end
    return m
end

function convert2MutableGreenNode(ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})
    mapping = Dict{Vector{JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}}, MutableGreenNode}()
    childmapping = Dict{Tuple{UInt32, UInt32}, MutableGreenNode}()
    pos = Ref{UInt32}(0)
    m = convert2MutableGreenNode!(mapping, childmapping, pos, ast)
    return m, mapping, childmapping
end

function forwardPointer(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}, mapping, childmapping)
    gnode = tree.data.raw
    args = gnode.args
    if args isa Tuple{}
        @assert tree.children isa Nothing
        start = UInt32(tree.data.position)
        span = (start, start + gnode.span - 1)
        m = childmapping[span]
        m.backpointer = tree
        return MutableTreeNode(tree, m)
    else
        m = mapping[args]
        m.backpointer = tree
        for i in tree.children::Vector{JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}}
            forwardPointer(i, mapping, childmapping)
        end
        return MutableTreeNode(tree, m)
    end
end

function constructBiMapping(ast::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})
    testNoDuplicatePointer(ast.data.raw)
    m, mapping, childmapping = convert2MutableGreenNode(ast.data.raw)
    mm = forwardPointer(ast, mapping, childmapping)
    return mm
end


function serializeAST!(alloc::Ref{Int}, rel::Vector{String}, ast::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}, parentid::String)::String
    alloc[] += 1
    id = alloc[]
    start = UInt32(ast.data.position)
    last = start + ast.data.raw.span - 1
    head = ast.data.raw.head
    push!(rel, "var ast$id = new ASTNode(new SyntaxHead(\"$(summary(head))\", $(head.flags)),$parentid, $start, $last)")
    c = ast.children
    if !(c isa Nothing)
        param = join([serializeAST!(alloc, rel, i, "ast$id") for i in ast.children], ',')
        push!(rel, "ast$id.children = [$param]")
    end
    return "ast$id"
end

function serializeAST(ast::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})
    alloc = Ref{Int}(0)
    rel = String[]
    serializeAST!(alloc, rel, ast, "null")
    return join(rel, ";\n")
end


open(joinpath(@__DIR__, "www", "index.html"), write=true) do f
    s = open(@__FILE__) do x
        parseAndConvert(read(x, String);filename=@__FILE__)
    end
    write(f, wrap(s))
    open(joinpath(@__DIR__, "www", "data.js"), write=true) do f
        ast = open(@__FILE__) do x
            JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, read(x, String);filename=@__FILE__)
        end
        mm = serializeAST(ast)
        write(f, mm)
    end
    s3 = open(joinpath(@__DIR__, "www", "data.js")) do f
        s1 = read(f, String)
        s2 = open(joinpath(@__DIR__, "www", "script.js")) do f
            read(f, String)
        end
        return s2*s1
    end
    open(joinpath(@__DIR__, "www", "final.js");write=true) do f
        write(f, s3)
    end
end


