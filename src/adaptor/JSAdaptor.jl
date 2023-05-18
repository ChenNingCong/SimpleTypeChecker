export parseAndConstructHTML

import ..SyntaxDefinition.formatLocation
import Base.RefValue

const wrapperString = open(joinpath(@__DIR__, "../www/index_template.html")) do f
    read(f, String)
end

@nocheck function parseAndConstructHTML(raw::String, filename::String, outdir::String)
    ast = parseJuAST(raw, filename)
    html = wrap(constructHTML(raw, ast))
    js = serializeAST(ast.node)
    htmlf = open(joinpath(outdir, "index.html"), write = true)
    write(htmlf, html)
    close(htmlf)
    jsf = open(joinpath(outdir, "data.js"), write = true)
    write(jsf, js)
    close(jsf)
    return ast
end

function constructHTML(raw::String, ast::JuAST)
    rel = convertGreenNode(raw, ast.node)
    # we can't join them with '\n', which might introduce a 4px space between spans
    join(rel)
end

function _helper(x::SubString)::String
    escapseNewlineWs(String(x))
end

function makeCodeSegment(raw::String, pos::RefValue{Int}, ast::MutableGreenNode)
    head = ast.head
    kind = head.kind
    start = pos[] + 1
    spanlen = ast.span.second - ast.span.first + 1
    last = pos[] + spanlen
    data = raw[start:last]
    pos[] = pos[] + spanlen
    range = "start = $start last = $last"
    if kind == NewlineWsKind
        s = join(map(_helper, split(data, '\n')), """<span class="linebreak"></span>""")
        return s
    elseif kind == WhitespaceKind
        data = escapseEmtpySpace(data)
        return """<span class="secondary">$data</span>"""
    else
        data = htmlEscape(data)
        if kind == CommentKind
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

function convertGreenNode!(rel, pos::RefValue{Int}, raw::String, ast::MutableGreenNode)
    args = ast.args
    if args isa Tuple{}
        push!(rel, makeCodeSegment(raw, pos, ast))
    else
        # a non-terminal
        for i in args
            convertGreenNode!(rel, pos, raw, i)
        end
    end
end

function convertGreenNode(raw::String, ast::MutableGreenNode)
    rel = String[]
    pos::RefValue{Int} = RefValue{Int}(0)
    convertGreenNode!(rel, pos, raw, ast)
    return rel
end

function wrap(x::String)
    return replace(wrapperString, "__TEMPLATE_PLACEHOLDER__"=>x)
end


function serializeAST!(alloc::RefValue{Int}, rel::Vector{String}, ast::MutableGreenNode, parentid::String)::String
    alloc[] = alloc[] + 1
    id = alloc[]
    span = ast.span
    start = span.first
    last = span.second
    head = ast.head
    push!(rel, "var ast$id = new ASTNode(new SyntaxHead(\"$(summary(head))\", $(head.flags)),$parentid, $start, $last)")
    c = ast.args
    if c isa Tuple{}
        return "ast$id"
    else
        strs = similar(c, String)
        for i in eachindex(strs)
            strs[i] = serializeAST!(alloc, rel, c[i], "ast$id")
        end
        param = join(strs, ',')
        push!(rel, "ast$id.args = [$param]")
        return "ast$id"
    end
end

function serializeAST(ast::MutableGreenNode)
    alloc = RefValue{Int}(0)
    rel = String[]
    serializeAST!(alloc, rel, ast, "null")
    return join(rel, ";\n")
end

