# This file converts JuliaSyntax.GreenNode and JuliaSyntax.TreeNode to mutable structs
# so that it's possible to establish pointer equality and convert the result to js object

export parseJuAST

const JuliaSyntax = Base.JuliaSyntax
# Maybe is used only in this module, we don't expose it to other modules
using ..SyntaxDefinition
import ..SyntaxDefinition.formatLocation
using ..Utility

function attachNodeToJuAST(ast::JuAST, node::MutableGreenNode)::Nothing
    if !checkField(ast, :node)
        error("Already attached with a GreenNode")
    end
    ast.node = node
    return
end

struct GreenNodeConvertResult
    node::MutableGreenNode
    spanMapping::Dict{SpanInfo, Vector{MutableGreenNode}}
end

# Convert GreenNode to its mutable counterpart
function convert2MutableGreenNode(ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})
    pos = Base.RefValue{UInt32}(0)
    spanMapping = Dict{SpanInfo, Vector{MutableGreenNode}}()
    node = convert2MutableGreenNode!(spanMapping, pos, ast)
    return GreenNodeConvertResult(node, spanMapping)
end

# We identity every non-leaf GreenNode with its span
function convert2MutableGreenNode!(spanMapping::Dict{SpanInfo, Vector{MutableGreenNode}},
                                   pos::Base.RefValue{UInt32},
                                   ast::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead})::MutableGreenNode
    args = ast.children
    span = SpanInfo(pos[] + 1, pos[] + ast.span)
    if args isa Nothing
        node = MutableGreenNode(ast.head, span, Tuple{}(), nothing)
        pos[] = pos[] + ast.span
    else
        argnodes = Vector{MutableGreenNode}(undef, length(args))
        for i in 1:length(argnodes)
            argnodes[i] = convert2MutableGreenNode!(spanMapping, pos, args[i])
        end
        node = MutableGreenNode(ast.head, span, argnodes, nothing)
    end
    if haskey(spanMapping, span)
        push!(spanMapping[span], node)
    else
        spanMapping[span] = MutableGreenNode[node]
    end
    return node
end

function getData(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})::JuliaSyntax.SyntaxData
    data = tree.data
    if data isa JuliaSyntax.SyntaxData
        return data
    else
        error("JuliaSyntax.TreeNode has no associated green node")
    end
end

function getNodeVal(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})::JuASTVal
    d = tree.data
    if d isa JuliaSyntax.SyntaxData
        return makeConstJuASTVal(d.val)
    else
        error("TreeNode has no associated SyntaxData")
    end
end

function untokenize(head::JuliaSyntax.SyntaxHead)::String
    JuliaSyntax.untokenize(head;include_flag_suff=true)
end

# Convert TreeNode to JuAST
# Ordering of JuAST and JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData} is different
# For example, in infix-operator call, parameter order is swapped
# maintain a mapping here
function convert2JuAST!(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}, 
                        parent::Maybe{JuAST})::JuAST
    data = getData(tree)
    start = UInt(data.position)
    span = SpanInfo(start, start + UInt(data.raw.span)-1)
    loc = Location(data.source, span)
    nchild_ = tree.children
    if nchild_ isa Nothing
        # this is a literal, like number, symbol or string
        v = getNodeVal(tree)
        # if the value is a symbol, then we represent it by an identifier instead of a symbol
        vv = v.val 
        if vv isa Symbol
            ast = JuAST(:identifier, UInt16(0), JuAST[], v, loc, parent, tree)
        else
            ast = JuAST(:literal, UInt16(0), JuAST[], v, loc, parent, tree)
        end
        return ast
    end
    nchild = getChildren(tree)
    head = data.raw.head
    kind = Int64(reinterpret(Int16, head.kind))
    flag = head.flags
    # JuliaSyntax stores a numeric value (e.g. the concatenation dimension
    # for `ncat` / `typed_ncat` — `[A;;B]` packs dim=2 as flag 512,
    # `[A;;;B]` packs dim=3 as flag 768) in the upper byte of `flag`.
    # That's not a boolean flag bit that we need to whitelist, so strip
    # bits 8-15 before validating. The full `flag` is still handed to
    # JuAST below so downstream consumers can read the dim.
    flag_bool = flag & 0x00FF
    if (flag_bool & ~JuliaSyntax.EMPTY_FLAGS &
                    ~JuliaSyntax.INFIX_FLAG  &
                    ~(1<<5) & # MUTABLE_FLAG
                    ~(1<<7) & # assignment
        ~(1<<6) & # string unescaped (unimportant for type inference)
        ~(1<<4) &
        # string macro
        ~JuliaSyntax.DOTOP_FLAG & # dotted op
        ~0x0040) != 0 # ! call
        error("Bad flag : $(formatLocation(loc)) $(head.kind) : $flag $(untokenize(head))")
    end
    # TODO : we need to handle more strange syntax here...
    shead = Symbol(JuliaSyntax._kind_int_to_str[reinterpret(UInt16, head.kind)])
    children = Vector{JuAST}(undef, length(nchild))
    ast = JuAST(shead, flag, children, makeEmptyJuASTVal(), loc, parent, tree)
    for i in 1:length(nchild)
        children[i] =  convert2JuAST!(nchild[i], Maybe{JuAST}(ast))
    end
    # TODO : we need to handle some other cases there
    # basically we need to convert the order to Expr
    if (flag & JuliaSyntax.INFIX_FLAG !=0) && (shead == :call || shead == :dotcall)
        # swap the ordering of first children
        tmp = children[1]
        children[1] = children[2]
        children[2] = tmp
    end
    return ast
end

function getChildren(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})::Vector{JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}}
    child = tree.children
    if child isa Vector{JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}}
        return child
    else
        error("Mismatched shape : green node and ast should both have no children here")
    end
end

function fastCompareGreenNode(node::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}, mnode::MutableGreenNode)::Bool
    l = mnode.span.second - mnode.span.first + 1
    if node.span != l
        return false
    end
    if node.head != mnode.head
        return false
    end
    return true
end

function compareGreenNode(node::JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}, mnode::MutableGreenNode)::Bool
    @warn "Slow Green Node comparing is used!"
    if !fastCompareGreenNode(node, mnode)
        return false
    end
    nchild = node.children
    mchild = mnode.args
    if nchild isa Nothing
        if mchild isa Tuple{}
            return true
        else
            return false
        end
    else
        if mchild isa Tuple{}
            return false
        else
            if length(nchild) != length(mchild)
                return false
            end
            for i in 1:length(nchild)
                if !compareGreenNode(nchild[i], mchild[i])
                    return false
                end
            end
        end
        return true
    end
    # const args::Union{Tuple{}, Vector{MutableGreenNodeInternal{T}}}
end

function matchGreenNode(ast::JuAST, nodes::Vector{MutableGreenNode})::MutableGreenNode
    if length(nodes) == 1
        return nodes[1]
    else
        candidate = MutableGreenNode[]
        data = getData(ast.tree)
        for i in nodes
            if fastCompareGreenNode(data.raw, i)
                push!(candidate, i)
            end
        end 
        if length(candidate) == 1
            return candidate[1]
        elseif length(candidate) == 0
            # TODO : this is possible for try-catch without finally branch, where a literal false is generated and has no back pointer
            @debug "No extract mapping"
            return nodes[1]
        else
            num = 0
            index = -1
            for i in 1:length(candidate)
                if compareGreenNode(data.raw, candidate[i])
                    num = num + 1
                    index = i
                end
                if num > 1
                    error("More than one matching")
                end
            end 
            if num == 0
                error("No exact matching")
            elseif num == 1
                return candidate[index]
            end
        end
        return candidate[1]
    end
end

function forwardPointer(spanMapping::Dict{SpanInfo, Vector{MutableGreenNode}},
                        ast::JuAST)
    span = ast.loc.span
    if haskey(spanMapping, span)
        node = matchGreenNode(ast, spanMapping[span])
        ast.node = node
        node.backpointer = ast
    else
        error("AST has no matching green node")
    end
    for i in ast.args
        forwardPointer(spanMapping, i)
    end
end

function checkField(ast::JuAST, field::Symbol)::Bool
    isdefined(ast, :node)::Bool
end

function validatePointer(ast::JuAST)::Bool
    span = ast.loc.span # some value is invalid, there is no need to map them
    if span.second < span.first
        return true
    end
    if !checkField(ast, :node)
        return false
    end
    bp = ast.node.backpointer
    if bp isa Nothing
        return false
    elseif bp != ast
        return false
    end
    for i in ast.args
        if !validatePointer(i)
            return false
        end
    end
    return true
end

function constructJuAST(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})::JuAST
    data = getData(tree)
    ast = convert2JuAST!(tree, Maybe{JuAST}(nothing))
    crel = convert2MutableGreenNode(data.raw)
    forwardPointer(crel.spanMapping, ast)
    if !validatePointer(ast)
        error("Internal implementation is incorrect")
    end
    return ast
end

function parseJuAST(str::String, filename::String)::JuAST
    tree = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, str;filename)
    ast = constructJuAST(tree)
    return ast
end
