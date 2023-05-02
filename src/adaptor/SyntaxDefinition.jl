
export SpanInfo, 
       Location, 
       JuAST,
       MutableGreenNode,
       JuASTVal,
       makeEmptyJuASTVal,
       makeConstJuASTVal

import JuliaSyntax
using ..Utility

# An literal value in the AST tree
struct JuASTVal
    # whether this represents a literal value
    isconst::Bool
    val::Any
end

# we use pair instead of tuple, because tuple is covariant
# also, pair can be accessed by p.first, p.second
const SpanInfo = Pair{UInt32, UInt32}

struct Location
    file::JuliaSyntax.SourceFile
    span::SpanInfo
end

@nocheck function makeConstJuASTVal(val::Any)
    return JuASTVal(true, val)
end

function makeEmptyJuASTVal()
    return JuASTVal(false, nothing)
end

@nocheck function getNodeVal(tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})::JuASTVal
    makeConstJuASTVal(tree.data.val)
end

# Identical to JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}
# not every green node has associated JuAST, like trivia node
mutable struct MutableGreenNodeInternal{T}
    const head::JuliaSyntax.SyntaxHead
    const span::SpanInfo
    const args::Union{Tuple{}, Vector{MutableGreenNodeInternal{T}}}
    backpointer::Union{Nothing, T}
end

# JuAST is similiar to Julia's Expr, except that it's associated with location information additionally
# we use parameterized type to get rid of recursive type definition
mutable struct JuAST
    const head::Symbol
    const args::Vector{JuAST}
    const val::JuASTVal

    const loc::Location
    # Maybe use an uninitialized field here?
    const parent::Union{JuAST, Nothing}
    const tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}
    # this field is uninitialized and assigned later
    node::MutableGreenNodeInternal{JuAST}
    function JuAST(head::Symbol, args::Vector{JuAST}, val::JuASTVal, loc::Location, mparent::Maybe{JuAST}, tree::JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData})
        return new(head, args, val, loc, mparent.val, tree)
    end
end

const MutableGreenNode = MutableGreenNodeInternal{JuAST}

function convert2line(v::Vector{Int64}, i::Int)::Pair{Int, Int}
    @assert i >= v[1] "Invalid line"
    if i >= v[lastindex(v)]
        return lastindex(v) => i - lastindex(v) + 1
    else
        l = 1
        r = lastindex(v)
        while ((r-l) > 1)
            mid = div(l + r, 2)
            if v[mid] > i
                r = mid
            elseif v[mid] == i
                return mid => i - v[mid] + 1
            else
                l = mid
            end
        end
        return l => i - v[l] + 1
    end
end

function formatLocation(loc::Location)::String
    file = loc.file
    span = loc.span
    start = convert2line(file.line_starts, Int(span[1]))
    ff = file.filename 
    if ff isa Nothing
        filename = "(none)"
    else
        filename = ff
    end
    return filename * ':' *string(start[1]) * ':'* string(start[2])
end

@nocheck function Base.show(io::IO, ast::JuAST)
    println(io, formatLocation(ast.loc))
    displayJuASTHelper(io, ast, 0)
end

@nocheck function displayJuASTHelper(io, ast::JuAST, id::Int)
    if ast.head == :literal
        s = ast.loc.span
        v = ast.val.val
        if v isa Symbol
            vstr = string(v)
        else
            vstr = Base.repr(v)
        end
        println(io, ' '^id, vstr, ' ', Int(s.first), ':', Int(s.second))
    else
        s = ast.loc.span
        println(io, ' '^id, ast.head, ' ', Int(s.first), ':', Int(s.second))
        for i in ast.args
            displayJuASTHelper(io, i, id + 1)
        end
    end
    return
end
