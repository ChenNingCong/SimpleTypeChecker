export locateNode
# Also used in JSAdaptor
const NewlineWsKind = convert(JuliaSyntax.Kind, "NewlineWs")
const WhitespaceKind = convert(JuliaSyntax.Kind, "Whitespace")
const CommentKind = convert(JuliaSyntax.Kind, "Comment")

function stringfy(x)
    return "$(Int(x.first)):$(Int(x.second))"
end

# This file provides a function such that given a span and a top-level ast,
# we can narrow the range of span to a specific subtree in the ast
function treeLookup(node::MutableGreenNode, targetSpan::SpanInfo)::Union{Nothing, MutableGreenNode}
    #println("Lookup $(stringfy(targetSpan)) in $(node.head), $(node.backpointer) $(stringfy(node.span))")
    span = node.span
    if (targetSpan.first > span.second || targetSpan.second < span.first)
        return nothing
    elseif targetSpan == span && !JuliaSyntax.is_trivia(node.head)
        return node
    else
        # inclusion
        if span.first <= targetSpan.first && targetSpan.second <= span.second
            args = node.args
            if JuliaSyntax.is_trivia(node.head)
                # a trivia, we alreadys give up on trivia
                return nothing
            elseif args isa Tuple{}
                # no children, stop here
                return node
            else
                for i in args
                    rel = treeLookup(i, targetSpan)
                    if rel isa Nothing
                        continue
                    else
                        return rel
                    end
                end
            end
            # inclusion, but not intersect with any element, a trivia
            # note, we need to handle trivia carefully
            return node
        else
            # partial intersection, shouldn't happend
            return nothing
        end
    end
end

function locateNode(node::MutableGreenNode, targetSpan::SpanInfo)::Union{Nothing, JuAST}
    val = treeLookup(node, targetSpan)
    if val isa Nothing
        return nothing
    end
    kind = val.head
    if kind == NewlineWsKind || kind == WhitespaceKind || kind == CommentKind
        # do nothing for these trivia
        return nothing
    else
        return val.backpointer
    end
end