import ..SyntaxAdaptor: locateNode
using ..SyntaxDefinition
import ..Utility: @nocheck
import ..Inference: FlowNode
# TODO : remove this in the future
unreachable() = error()
# a really simple HTTP server
# no TLS, and only support HTTP 1.0
using Sockets

struct ParseError <: Exception
    msg::String
end

function parseHTTPBody(s::String)
    m = findfirst("\r\n", s)
    if m isa Nothing
        Base.throw(ParseError("Failed to parse method from HTTP request"))
        unreachable()
    end
    method = s[1:first(m)-1]
    body = findfirst("\r\n\r\n", s)
    if body isa Nothing
        Base.throw(ParseError("Failed to parse HTTP body from HTTP request"))
        unreachable()
    end
    bodystr = s[last(body)+1:end]
    return method => bodystr
end

function wrapResponseHeader(str::String)
    header = """
    HTTP/1.1 200 OK
    Content-Type: text/plain ;charset=utf-8
    Access-Control-Allow-Origin: *
    Connection: Closed
    Content-Length: $(sizeof(str))

    """
    return replace(header, '\n'=> "\r\n") * str
end

struct QueryFunctor
    f::Function
    ast::JuAST
    flowMapping::Dict{JuAST, Vector{FlowNode}}
end

@nocheck function query(f::QueryFunctor, method::String, body::String)::String
    greennode = f.ast.node
    params = split(body, ' ')
    if params[1] == "locateNode"
        node = locateNode(greennode, SpanInfo(Base.parse(Int, params[2]), Base.parse(Int, params[3])))
        if node isa Nothing
            return "nothing"
        else
            if haskey(f.flowMapping, node)
                flownode = f.flowMapping[node]
            else
                flownode = nothing
            end
            f.f(node, flownode)
        end
    else
        return "Unsupported call"
    end
end

@nocheck function handleQuery(f::QueryFunctor)
    println("Launching LSP server")
    s = listen(Sockets.IPv4("127.0.0.1"), 3467)
    # process message serially
    # maybe need to fix this
    try
        while true
            tcpsocket = accept(s)
            println("  Receving a request")
            meg = readavailable(tcpsocket)
            try
                p = parseHTTPBody(String(meg))
                method = p.first
                body = p.second
                value = query(f, method, body)
                resp = wrapResponseHeader(value)
                write(tcpsocket, resp)
            catch e
                if e isa ParseError
                    println("  Failed to parse the request")
                else
                    # capture by outside try-catch
                    rethrow(e)
                end
            finally
                close(tcpsocket)
            end
            println("  End processing the request\n")
        end
    catch e
        if e isa InterruptException
            println("Closing the server, exit normally")
        else
            println("Closing the server, exit abnormally")
            rethrow(e)
        end
    finally
        close(s)
    end
end
