using Sockets
s = listen(ip"127.0.0.1", 5000)
tcpsocket = accept(s)
meg = readavailable(tcpsocket)


function parseHTTPBody(s::String)
    m = findfirst("\r\n", s)
    if m isa Nothing
        return nothing
    end
    method = s[1:first(m)-1]
    body = findfirst("\r\n\r\n", s)
    if body isa Nothing
        return nothing
    end
    bodystr = s[last(body)+1:end]
    return method, bodystr
end