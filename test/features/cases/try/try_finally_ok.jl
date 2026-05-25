module TryFinallyOk
# try / catch / finally — exercises that the finally body is walked for
# side-effect checks but its locals don't leak.
function withcleanup(a::Int)::Int
    x::Int = 0
    try
        x = a + 1
    catch e
        x = 0
    finally
        x = x
    end
    return x
end
end
