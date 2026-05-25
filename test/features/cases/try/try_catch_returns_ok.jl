module TryCatchReturnsOk
# When the catch ALWAYS exits (return / rethrow), the var assigned at
# the end of try is initialized after the try/catch — because reaching
# code past the catch implies try completed without throwing.
function classify(x::Int)::Int
    local result::Int
    try
        result = div(100, x)
    catch e
        return -1
    end
    return result
end
end
