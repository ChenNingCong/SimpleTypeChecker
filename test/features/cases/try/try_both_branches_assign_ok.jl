module TryBothBranchesAssignOk
# Variable assigned in BOTH try and catch — must be treated as
# initialized after the try/catch block. Mirrors how `inferMacroCall`
# uses the pattern.
function classify(x::Int)::Int
    local result::Int
    try
        result = div(100, x)
    catch e
        result = -1
    end
    return result
end
end
