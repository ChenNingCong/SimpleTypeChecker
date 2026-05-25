module TryNoCatchVarOk
# `catch` without binding a variable — args[2] is :literal(nothing).
function tolerant(a::Int)::Int
    try
        return a + 1
    catch
        return 0
    end
end
end
