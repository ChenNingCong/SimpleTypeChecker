module DispatchKwargOk
# Kwarg call with a concrete user-defined callee. Exercises the kwcall
# sig-rewrite path in getMethodMatches and confirms the dyn-dispatch
# cache lookup uses the same key.
scale(x::Int; factor::Int = 2)::Int = x * factor

function use()::Int
    return scale(7; factor = 3)
end
end
