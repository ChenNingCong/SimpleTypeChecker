module DispatchCallableStruct
# Exercises the non-singleton-callable branch in inferReturnType(ctx, sig):
# a callable struct with fields has a non-singleton parameter type, so the
# 3-arity dispatcher falls through to `else` and passes `nothing` as the f
# placeholder. The call should still resolve statically and not flag.
struct Counter
    n::Int
end
(c::Counter)(x::Int)::Int = c.n + x

function use()::Int
    c = Counter(10)
    return c(5)
end
end
