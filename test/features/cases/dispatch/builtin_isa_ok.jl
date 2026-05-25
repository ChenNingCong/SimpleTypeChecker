module DispatchBuiltinIsa
# `isa`, `===`, `typeof`, etc. are Core.Builtin / Core.IntrinsicFunction.
# They lower to intrinsics — abstract args at the call site do NOT cause
# runtime method-table dispatch, so the abstract-arg check must NOT fire
# here even though `e` is `Any`.
struct MyError end
function classify(@nospecialize e)::Int
    if e isa MyError
        return 1
    elseif e === nothing
        return 2
    elseif typeof(e) === Int
        return 3
    end
    return 0
end
function caller(@nospecialize e)::Int
    return classify(e)
end
end
