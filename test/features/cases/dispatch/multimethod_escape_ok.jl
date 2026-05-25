module DispatchMultimethodEscape
# `Base.rethrow` shape: one zero-arg method + one 1-arg @nospecialize
# method. The 1-arg call site must be accepted as an escape hatch even
# though the function has 2 methods total — at the call's arity (2:
# function slot + 1 user arg) only the @nospecialize method matches.
function reraise()
    return nothing
end
function reraise(@nospecialize e)
    return e
end
function caller(@nospecialize e)
    return reraise(e)
end
end
