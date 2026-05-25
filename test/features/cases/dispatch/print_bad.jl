module DispatchPrint
# Calling `Base.print(::String)` directly — still hits abstract stdout.
function bad()::Nothing
    print("hi")
    return nothing
end
end
