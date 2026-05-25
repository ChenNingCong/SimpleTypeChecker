module DispatchPrintln
# `println(::String)` resolves through `Base.print(::IO, ...)` where IO is
# abstract. The :call survives optimization → dynamic dispatch deep in the
# chain. Expected to flag InferenceErrorTopLevelDispatch at this site.
function bad()::Nothing
    println("hello")
    return nothing
end
end
