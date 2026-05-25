module DispatchShowAbstractArg
# Demonstrates precedence: `stdout` is typed as the abstract `IO`, so the
# user's call `show(stdout, 42)` has an abstract arg at this site. The
# abstract-arg check (InferenceErrorFunCallArgs) fires FIRST and the
# top-level dyn-dispatch check is never reached at this site — even though
# internally show's call tree is full of dynamic dispatch.
function bad()::Nothing
    show(stdout, 42)
    return nothing
end
end
