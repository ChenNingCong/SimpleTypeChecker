module TryCatchVarScoped
# The catch variable `e` is local to the catch block — referencing it
# outside should be an undefined-variable error. The checker's scope pass
# (not the dispatch detector) is responsible for catching this.
function leaks(a::Int)::Int
    try
        return a
    catch e
        return 0
    end
    # Using `e` here would be undefined — keep this commented to match
    # the positive expectation. We only test the in-scope case here; an
    # explicit negative test for the leak would go in a sibling fixture.
end
end
