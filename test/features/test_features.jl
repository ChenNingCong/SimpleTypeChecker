# Language-features test orchestrator. Each feature gets its own file; this
# entry point pulls shared helpers in once and then includes them.
#
# When you add a new language feature, create a corresponding `test_<name>.jl`
# in this directory and append an include below — that keeps each file small
# and focused, and makes it easier to run a single feature in isolation
# (`julia --project=. -e 'using Test; include("test/features/helpers.jl"); include("test/features/test_dispatch.jl")'`).

include("helpers.jl")

@testset "Features" begin
    include("test_subtype.jl")
    include("test_assertion.jl")
    include("test_lambda.jl")
    include("test_macros.jl")
    include("test_arrays.jl")
    include("test_try.jl")
    include("test_dispatch.jl")
end
