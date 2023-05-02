module CaseTest
    include("case.jl")
    include("../src/adaptor/toplevel.jl")
    include("utility.jl")
    runtest(CaseTest, "test/case.jl")
end