module MyTest

include("../../src/SimpleTypeChecker.jl")
include("case.jl")
testpath = abspath(joinpath(@__DIR__, "case.jl"))
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, CaseTest, testpath)
SimpleTypeChecker.API.runCheck!(ctx)

end