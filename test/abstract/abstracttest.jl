module MyTest
import SimpleTypeChecker
include("abstract.jl")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, AbstractTest, abspath("test/abstract.jl"))
SimpleTypeChecker.API.runCheck!(ctx)

end