module MyTest
include("../src/adaptor/toplevel.jl")
include("utility.jl")
runtest(SimpleTypeChecker.SyntaxDefinition, "src/adaptor/SyntaxDefinition.jl")
runtest(SimpleTypeChecker.SyntaxAdaptor, "src/adaptor/SyntaxAdaptor.jl")
runtest(SimpleTypeChecker.Inference, "src/adaptor/InferenceError.jl")
runtest(SimpleTypeChecker.Inference, "src/adaptor/Inference.jl")
runtest(SimpleTypeChecker.Inference, "src/adaptor/JuExprAdaptor.jl")
runtest(SimpleTypeChecker.Inference, "src/adaptor/JuExprValidator.jl")
end