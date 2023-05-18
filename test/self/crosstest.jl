module MyTest

import SimpleTypeChecker

module Wrapper
    include("../src/SimpleTypeChecker.jl")
end

const path = abspath(".")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxDefinition, joinpath(path, "src/adaptor/SyntaxDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/SyntaxAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceError.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/Inference.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/JuExprAdaptor.jl"))
#SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/JuExprValidator.jl"))
SimpleTypeChecker.API.runCheck!(ctx)

end