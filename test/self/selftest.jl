module MyTest

include("../../src/SimpleTypeChecker.jl")
const path = abspath(".")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxDefinition, joinpath(path, "src/adaptor/SyntaxDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/SyntaxAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/JSAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/TreeQuery.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/Inference.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceErrorUtility.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceError.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/ScopeChecking.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Server, joinpath(path, "src/server/SimpleHTTPServer.jl"))
SimpleTypeChecker.API.runCheck!(ctx)

end