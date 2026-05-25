module MyTest

include("../../src/SimpleTypeChecker.jl")
include("macro.jl")
const path = abspath(".")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.Inference.getGlobalExpander(ctx)
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxDefinition, joinpath(path, "src/syntax/SyntaxDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/syntax/SyntaxAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/syntax/JSAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/syntax/TreeQuery.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/inference/InferenceDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/inference/Inference.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/inference/InferenceErrorUtility.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/inference/InferenceError.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/inference/ScopeChecking.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Server, joinpath(path, "src/server/SimpleHTTPServer.jl"))
SimpleTypeChecker.API.runCheck!(ctx)

end