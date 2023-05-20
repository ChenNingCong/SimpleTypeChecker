module MyTest

import SimpleTypeChecker

module Wrapper
    include("../src/SimpleTypeChecker.jl")
end

const path = abspath(".")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.SyntaxDefinition, joinpath(path, "src/adaptor/SyntaxDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/SyntaxAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/JSAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/TreeQuery.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/Inference.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceErrorUtility.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceError.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/ScopeChecking.jl"))
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Server, joinpath(path, "src/server/SimpleHTTPServer.jl"))
SimpleTypeChecker.API.runCheck!(ctx)
end
#include("eval.jl")