module Utility
    include("utility/utility.jl")
    include("utility/datastruct.jl")
end

module SyntaxDefinition
    include("syntax/SyntaxDefinition.jl")
end

module SyntaxAdaptor
    include("syntax/SyntaxAdaptor.jl")
    include("syntax/JSAdaptor.jl")
    include("syntax/TreeQuery.jl")
end

module Inference
    include("compiler/CompilerAdaptorTypes.jl")
    include("inference/InferenceDefinition.jl")
    include("compiler/CompilerAdaptor.jl")
    include("compiler/CompilerOps.jl")
    include("compiler/StrictRuntimeContext.jl")
    include("inference/ScopeChecking.jl")
    include("inference/InferenceErrorUtility.jl")
    include("inference/InferenceError.jl")
    include("inference/UtilityUnType.jl")
    include("inference/Inference.jl")
end

module Server
    include("server/SimpleHTTPServer.jl")
end

module API
    include("api/API.jl")
end

include("precompile.jl")
