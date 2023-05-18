module Utility
    include("utility.jl")
    include("datastruct.jl")
end

module SyntaxDefinition
    include("SyntaxDefinition.jl")
end

module SyntaxAdaptor
    include("SyntaxAdaptor.jl")
    include("JSAdaptor.jl")
    include("TreeQuery.jl")
end

module Inference
    include("InferenceDefinition.jl")
    #include("DerivedExpr.jl")
    #include("JuExprAdaptor.jl")
    include("ScopeChecking.jl")
    include("InferenceErrorUtility.jl")
    include("InferenceError.jl")
    include("UtilityUnType.jl")
    include("Inference.jl")
    #include("JuExprValidator.jl")
end

module Server
    include("../server/SimpleHTTPServer.jl")
end

module API
    include("API.jl")
end
include("precompile.jl")