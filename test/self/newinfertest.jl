module MyTest

import SimpleTypeChecker

module Wrapper
    include("../src/SimpleTypeChecker.jl")
end

const path = abspath(".")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.Inference.Wrapper, joinpath(path, "src/adaptor/newinfer.jl"))
SimpleTypeChecker.API.runCheck!(ctx)

end