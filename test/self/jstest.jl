module MyTest

import SimpleTypeChecker
module Wrapper
    include("../../src/SimpleTypeChecker.jl")
end
const path = abspath(".")
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, Wrapper.SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/JSAdaptor.jl"))
SimpleTypeChecker.API.runCheck!(ctx)

testfilepath = joinpath(@__DIR__, "simpleinfer.jl")
str = open(x->read(x,String), testfilepath)
Wrapper.SimpleTypeChecker.SyntaxAdaptor.parseAndConstructHTML(str, path, joinpath(@__DIR__, "../../src/", "www"))


end