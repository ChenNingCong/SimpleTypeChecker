SimpleTypeChecker is an experimental Julia package designed to enforce C-style programming in Julia language.

# Usage
```
SimpleTypeChecker.API.runtest(mod::Core.Module, filename::String)
SimpleTypeChecker.API.@nocheck fun
```
SimpleTypeChecker provides a API to check all the functions in a file. To use this function, firstly import/include the module you want to check, then call `SimpleTypeChecker.API.runtest(mod, filename)`. Here mod is the module you just evaled and filename is the filepath (*absolute path*) where the module is defined.

If you have a function that is highly dynamic or uses some features that SimpleTypeChecker doesn't support, and you are certain the function is type stable, then you can use `SimpleTypeChecker.API.@nocheck fun` to skip the checking of that function.

Currently, only functions with concrete type annotation can be checked. So function signatures like `sum(x::AbstractArray)` and `println(io::IO, x)` will create a lot of false positives, because the abstract types are used for type inference. In the future, we will support check of individual specializations of each method, with a interface similar to `precompile(f, tts)`.

# Demo
1. SimpleTypeChecker is checked against itself, the following code checks almost all the codes of SimpleTypeChecker (some codes interact with Julia's compiler so they simply can't be statically-typed)
```
import SimpleTypeChecker
# firstly we get the package directory of SimpleTypeChecker
const path = abspath(joinpath(splitdir(pathof(SimpleTypeChecker))[1], ".."))
SimpleTypeChecker.API.runtest(SimpleTypeChecker.SyntaxDefinition, joinpath(path, "src/adaptor/SyntaxDefinition.jl"))
SimpleTypeChecker.API.runtest(SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/SyntaxAdaptor.jl"))
SimpleTypeChecker.API.runtest(SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceError.jl"))
SimpleTypeChecker.API.runtest(SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/Inference.jl"))
SimpleTypeChecker.API.runtest(SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/JuExprAdaptor.jl"))
SimpleTypeChecker.API.runtest(SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/JuExprValidator.jl"))
```
2. You can check the `test/case.jl` of SimpleTypeChecker's test cases, which includes some common program errors.
```
import SimpleTypeChecker
# firstly we get the package directory of SimpleTypeChecker
const path = abspath(joinpath(splitdir(pathof(SimpleTypeChecker))[1], ".."))
const testpath = abspath(joinpath(path, "test/case.jl"))
include(testpath)
SimpleTypeChecker.API.runtest(CaseTest, testpath)
```

# Limitation
WIP
