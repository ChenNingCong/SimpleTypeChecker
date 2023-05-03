SimpleTypeChecker is an experimental Julia package designed to enforce C-style programming in Julia language. Note : it won't save you if your codes already contain a lot of messy dynamic dispatch. It's more appropriate for developing new codes from scratch.

# Installation
```julia
pkg> add https://github.com/ChenNingCong/SimpleTypeChecker
```

# Usage
```julia
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, mod, filepath)
SimpleTypeChecker.API.runCheck!(ctx)
SimpleTypeChecker.API.@nocheck fun
SimpleTypeChecker.check(ctx, f, tt)
```
SimpleTypeChecker provides several APIs to check all the functions in a file. To use this function, firstly import/include the module you want to check, then call `ctx = SimpleTypeChecker.Inference.GlobalContext()` to construct a context for type inference. Use `SimpleTypeChecker.API.addFile!(ctx, mod, filepath)` to add all the files you want to check into the context. Here `mod` is the module you just evaled and `filepath` is the filepath (*absolute path*) where the module is defined. Finally call `runCheck!` to check the context 

If you have a function that is highly dynamic or uses some features that SimpleTypeChecker doesn't support, and you are certain the function is type stable, then you can use `SimpleTypeChecker.API.@nocheck fun` to skip the checking of that particular function.

Currently, only functions with concrete type annotation can be checked. If you want to check individual specializations like `sum(x::AbstractArray)` and `println(io::IO, x)`, use `SimpleTypeChecker.check(ctx, f, tt)`. `SimpleTypeChecker.check` accepts parameters like `code_warntype` and `code_typed`, `SimpleTypeChecker.check(ctx, sin, (Float64,))`. If you find this too complicated, then you can create a `main` function and put all the specializations in that `main` function. `SimpleTypeChecker` will recur into the subprocedure calls automatically.

# Demo
1. SimpleTypeChecker is checked against itself, the following code checks almost all the codes of SimpleTypeChecker (some codes interact with Julia's compiler so they simply can't be statically-typed)
```julia
import SimpleTypeChecker
# firstly we get the package directory of SimpleTypeChecker
const path = abspath(joinpath(splitdir(pathof(SimpleTypeChecker))[1], ".."))
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxDefinition, joinpath(path, "src/adaptor/SyntaxDefinition.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.SyntaxAdaptor, joinpath(path, "src/adaptor/SyntaxAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/InferenceError.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/Inference.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/JuExprAdaptor.jl"))
SimpleTypeChecker.API.addFile!(ctx, SimpleTypeChecker.Inference, joinpath(path, "src/adaptor/JuExprValidator.jl"))
SimpleTypeChecker.API.runCheck!(ctx)
```
2. You can check the `test/case.jl` of SimpleTypeChecker's test cases, which includes some common program errors.
```julia
import SimpleTypeChecker
# firstly we get the package directory of SimpleTypeChecker
const path = abspath(joinpath(splitdir(pathof(SimpleTypeChecker))[1], ".."))
const testpath = abspath(joinpath(path, "test/case.jl"))
include(testpath)
ctx = SimpleTypeChecker.Inference.GlobalContext()
SimpleTypeChecker.API.addFile!(ctx, CaseTest, joinpath(path, testpath))
SimpleTypeChecker.API.runCheck!(ctx)
```

# Internal
The idea is simple : SimpleTypeChecker performs type inference on Julia's AST (instead of lowered IR) and also rely on Julia's type inferencer
1. SimpleTypeChecker reads the method definitions from the file you provide, and use `JuliaSyntax` to parse the file and construct a loseless AST (a.k.a AST with precise location information)
2. For each method AST `f`, SimpleTypeCheck walk through the AST and check common errors. If it encounters a function calls to another function `g`, then it calls Julia's builtin type inferencer to get the return type of `g`

Note : we need to rely on Julia's builtin type inferencer, because we don't have AST for every method definition (especially those in Base library). Also, inference for recursive function is generally undeciable, adding recursive check in SimpleTypeChecker will complicate a lot of thing.

# Limitation
Currently there are many limitations, like we don't support destruct assignment (`(x, v) = k`).
Some of them are temporal and will be removed in the future, but at least the following limitations are permanent. They are critial parts of SimpleTypeChecker's design and can't be simply removed by clever engineer efforts.

In summary, these limitations are the prices in exchange for a better error reporting.

---
>Function calls with abstract parameters (including `Union{...}, Any, Integer, where`) are unconditionally disallowed, whether there is only one matching method, whether the function is a commonly used one (like `==(Any, Any)`), etc.

Reason:
Sometimes abstract calls are intended, like
```julia
struct A
val::Union{Int, Nothing}
end
# a.val isa Union, an abstract type
copy(a::A) = A(a.val)
```
So there is simply no way to tell whether an abstract call is intended or not. Then the only solution is to ban abstract calls everywhere.

Workaround:
Wrap the abstract values in a struct. Only pass the wrapper and manipulate the wrapper with interface methods (In the future, SimpleTypeChecker may directly support `MLStyle.jl`'s pattern matching and algebraic types, so we don't need to wrap and dewrap them manually).

---
>Limited type calculations and constant propagation, no heap analysis, no complicated type narrowing (only a single `isa`)

Reason:

Such analyses require nontrivial bidirectional flow analysis. One immediate consequence of such complicated analyses is that it's hard to precisely locate where the error is. Codebases in Julia are generally loosely typed. If there is an error somewhere, the this error may gradually propagate to other codes. That's why it's painful to debug type instability, because you need to recurse into a long function chains and context to seek the real source of bug.

Workaround:

Do calculations in short unchecked functions

---
> Unable to distinguish errors caused by `::` and implicit convertion, like `push!([1,2,3], "1")`.

Reason:

`push!([1,2,3], "1")` is a totally valid function call. Honestly speaking, should this be considered as compile time error at all?

Workaround:
Almost impossible to workaround. Currently my thought:
1. Use a whitelist. Only the functions in the whitelist can return `Union{}`, like `error`.
2. Check functions recursively and only trust Julia's compiler when we don't have AST (this solve the problem if you have a return type assertion on a method, but the method is inferred as `Union{}`)
3. Manually provide *interface* definition for commonly used container in Base library. (Much like typeclass in Haskell, if type parameters don't satisfy the constraint, then immediately raise error)

---

> Scope of variable in if-block and interpretation of `local x::Int` is vague.

Reason:
Julia has no scoped if block, which raises some interesting problems of variable in if-block. Should the following case be treated as type instability?
```julia
function f(y::Bool)
    if y
        x = 1
        print(x)
    else
        x = 1.0
        print(x)
    end
end
```

If we perform basic-block specializations and view two x as individual unrelated copies of value 1 and 1.0, then this code is type stable. SimpleTypeChecker can actually detect it and allow it (as long as if x is not used in following branch), but I choose to ban this case. So automatically merge of different types are prohibited. This is to prevent the following case:

```julia
function f(y::Bool)
    if y
        x = 1
        println(x)
        # many more codes here...
    elseif somecond
        # ... 
    else
        # accidentally name a string as x
        x = "1"
    end
    # more code here...
    # abstract call !!!

    # error reports here, too late
    print(x)
end
```

What if I want to construct a Union type? then use a typed slot:
```julia
function f(y::Bool)
    local x::Union{Int, Nothing}
    if y
        x = 1
    else
        x = nothing
    end
end
```

