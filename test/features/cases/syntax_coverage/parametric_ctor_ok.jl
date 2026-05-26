module ParametricCtorOk
# Parametric constructor function names: `function Foo{K,V}(...)`. The
# call's `args[1]` is a `:curly` node, which collectQualifiedName must
# peel before reading the underlying identifier.
struct Box{K,V}
    data::Dict{K,V}
end

function Box{K,V}() where {K,V}
    return Box{K,V}(Dict{K,V}())
end

function caller()::Box{Int,String}
    return Box{Int,String}()
end
end
