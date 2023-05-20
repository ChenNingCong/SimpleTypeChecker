module CaseTest

const ExpectError = Dict{Any, Any}()
# check literal type
function f1()::Float64
    return 1
end

# assign with different type is disallowed
function f2()
    x = 1
    x = 2
    x = 1.0
end

# call with union type is disallowed
function f3(x::Union{Nothing, Int})
    print(x)
end

# call with abstract type is disallowed
function f4(x::Ref{Int})
    print(x)
end

# x should be non-constant
function f5(y::Bool)
    if y
        x = 1
    else
        x = 2
    end
    print(x)
end

# x should be constant
function f6(y::Bool)
    if y
        x = 1
    else
        x = 1
    end
    return Array{Int, x}
end

# non-constant apply is disallowed
function f7(y::Bool)
    if y
        x = 1
    else
        x = 2
    end
    return Array{Int, x}
end

# join of union type is disallowed
function f8(y::Bool)
    if y
        x = 1.0
    else
        x = 1
    end
end

# unreachable can narrow type
function f9(x::Union{Int, Nothing})
    if x isa Int
        print(x)
        error()
    else
    end
    print(x)
end

# unreachable can narrow type
function f10(x::Union{Int, Nothing})
    if x isa Int
        print(x)
        error()
    end
    print(x)
end

function f11(y::Int)
    if y > 0
        x = 1
        error()
    elseif y > 1
        # x is not defined in this branch
        z = 3
    else
        x = 3
        error()
    end
    print(x)
end

# assignment in unreachable branch is still checked
function f12(y::Int)
    if y > 0
        x = 1
    else
        x = 1.0
        error()
    end
    print(x)
end

function f13()
    error()
    return 1
end

function f14()
    return xxxxxxxxxxx
end

function f15(x::Vector{Int})
    y = similar(x, Vector{eltype(x)})
    z = y[1][1]
    print(z)
end

function f16(x::Vector{Float64})
    y = Dict{Dict{eltype(x), Vector{eltype(x)}}, Int}
    z = Dict{y, y}()
    print(z)
end

function f17(x::Vector{Int})
    y = 3
    t = 3
    if y > 0
        for i in x
            t = 1
        end
        print(t)
    else
        t = 4
    end
    print(t)
end

function f18(x::Int)::Union{Int, String}
    if x == 1
        z = 1
    else
        y = ""
    end
end

function f19(y::Bool)
    local x::Union{Int, Float64}
    if y
        x = 1
    else
        x = 1.0
    end
    return x
end

function f20(y::Bool)
    local x::Union{Int, Float64}
    if y
        x = 1
    else
        x = 1.0
    end
    print(x)
end

function f21()
    x = 1.0
    for i in 1:10
        x = 1
        print(x)
    end
end

function f22()
    x = 1.0
    for i in 1:10
        local x = 1
        print(x)
    end
end

function f23()
    local x
    print(x)
end

function f23(y::Bool)
    local x
    if y
        x = 1
    end
    print(x)
end

function f24(y::Bool)
    # our inferencer can't handle this correctly...
    local x::Int
    if y
        x = 1
    end
    print(x)
end

function f25(y::Bool)
    local x::Int
    if y
        x = 1
    else
        x = 2
    end
    print(x)
end

function f26()
    local y::Int
    for i in 1:10
        y = 1
    end
    print(y)
end

function f27()
    local y::Int
    for i in 1:10
        y = 1
        if y > 0
            z = 1
        end
        print(z)
    end
    print(y)
end

function f28()
    local y::Int
    y = 2
    for i in 1:10
        y = 1
        if y > 0
            z = 1
        end
        z = 3
        print(z)
    end
    print(y)
end

function f29(y::Bool)
    if y
        return 1
    end
    return 2
end

function f30()
    x = 1
    for i in 1:10
        println(i)
        x = 3
    end
    println(Vector{Int, x})
end

function f31()
    x = 1
    for i in 1:10
        println(i)
        x = 1
    end
    println(Array{Int, x})
end


function f32()
    (x,y) = (1,2)
    print((2,3))
end

function f33(x::Int)
    x += 1
end

function f34(x::Int)
    x += 1.0
end

function f35(x::Float64)
    x += 1
end

function f36(z::Float64)
    local x::Int = 1
    y::Int = 3
end

# disallowed
function f37(z::Float64)
    local x::Int = 1.0
end

function f38()
    xref = Base.RefValue{Union{Float64, Int, Float32}}(1)
    x = xref[]
    if x isa Int
        println(x)
    elseif x isa Float32
        println(x)
    else
        println(x)
    end
end

function f39()
    xref = Base.RefValue{Union{Float64, Int, Float32}}(1)
    x = xref[]
    if x isa Int
        println(x)
    elseif x isa Float32
        println(x)
    else
        println(x)
    end
end

function f40(::Type{Vector{Int}})
    return 1
end

function f41(y::Int)
    local x::Union{Int, Nothing}
    if y > 0
        x = 1
    else
        x = nothing
    end
    return x === nothing
end

function f42(y::Vector{Int})
    return y.+1
end

function f43(y::Vector{Int})
    y .+= 3
    y .= y.+y
    println(y[1])
end

struct A
    x::Vector{Int}
end

function f44(y::Vector{Int})
    y .+= 3
    y .= y.+y
    println(y[1])
end

function f45(y::Vector{Int})
    y .+= 3
    
    y .= y
    y .= 1
    y[1:3] .= 1
end

function f45(y::Vector{Vector{Int}})
    y[1] .= y[2]
    
end

end