module AbstractTest

function mysum(v)
    i = zero(eltype(v))
    for z in v
        i = i + z
    end
    return i
end

function main()
    v = Int[1,2,3]
    z = mysum(v)
    mysum2(v)
    mysum3(v)
    mysum4(v)
    mysum5(v)
    if z > 0
        main()
    end
end

function mysum2(v::Vector{T}) where T
    i = zero(T)
    for z in v
        i = i + z
    end
    return i
end


function mysum3(v::Vector{T}) where T
    i = zero(T)
    for z in v
        i = i + 1.0
    end
    return i
end

function mysum4(v::Vector{T}) where T <: Real
    i = zero(T)
    for z in v
        i = i + 1.0
    end
    return i
end

function mysum5(v::Vector{T}) where T
    i = Vector{T}()[1]
    for z in v
        i = i + 1.0
    end
    return i
end




end