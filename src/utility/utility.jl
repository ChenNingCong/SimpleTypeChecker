macro nocheck(e)
    return esc(e)
end

struct Maybe{T}
    val::Union{Nothing, T}
end

function None(val::Type{T}) where T
    return Maybe{T}(nothing)
end
function Just(x::T) where T
    return Maybe{T}(x)
end

function isNone(x::Maybe{T})::Bool where T
    return x.val isa Nothing
end

function castJust(x::Maybe{T})::T where T
    val = x.val
    if val isa Nothing
        error("Not a Just")
    else
        return val
    end
end
export @nocheck, Maybe, None, Just, isNone, castJust