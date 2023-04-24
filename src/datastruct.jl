struct ImmutableDict{K,V}
    data::Dict{K, V}
end

function ImmutableDict{K,V}() where {K,V}
    return ImmutableDict{K,V}(Dict{K, V}())
end

function shallow_copy(d::ImmutableDict{K,V})::ImmutableDict{K,V} where {K,V}
    return ImmutableDict{K,V}(copy(d.data))
end

function update(d::ImmutableDict{K,V}, k::K, v::V)::ImmutableDict{K,V} where {K,V}
    newd = shallow_copy(d)
    newd.data[k] = v;
    return newd     
end

function exist(d::ImmutableDict{K,V}, k::K)::Bool where {K,V}
    return Base.haskey(d.data, k)
end

function fetch(d::ImmutableDict{K,V}, k::K)::V where {K,V}
    if exist(d, k)
        return d.data[k]
    else
        throw(Base.KeyError(k))
    end
end

