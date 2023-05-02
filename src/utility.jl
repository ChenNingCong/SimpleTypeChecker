macro nocheck(e)
    return e
end

function getMethodMatches(f::Function, tts::Vector{FlowNode})
    Base.code_typed_by_type(Tuple{typeof(f), [i.typ.val for i in tts]...})
end

function getMethodMatches(tts::Vector{FlowNode})
    Base.code_typed_by_type(Tuple{[i.typ.val for i in tts]...})
end

extractUniqueMatch(v::Vector{Any})::JuType = makeJuType(v[1][2])