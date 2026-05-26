module QualifiedFnOk
# `function Base.write(...)` is a method extension on a qualified name.
# JuliaSyntax emits `:.[identifier Base, identifier write]` for plain
# qualified access — the old collectQualifiedName required the rhs to be
# `:quote`-wrapped, so this form was rejected.
struct Sink
    n::Int
end

function Base.write(s::Sink, x::UInt8)::Int
    return Int(x) + s.n
end
end
