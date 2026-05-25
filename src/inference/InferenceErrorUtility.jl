# For debug only
abstract type InferenceError end
function throwInferenceError(err::InferenceError)::Union{}
    Base.throw(err)
end

# Typed type-to-string. The naive `string(typ.typ)` lowers to `string(::Any)`
# which dyn-dispatches and trips the deep dispatch detector at every caller.
# Here we keep a single entry method with `::Any` arg (single-method dispatch,
# resolves statically at the call site) and route by `isa` inside the body.
# Each branch operates on a narrowed concrete type so its inner calls are
# static too. The DataType name itself comes from Julia's C runtime via
# ccall (`jl_typename_str`) — same trim-safe pattern as the JLL ccall layer.
#
# For the truly-unknown fallback (Const values etc.) we delegate to a single
# `@nocheck`-marked helper to keep the caller's call tree clean.
@nocheck function _to_string_fallback(t::Any)::String
    return string(t)
end

function _type_to_string(@nospecialize t)::String
    if t isa DataType
        cstr = ccall(:jl_typename_str, Cstring, (Any,), t)
        name::String = Base.unsafe_string(cstr)
        params = t.parameters
        if length(params) == 0
            return name
        end
        parts = String[name, "{"]
        n = length(params)
        i = 1
        while i <= n
            push!(parts, _type_to_string(params[i]))
            if i < n
                push!(parts, ", ")
            end
            i += 1
        end
        push!(parts, "}")
        return join(parts)
    elseif t isa Union
        parts = String["Union{"]
        _union_flatten!(parts, t)
        push!(parts, "}")
        return join(parts)
    elseif t isa UnionAll
        body_str::String = _type_to_string(t.body)
        var_str::String  = _type_to_string(t.var)
        return string(body_str, " where ", var_str)
    elseif t isa TypeVar
        return String(t.name)
    else
        return _to_string_fallback(t)
    end
end

# Walks a Union's nested `.a` / `.b` structure and pushes "A, B, C" parts.
# Single-method entry, isa-dispatched body — same shape as _type_to_string.
function _union_flatten!(parts::Vector{String}, @nospecialize t)::Nothing
    if t isa Union
        _union_flatten!(parts, t.a)
        push!(parts, ", ")
        _union_flatten!(parts, t.b)
    else
        push!(parts, _type_to_string(t))
    end
    return nothing
end

@nocheck function toString(typ::CompileType)::String
    return _type_to_string(typ.typ)
end

function printErrorHead(eng::Engine, ast::JuAST, errkind::String)::Nothing
    loc = ast.loc
    writelogln!(eng.errio, formatLocation(loc))
    writelog!(eng.errio, " ")
    writelogln!(eng.errio, errkind)
    try
        code = loc.file.code[loc.span[1]:loc.span[2]]
        needTab = !occursin('\n', code)
        if needTab
            writelog!(eng.errio, "    ")
            writelogln!(eng.errio, code)
        else
            writelogln!(eng.errio)
            writelogln!(eng.errio, code)
        end
    catch e
        writelogln!(eng.errio, "    (Failed to extract code)")
    end
    return nothing
end

@nocheck function printSignature(io::IOBuffer, f::CompileType, typ::Vector{CompileType}, kwtyp::Vector{Pair{Symbol, CompileType}})::Nothing
    if isConstVal(f)
        write(io, _type_to_string(f.val))
    else
        write(io, _type_to_string(f.typ))
    end
    write(io, '(')
    n = length(typ)
    for i in 1:n
        write(io, "::")
        write(io, _type_to_string(typ[i].typ))
        if i != n
            write(io, ", ")
        elseif length(kwtyp) != 0
            write(io, "; ")
        end
    end
    nkw = length(kwtyp)
    for j in 1:nkw
        kw = kwtyp[j]
        write(io, String(kw.first))
        write(io, "::")
        write(io, _type_to_string(kw.second.typ))
    end
    write(io, ')')
    return nothing
end


function getSignature(ms::MethodCallStruct)::String
    io = IOBuffer()
    args = ms.fargs
    argstt = Vector{CompileType}(undef, length(args) - 1)
    for i in 1:length(args) - 1
        argstt[i] = args[i + 1].typ
    end
    kwargstt = similar(ms.kwargs, Pair{Symbol, CompileType})
    for i in eachindex(kwargstt)
        kwargstt[i] = ms.kwargs[i].first=>ms.kwargs[i].second.typ
    end
    printSignature(io, args[1].typ, argstt, kwargstt)
    return String(take!(io))
end

function toIndex(v::Vector{Int})::String
    parts = String[]
    for x in v
        if x == 1
            push!(parts, "the called function")
        else
            push!(parts, string(x - 1))
        end
    end
    return join(parts, " ,")
end


@nocheck function getTypeSignature(args::Vector{FlowNode})::String
    io = IOBuffer()
    write(io, _type_to_string(args[1].typ.val))
    write(io, '{')
    n = length(args)
    for i in 2:n
        write(io, _type_to_string(args[i].typ.val))
        if i != n
            write(io, ", ")
        end
    end
    write(io, '}')
    return String(take!(io))
end
