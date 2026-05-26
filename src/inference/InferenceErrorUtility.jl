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
# Fallback for non-type values (Const values etc.). Single-method
# `@nospecialize` makes this an escape-hatch — the checker accepts the
# `Any` arg here without flagging callers, and skips the body. Body
# delegates to Base.string which dyn-dispatches on Any (by design).
function _to_string_fallback(@nospecialize t)::String
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

# `toString(rtctx, ct)` / `toConstString(rtctx, ct)` live in
# src/compiler/CompilerOps.jl alongside the other membrane-op wrappers.
# Don't redefine them here — Julia precompile rejects method overwriting.

# Legacy entry point for emitters that haven't been individually migrated
# to build a rich `Diagnostic` (with notes / helps / labeled spans). It
# parses the established `<Category>Error[ ]:[ ]<msg>` headline shape into
# a stable code (`JTC-ASSIGN`, `JTC-BOTTOM`, `JTC-METHOD`, ...) and renders
# the rustc-shape layout with a primary span at `ast`. Emitters that need
# secondary notes should build a `Diagnostic` directly and call
# `renderDiagnostic` — this shim is for the simple headline-only cases.
function printErrorHead(eng::Engine, ast::JuAST, errkind::String)::Nothing
    codeAndHead = _legacyHeadlineToCode(errkind)
    primary = LabeledSpan[LabeledSpan(ast.loc, "", true)]
    diag = Diagnostic(codeAndHead.first, true, codeAndHead.second,
                      primary, DiagNote[], String[])
    renderDiagnostic(eng.errio, diag)
    return nothing
end

# Split a legacy "<CategoryError>: <msg>" headline into (code, headline).
# "AssignError: rhs is a bottom value" → ("JTC-ASSIGN", "rhs is a bottom
# value"). The "Error" suffix is stripped; the prefix is uppercased.
# Headlines that don't match the pattern get `JTC-MISC` and pass through
# verbatim.
function _legacyHeadlineToCode(s::String)::Pair{String, String}
    colonPos = 0
    i = 1
    n = sizeof(s)
    while i <= n
        if s[i] == ':'
            colonPos = i
            break
        end
        i += 1
    end
    if colonPos == 0
        return "JTC-MISC" => s
    end
    # rstrip whitespace before ':'
    prefixEnd = colonPos - 1
    while prefixEnd >= 1 && (s[prefixEnd] == ' ' || s[prefixEnd] == '\t')
        prefixEnd -= 1
    end
    # lstrip whitespace after ':'
    bodyStart = colonPos + 1
    while bodyStart <= n && (s[bodyStart] == ' ' || s[bodyStart] == '\t')
        bodyStart += 1
    end
    if prefixEnd < 1
        return "JTC-MISC" => s
    end
    prefix = SubString(s, 1, prefixEnd)
    body = bodyStart <= n ? SubString(s, bodyStart, n) : SubString(s, 1, 0)

    # Drop the trailing "Error" if present
    pstr::String = String(prefix)
    if endswith(pstr, "Error")
        cut = sizeof(pstr) - sizeof("Error")
        pstr = String(SubString(pstr, 1, cut))
    end
    if isempty(pstr)
        return "JTC-MISC" => s
    end
    # Uppercase only — keep names short and simple. CamelCase splits could
    # come later if we want JTC-UNDEF-VAR vs JTC-UNDEFVAR.
    buf = IOBuffer()
    write(buf, "JTC-")
    for c in pstr
        write(buf, uppercase(c))
    end
    code = String(take!(buf))
    return code => String(body)
end

function printSignature(rtctx::RuntimeContext, io::IOBuffer,
                        f::CompileType,
                        typ::Vector{CompileType},
                        kwtyp::Vector{Pair{Symbol, CompileType}})::Nothing
    # `toConstString` handles both branches (const-value via `repr`, otherwise
    # type-display) — the inferencer doesn't need to peek `f.isConst` here.
    write(io, toConstString(rtctx, f))
    write(io, '(')
    n = length(typ)
    for i in 1:n
        write(io, "::")
        write(io, toString(rtctx, typ[i]))
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
        write(io, toString(rtctx, kw.second))
    end
    write(io, ')')
    return nothing
end


function getSignature(rtctx::RuntimeContext, ms::MethodCallStruct)::String
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
    printSignature(rtctx, io, args[1].typ, argstt, kwargstt)
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


function getTypeSignature(rtctx::RuntimeContext, args::Vector{FlowNode})::String
    io = IOBuffer()
    # Each arg's `.typ` is a const Type-value CompileType — `toConstString`
    # crosses the seam and returns the rendered name.
    write(io, toConstString(rtctx, args[1].typ))
    write(io, '{')
    n = length(args)
    for i in 2:n
        write(io, toConstString(rtctx, args[i].typ))
        if i != n
            write(io, ", ")
        end
    end
    write(io, '}')
    return String(take!(io))
end
