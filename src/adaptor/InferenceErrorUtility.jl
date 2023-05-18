# For debug only
abstract type InferenceError end
function throwInferenceError(err::InferenceError)::Union{}
    Base.throw(err)
    error()
end

@nocheck function toString(typ::CompileType)::String
    return string(typ.typ)
end

function printErrorHead(eng::Engine, ast::JuAST, errkind::String)
    loc = ast.loc
    println(eng.errio, formatLocation(loc))
    print(eng.errio, " ")
    println(eng.errio, errkind)
    code = loc.file.code[loc.span[1]:loc.span[2]]
    println(eng.errio, ' '^4, code)
end

@nocheck function printSignature(io::IOBuffer, f::CompileType, typ::Vector{CompileType}, kwtyp::Vector{Pair{Symbol, CompileType}})
    if isConstVal(f)
        print(io, string(f.val), '(')
    else
        print(io, string(f.typ), '(')
    end
    for i in 1:length(typ)
        print(io, "::", string(typ[i].typ))
        if i != length(typ)
            print(io, ", ")
        else
            if length(kwtyp) != 0
                print(io, "; ")
            end
        end
    end
    for i in kwtyp
        print(io, "$(i.first)::", string(i.second.typ))
    end
    print(io, ')')
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

@nocheck function toIndex(v::Vector{Int})::String
    join(map(x->ifelse(x==1,"the called function", string(x-1)), v), " ,")
end


@nocheck function getTypeSignature(args::Vector{FlowNode})::String
    io = IOBuffer()
    print(io, string(args[1].typ.val), '{')
    for i in 2:length(args)
        print(io, string(args[i].typ.val))
        if i != length(args)
            print(io, ", ")
        end
    end
    print(io, '}')
    return String(take!(io))
end
