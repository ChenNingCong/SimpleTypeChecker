import ..SyntaxDefinition.formatLocation

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

@nocheck function printSignature(io::IOBuffer, f::CompileType, typ::Vector{CompileType})
    if isConstVal(f)
        print(io, string(f.val), '(')
    else
        print(io, string(f.typ), '(')
    end
    for i in 1:length(typ)
        print(io, "::", string(typ[i].typ))
        if i != length(typ)
            print(io, ", ")
        end
    end
    print(io, ')')
end

# For debug only
struct InferenceError 
    frame::Vector{Base.StackTraces.StackFrame}
end

function throwInferneceError()::Union{}
    Base.throw(InferenceError(stacktrace()))
    error()
end

function reportErrorUnimplementedAST(eng::Engine, ast::JuAST)
    printErrorHead(eng, ast, "Unimplemented AST kind")
    throwInferneceError()
end

#=
    TODO : we need to raise error more precisely
=#
function reportErrorGetFieldOfBottom(eng::Engine, ast::JuAST)::Union{}
    printErrorHead(eng, ast, "BottomError: calling getproperty on a bottom value")
    throwInferneceError()
end

function reportErrorSetFieldOfBottom(eng::Engine, ast::JuAST)::Union{}
    printErrorHead(eng, ast, "BottomError: calling setproperty! on a bottom value")
    throwInferneceError()
end

function reportErrorSetFieldWithBottom(eng::Engine, ast::JuAST)::Union{}
    printErrorHead(eng, ast, "BottomError: try to assign a bottom value to a field")
    throwInferneceError()
end

function reportErrorArrayRefBottom(eng::Engine, ast::JuAST, i::Int, isSet::Bool)::Union{}
    if isSet
        printErrorHead(eng, ast, "BottomError: rhs of a setindex! is a bottom value")
    else
        if i == 1
            printErrorHead(eng, ast, "BottomError: try to index a bottom value")
        else
            printErrorHead(eng, ast, "BottomError: $(i-1)-th parameter of getindex is an bottom value")
        end
    end
    throwInferneceError()
end

function reportErrorFunCallInternal(eng::Engine, f::CompileType, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    io = IOBuffer()
    argstt = Vector{CompileType}(undef, length(args))
    for i in 1:length(args)
        argstt[i] = args[i].typ
    end
    printSignature(io, f, argstt)
    str = String(take!(io))
    if num >= 2
        printErrorHead(eng, ast, "MethodError: more than one matching method for $(str)")
    elseif num == 0
        printErrorHead(eng, ast, "MethodError: no matching method for $(str)")
    end
    throwInferneceError()
end


function reportErrorArrayRef(eng::Engine, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    reportErrorFunCallInternal(eng, makeConstVal(Base.getindex), ast, args, num)
end

function reportErrorArraySet(eng::Engine, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    reportErrorFunCallInternal(eng, makeConstVal(Base.setindex!), ast, args, num)
end

function getSignature(args::Vector{FlowNode})::String
    io = IOBuffer()
    argstt = Vector{CompileType}(undef, length(args) - 1)
    for i in 1:length(args) - 1
        argstt[i] = args[i + 1].typ
    end
    printSignature(io, args[1].typ, argstt)
    return String(take!(io))
end

function reportErrorIndexReturnBottom(eng::Engine, ast::JuAST, args::Vector{FlowNode})::Union{}
    str = getSignature(args)
    printErrorHead(eng, ast, "BottomError: indexing operation $str returns bottom value")
    throwInferneceError()
end

function reportErrorUseConditionallyDefinedVar(eng::Engine, ast::JuAST, id::Symbol)::Union{}
    printErrorHead(eng, ast, "UndefinedError: variable $id is conditionally defined here")
    throwInferneceError()
end

function reportErrorFunCallUseBottom(eng::Engine, ast::JuAST, i::Int)::Union{}
    if i == 1
        printErrorHead(eng, ast, "BottomError: try to calling a bottom value")
    else
        printErrorHead(eng, ast, "BottomError: $(i-1)-th parameter of function call is an bottom value")
    end
    throwInferneceError()
end

@nocheck function toIndex(v::Vector{Int})::String
    join(map(x->ifelse(x==1,"the called function", string(x-1)), v), " ,")
end

function reportErrorFunCallArgs(eng::Engine, ast::JuAST, args::Vector{FlowNode}, i::Vector{Int})::Union{}
    str = getSignature(args)
    x = toIndex(i)
    printErrorHead(eng, ast, "ArgumentError: $x-th argument of $str is abstract")
    throwInferneceError()
end

function reportErrorNoConstructor(eng::Engine, ast::JuAST, args::Vector{FlowNode})::Union{}
    str = getSignature(args)
    printErrorHead(eng, ast, "ConstructorError : constructor $str returns Union{}")
    throwInferneceError()
end

function reportErrorFunCall(eng::Engine, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    str = getSignature(args)
    if num >= 2
        printErrorHead(eng, ast, "MethodError: more than one matching method for $(str)")
    elseif num == 0
        printErrorHead(eng, ast, "MethodError: no matching method for $(str)")
    end
    throwInferneceError()
end


function reportErrorCurlyCallUseBottom(eng::Engine, ast::JuAST, i::Int)::Union{}
    if i == 1
        printErrorHead(eng, ast, "BottomError: try to call `type`{...} where `type` is a bottom value")
    else
        printErrorHead(eng, ast, "BottomError: $(i-1)-th parameter of `type`{...} call is an bottom value")
    end
    throwInferneceError()
end

function reportErrorApplyTypeNonconst(eng::Engine, ast::JuAST, i::Vector{Int})::Union{}
    x = toIndex(i)
    printErrorHead(eng, ast, "TypeError: $x-th paramter of type application (a.k.a parameterized type) is not a constant")
    throwInferneceError()
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

function reportErrorApplyTypeFailure(eng::Engine, ast::JuAST, args::Vector{FlowNode})::Union{}
    str = getTypeSignature(args)
    printErrorHead(eng, ast, "TypeError: fail to construct type $str")
    throwInferneceError()
end

function reportErrorAssignBottom(eng::Engine, ast::JuAST, i::Symbol)::Union{}
    printErrorHead(eng, ast, "AssignError : try to assign a bottom value to variable $i")
    throwInferneceError()
end

function reportErrorFieldType(eng::Engine, ast::JuAST, arg::FlowNode, isGet::Bool)::Union{}
    accesstype = ifelse(isGet, "getproperty", "setproperty!")
    printErrorHead(eng, ast, "FieldError: calling $accesstype on non-concrete type $(toString(arg.typ))")
    throwInferneceError()
end

function reportErrorNoField(eng::Engine, ast::JuAST, arg::FlowNode, p::Symbol)::Union{}
    printErrorHead(eng, ast, "FieldError: type $(toString(arg.typ)) has no field $p")
    throwInferneceError()
end

function reportErrorSetFieldTypeIncompatible(eng::Engine, ast::JuAST, x::FlowNode, v::FlowNode, ft::CompileType, p::Symbol)::Union{}
    printErrorHead(eng, ast, "FieldError: assigning value of type $(toString(v.typ)) to $(toString(x.typ)) 's field $p, which has type $(toString(ft))")
    throwInferneceError()
end

function reportErrorUndefinedVar(eng::Engine, ast::JuAST, m::Module, p::Symbol)::Union{}
    printErrorHead(eng, ast, "UndefVarError: In module $m, variable $p is not defined")
    throwInferneceError()
end


function reportErrorAssignIncompatible(eng::Engine, old::FlowNode, new::FlowNode)::Union{}
    printErrorHead(eng, new.ex.ast, "AssignError: reassigned type $(toString(new.typ)) is incompatible")
    println(eng.errio, "Previous type is $(toString(old.typ)), ", formatLocation(old.ex.ast.loc))
    throwInferneceError()
end

function reportErrorPrematureUnreachable(eng::Engine, ast::JuAST)::Union{}
    printErrorHead(eng, ast, "UnreachableError: unreachable code exists after this expression")
    throwInferneceError()
end

function reportErrorReturnEnlargeType(eng::Engine, r1::FlowNode, r2::FlowNode)::Union{}
    printErrorHead(eng, r2.ex.ast, "ReturnError: return type is enlarged")
    println(eng.errio, "Previous type is $(toString(r1.typ)), ", formatLocation(r1.ex.ast.loc))
    println(eng.errio, "Current type is $(toString(r2.typ)), ", formatLocation(r2.ex.ast.loc))
    throwInferneceError()
end

function reportErrorBadIsa(eng::Engine, ast::JuAST, msg::String)::Union{}
    printErrorHead(eng, ast, msg)
    throwInferneceError()
end

function reportErrorIsaLHSBadType(eng::Engine, ast::JuAST, val::CompileType)
    printErrorHead(eng, ast, "IsaError: rhs $(toString(val)) is not a valid splitted target")
    throwInferneceError()
end

function reportErrorCondNotBool(eng::Engine, cond::FlowNode)
    printErrorHead(eng, cond.ex.ast, "TypeError: non-boolean type $(toString(cond.typ)) used as condition")
    throwInferneceError()
end

function reportErrorMismatchedSplit(eng::Engine, ast::JuAST, xnode::FlowNode, rhsnode::FlowNode)
    printErrorHead(eng, ast, "IsaError: can't split type $(toString(xnode.typ)) to $(toString(rhsnode.typ))")
    throwInferneceError()
end

function reportErrorIsaBadForm(eng::Engine, ast::JuAST, msg::String)
    printErrorHead(eng, ast, "IsaError: $msg")
    throwInferneceError()
end

function reportErrorIfEnlargeType(eng::Engine, r1::FlowNode, r2::FlowNode)
    printErrorHead(eng, r2.ex.ast, "IfError: if type is enlarged")
    println(eng.errio, "Previous type is $(toString(r1.typ)), ", formatLocation(r1.ex.ast.loc))
    println(eng.errio, "Current type is $(toString(r2.typ)), ", formatLocation(r2.ex.ast.loc))
    throwInferneceError()
end

function reportErrorUnitializedVar(eng::Engine, ast::JuAST, id::Symbol)::Union{}
    printErrorHead(eng, ast, "UndefinedError: variable $id is potentially unitialized here")
    throwInferneceError()
end