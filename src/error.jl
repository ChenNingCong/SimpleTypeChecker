function formatLocation(loc::Location)::String
    file = loc.file
    span = loc.span
    start = convert2line(file.line_starts, span[1])
    ff = file.filename 
    if ff isa Nothing
        filename = "(none)"
    else
        filename = file.filename
    end
    return "at file $(filename) line $(start[1]):$(start[2])"
end

function printError(ast::JuAST)
    loc = ast.span
    println(formatLocation(loc))
    code = loc.file.code[loc.span[1]:loc.span[2]]
    println("  $code")
    return
end

function printErrorHead(eng::Engine, ast::JuAST, errkind::String)
    println(eng.errio, formatLocation(ast.span))
    print(eng.errio, "  ")
    println(eng.errio, errkind)
end

function printSignature(io::IO, f::Any, typ::Vector{Any})
    print(io, f, '(')
    for i in 1:length(typ)
        print(io, "::", typ[i])
        if i != length(typ)
            print(io, ", ")
        end
    end
    print(io, ')')
end

struct InferenceError
end

function reportErrorFunCallInternal(eng::Engine, f::Any, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    io = IOBuffer()
    printSignature(io, f, Any[i.typ.val for i in args])
    str = String(take!(io))
    if num >= 2
        printErrorHead(eng, ast, "MethodError: more than one method matching $str")
    elseif num == 0
        printErrorHead(eng, ast, "MethodError: no method matching $str")
    end
    Base.throw(InferenceError())
end

function reportErrorFunCall(eng::Engine, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    io = IOBuffer()
    printSignature(io, args[1].val.val, Any[args[i].typ.val for i in 2:length(args)])
    str = String(take!(io))
    if num >= 2
        printErrorHead(eng, ast, "MethodError: more than one method matching $str")
    elseif num == 0
        printErrorHead(eng, ast, "MethodError: no method matching $str")
    end
    Base.throw(InferenceError())
end


function reportErrorArrayRef(eng::Engine, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    reportErrorFunCallInternal(eng, Base.getindex, ast, args, num)
end

function reportErrorArraySet(eng::Engine, ast::JuAST, args::Vector{FlowNode}, num::Int)::Union{}
    reportErrorFunCallInternal(eng, Base.setindex!, ast, args, num)
end

function reportErrorFunCallArgs(eng::Engine, ast::JuAST, args::Vector{FlowNode}, i::Vector{Int})::Union{}
    io = IOBuffer()
    printSignature(io, args[1].typ.val, Any[args[i].typ.val for i in 2:length(args)])
    str = String(take!(io))
    printErrorHead(eng, ast, "ArgumentError: $i-th argument of $str is abstract")
    Base.throw(InferenceError())
end

function reportErrorFieldType(eng::Engine, ast::JuAST, arg::FlowNode, isGet::Bool)
    accesstype = isGet ? "getproperty" : "setproperty!"
    printErrorHead(eng, ast, "FieldError: calling $accesstype on non-concrete type $(arg.typ.val)")
    Base.throw(InferenceError())
end

function reportErrorNoField(eng::Engine, ast::JuAST, arg::FlowNode, p::Symbol)
    printErrorHead(eng, ast, "FieldError: type $(arg.typ.val) has no field $p")
    Base.throw(InferenceError())
end

function reportErrorSetFieldTypeIncompatible(eng::Engine, ast::JuAST, x::FlowNode, v::FlowNode, ft::Any, p::Symbol)
    printErrorHead(eng, ast, "FieldError: assigning value of type $(v.typ.val) to $(x.typ.val) 's field $p, which has type $ft")
    Base.throw(InferenceError())
end

function reportErrorUndefinedVar(eng::Engine, ast::JuAST, m::Module, p::Symbol)
    printErrorHead(eng, ast, "UndefVarError: In module $m, variable $p is not defined")
    Base.throw(InferenceError())
end

function reportErrorReturnEnlargeType(eng::Engine, r1::FlowNode, r2::FlowNode)
    printErrorHead(eng, r2.ast, "ReturnError: return type is enlarged")
    println(eng.errio, "Previous type is $(r1.typ.val), ", formatLocation(r1.ast.span))
    println(eng.errio, "Current type is $(r2.typ.val), ", formatLocation(r2.ast.span))
    Base.throw(InferenceError())
end

function reportErrorAssignIncompatible(eng::Engine, old::FlowNode, new::FlowNode)
    printErrorHead(eng, new.ast, "AssignError: reassigned type $(new.typ.val) is incompatible")
    println(eng.errio, "Previous type is $(old.typ.val), ", formatLocation(old.ast.span))
    Base.throw(InferenceError())
end

function reportErrorCondNotBool(eng::Engine, cond::FlowNode)
    printErrorHead(eng, cond.ast, "TypeError: non-boolean type $(cond.typ.val) used as condition")
    Base.throw(InferenceError())
end

function reportWarningUnionAsValue(eng, ast::JuAST, i::Int)
    if i == 1
        printErrorHead(eng, ast, "UnreachableError: function being called is a bottom value")
    else
        printErrorHead(eng, ast, "UnreachableError: $(i-1)-th argument of function call is a bottom value")
    end
end

function reportWarningAssignUnion(eng, ast::JuAST, i::Symbol)
    printErrorHead(eng, ast, "AssignError : try to assign a Union{} to variable $i")
end

function reportErrorNoConstructor(eng::Engine, ast::JuAST, args::Vector{FlowNode})::Union{}
    io = IOBuffer()
    printSignature(io, args[1].val.val, Any[args[i].typ.val for i in 2:length(args)])
    str = String(take!(io))
    printErrorHead(eng, ast, "ConstructorError : constructor $str returns Union{}")
    Base.throw(InferenceError())
end

function reportErrorIfEnlargeType(eng::Engine, r1::FlowNode, r2::FlowNode)
    printErrorHead(eng, r2.ast, "IfError: if type is enlarged")
    println(eng.errio, "Previous type is $(r1.typ.val), ", formatLocation(r1.ast.span))
    println(eng.errio, "Current type is $(r2.typ.val), ", formatLocation(r2.ast.span))
    Base.throw(InferenceError())
end

function reportErrorIsaBadForm(eng::Engine, ast::JuAST, msg::String)
    printErrorHead(eng, ast, "IsaError: $msg")
    Base.throw(InferenceError())
end

function reportErrorIsaLHSBadType(eng::Engine, ast::JuAST, node::FlowNode)
    printErrorHead(eng, ast, "IsaError: current value is not a valid splitted target $(node.typ.val)")
    println(eng.errio, "Current type is $(node.typ.val)")
    Base.throw(InferenceError())
end