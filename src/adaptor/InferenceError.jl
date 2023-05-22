import ..SyntaxDefinition.formatLocation

#=
To make test easier, we separate error reporting and error generation
A type inference error is firstly packed into a InferenceError structure, then dispatched by an io function
to generate human-readable messages.
abstract type InferenceError -> a subtype represents all kinds of inference error
struct InferenceErrorXXX -> a type represent a specific kind of inference error
reportErrorXXX(...)::InferenceErrorXXX -> generate a inference error
displayErrorXXX(io::IO, err::InferenceErrorXXX) -> print the error to the terminal or IOBuffer
=#

struct InferenceErrorAssignBottom <: InferenceError 
    eng::Engine
    ast::JuAST
end

function reportErrorAssignBottom(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorAssignBottom(eng, ast))
end

function displayErrorAssignBottom(err::InferenceErrorAssignBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "AssignError : rhs is a bottom value")
end

struct InferenceErrorAssignIncompatible <: InferenceError 
    eng::Engine
    ast::JuAST
    old::FlowNode
    new::FlowNode
end

function reportErrorAssignIncompatible(eng::Engine, old::FlowNode, new::FlowNode)::Union{}
    throwInferenceError(InferenceErrorAssignIncompatible(eng, new.ast, old, new))
end

function displayErrorAssignIncompatible(err::InferenceErrorAssignIncompatible)::Nothing
    eng = err.eng
    new = err.new
    old = err.old
    new = err.new
    printErrorHead(eng, new.ast, "AssignError: reassigned type $(toString(new.typ)) is incompatible")
    println(eng.errio, "Previous type is $(toString(old.typ)), ", formatLocation(old.ast.loc))
end

struct InferenceErrorUnitializedVar <: InferenceError 
    eng::Engine
    ast::JuAST
    id::Symbol
end

function reportErrorUnitializedVar(eng::Engine, ast::JuAST, id::Symbol)::Union{}
    throwInferenceError(InferenceErrorUnitializedVar(eng, ast, id))
end

function displayErrorUnitializedVar(err::InferenceErrorUnitializedVar)::Nothing
    eng = err.eng
    ast = err.ast
    id = err.id
    printErrorHead(eng, ast, "UndefinedError: variable $id is potentially unitialized here")
end

struct InferenceErrorUndefinedVar <: InferenceError 
    eng::Engine
    ast::JuAST
    m::Module
    p::Symbol
end

function reportErrorUndefinedVar(eng::Engine, ast::JuAST, m::Module, p::Symbol)::Union{}
    throwInferenceError(InferenceErrorUndefinedVar(eng, ast, m, p))
end

function displayErrorUndefinedVar(err::InferenceErrorUndefinedVar)::Nothing
    eng = err.eng
    ast = err.ast
    m = err.m
    p = err.p
    printErrorHead(eng, ast, "UndefVarError: In module $m, variable $p is not defined")
end

struct InferenceErrorUpdateAssignReturnBottom <: InferenceError 
    eng::Engine
    ast::JuAST
end

function reportErrorUpdateAssignReturnBottom(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorUpdateAssignReturnBottom(eng, ast))
end

function displayErrorUpdateAssignReturnBottom(err::InferenceErrorUpdateAssignReturnBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "AssignError: update assign evaluates to a bottom value")
end

struct InferenceErrorFunCall <: InferenceError 
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
    num::Int
    isDotcall::Bool
end

@nocheck function reportErrorFunCall(eng::Engine, ast::JuAST, ms::MethodCallStruct, num::Int, isDotcall::Bool = false)::Union{}
    throwInferenceError(InferenceErrorFunCall(eng, ast, ms, num, isDotcall))
end

function displayErrorFunCall(err::InferenceErrorFunCall)::Nothing
    eng = err.eng
    ast = err.ast
    ms = err.ms
    num = err.num
    isDotcall = err.isDotcall
    str = getSignature(ms)
    if isDotcall
        m = "broadcast call"
    else
        m = "method"
    end
    if num >= 2
        printErrorHead(eng, ast, "MethodError: more than one matching $m for $(str)")
    elseif num == 0
        printErrorHead(eng, ast, "MethodError: no matching $m for $(str)")
    end
end

struct InferenceErrorKeywordUseBottom <: InferenceError 
    eng::Engine
    ast::JuAST
    sym::Symbol
end

function reportErrorKeywordUseBottom(eng::Engine, ast::JuAST, sym::Symbol)::Union{}
    throwInferenceError(InferenceErrorKeywordUseBottom(eng, ast, sym))
end

function displayErrorKeywordUseBottom(err::InferenceErrorKeywordUseBottom)::Nothing
    eng = err.eng
    ast = err.ast
    sym = err.sym
    printErrorHead(eng, ast, "BottomError : keyword argument $sym is assigned with bottom")
end

struct InferenceErrorPrematureUnreachable <: InferenceError 
    eng::Engine
    ast::JuAST
end

function reportErrorPrematureUnreachable(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorPrematureUnreachable(eng, ast))
end

function displayErrorPrematureUnreachable(err::InferenceErrorPrematureUnreachable)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "UnreachableError: unreachable code exists after this expression")
end

struct InferenceErrorTupleBottom <: InferenceError 
    eng::Engine
    ast::JuAST
    id::Int
end

function reportErrorTupleBottom(eng::Engine, ast::JuAST, id::Int)::Union{}
    throwInferenceError(InferenceErrorTupleBottom(eng, ast, id))
end

function displayErrorTupleBottom(err::InferenceErrorTupleBottom)::Nothing
    eng = err.eng
    ast = err.ast
    id = err.id
    printErrorHead(eng, ast, "BottomError: $id-th parameter of the tuple is a bottom value")
end

struct InferenceErrorErrorCurlyCallUseBottom <: InferenceError
    eng::Engine
    ast::JuAST
    i::Int
end

function reportErrorCurlyCallUseBottom(eng::Engine, ast::JuAST, i::Int)::Union{}
    throwInferenceError(InferenceErrorErrorCurlyCallUseBottom(eng, ast, i))
end

function displayErrorCurlyCallUseBottom(err::InferenceErrorErrorCurlyCallUseBottom)::Nothing
    eng = err.eng
    ast = err.ast
    i = err.i
    if i == 1
        printErrorHead(eng, ast, "BottomError: try to call `type`{...} where `type` is a bottom value")
    else
        printErrorHead(eng, ast, "BottomError: $(i-1)-th parameter of `type`{...} call is an bottom value")
    end
end

struct InferenceErrorApplyTypeNonconst <: InferenceError
    eng::Engine
    ast::JuAST
    i::Vector{Int}
end

function reportErrorApplyTypeNonconst(eng::Engine, ast::JuAST, i::Vector{Int})::Union{}
    throwInferenceError(InferenceErrorApplyTypeNonconst(eng, ast, i))
end

function displayErrorApplyTypeNonconst(err::InferenceErrorApplyTypeNonconst)::Nothing
    eng = err.eng
    ast = err.ast
    i = err.i
    x = toIndex(i)
    printErrorHead(eng, ast, "TypeError: $x-th paramter of type application (a.k.a parameterized type) is not a constant")
end

struct InferenceErrorGetFieldOfBottom <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorGetFieldOfBottom(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorGetFieldOfBottom(eng, ast))
end

function displayErrorGetFieldOfBottom(err::InferenceErrorGetFieldOfBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "BottomError: calling getproperty on a bottom value")
end

struct InferenceErrorSetFieldOfBottom <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorSetFieldOfBottom(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorSetFieldOfBottom(eng, ast))
end

function displayErrorSetFieldOfBottom(err::InferenceErrorSetFieldOfBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "BottomError: calling setproperty! on a bottom value")
end

struct InferenceErrorSetFieldWithBottom <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorSetFieldWithBottom(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorSetFieldWithBottom(eng, ast))
end

function displayErrorSetFieldWithBottom(err::InferenceErrorSetFieldWithBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "BottomError: try to assign a bottom value to a field")
end

struct InferenceErrorArrayRefBottom <: InferenceError
    eng::Engine
    ast::JuAST
    i::Int
    isSet::Bool
end

function reportErrorArrayRefBottom(eng::Engine, ast::JuAST, i::Int, isSet::Bool)::Union{}
    throwInferenceError(InferenceErrorArrayRefBottom(eng, ast, i, isSet))
end

function displayErrorArrayRefBottom(err::InferenceErrorArrayRefBottom)::Nothing
    eng = err.eng
    ast = err.ast
    i = err.i
    isSet = err.isSet
    if isSet
        printErrorHead(eng, ast, "BottomError: rhs of a setindex! is a bottom value")
    else
        if i == 1
            printErrorHead(eng, ast, "BottomError: try to index a bottom value")
        else
            printErrorHead(eng, ast, "BottomError: $(i-1)-th parameter of getindex is an bottom value")
        end
    end
end

struct InferenceErrorStringUseBottom <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorStringUseBottom(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorStringUseBottom(eng, ast))
end

function displayErrorStringUseBottom(err::InferenceErrorStringUseBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "BottomError: try to intepolate a bottom into a string")
end


struct InferenceErrorStringConstructor <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
    num::Int
end

function reportErrorStringConstructor(eng::Engine, ast::JuAST, ms::MethodCallStruct, num::Int)::Union{}
    throwInferenceError(InferenceErrorStringConstructor(eng, ast, ms, num))
end

function displayErrorStringConstructor(err::InferenceErrorStringConstructor)::Nothing
    displayErrorFunCall(InferenceErrorFunCall(err.eng, err.ast, err.ms, err.num, false))
end

struct InferenceErrorStringReturnBottom <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
end

function reportErrorStringReturnBottom(eng::Engine, ast::JuAST, ms::MethodCallStruct)::Union{}
    throwInferenceError(InferenceErrorStringReturnBottom(eng, ast, ms))
end
    
function displayErrorStringReturnBottom(err::InferenceErrorStringReturnBottom)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "StringError: interpolation of string returns bottom")
end

struct InferenceErrorArrayRef <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
    num::Int
end

function reportErrorArrayRef(eng::Engine, ast::JuAST, ms::MethodCallStruct, num::Int)::Union{}
    throwInferenceError(InferenceErrorArrayRef(eng, ast, ms, num))
end

function displayErrorArrayRef(err::InferenceErrorArrayRef)::Nothing
    displayErrorFunCall(InferenceErrorFunCall(err.eng, err.ast, err.ms, err.num, false))
end

struct InferenceErrorArraySet <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
    num::Int
end

function reportErrorArraySet(eng::Engine, ast::JuAST, ms::MethodCallStruct, num::Int)::Union{}
    throwInferenceError(InferenceErrorArraySet(eng, ast, ms, num))
end

function displayErrorArraySet(err::InferenceErrorArraySet)::Nothing
    displayErrorFunCall(InferenceErrorFunCall(err.eng, err.ast, err.ms, err.num, false))
end

struct InferenceErrorIndexReturnBottom <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
end

function reportErrorIndexReturnBottom(eng::Engine, ast::JuAST, ms::MethodCallStruct)::Union{}
    throwInferenceError(InferenceErrorIndexReturnBottom(eng, ast, ms))
end

function displayErrorIndexReturnBottom(err::InferenceErrorIndexReturnBottom)::Nothing
    eng = err.eng
    ast = err.ast
    ms = err.ms
    str = getSignature(ms)
    printErrorHead(eng, ast, "BottomError: indexing operation $str returns bottom value")
end

struct InferenceErrorUseConditionallyDefinedVar <: InferenceError
    eng::Engine
    ast::JuAST
    id::Symbol
end

function reportErrorUseConditionallyDefinedVar(eng::Engine, ast::JuAST, id::Symbol)::Union{}
    throwInferenceError(InferenceErrorUseConditionallyDefinedVar(eng, ast, id))
end

function displayErrorUseConditionallyDefinedVar(err::InferenceErrorUseConditionallyDefinedVar)::Nothing
    eng = err.eng
    ast = err.ast
    id = err.id
    printErrorHead(eng, ast, "UndefinedError: variable $id is conditionally defined here")
end

struct InferenceErrorFunCallUseBottom <: InferenceError
    eng::Engine
    ast::JuAST
    i::Int
end

function reportErrorFunCallUseBottom(eng::Engine, ast::JuAST, i::Int)::Union{}
    throwInferenceError(InferenceErrorFunCallUseBottom(eng, ast, i))
end

function displayErrorFunCallUseBottom(err::InferenceErrorFunCallUseBottom)::Nothing
    eng = err.eng
    ast = err.ast
    i = err.i
    if i == 1
        printErrorHead(eng, ast, "BottomError: try to calling a bottom value")
    else
        printErrorHead(eng, ast, "BottomError: $(i-1)-th parameter of function call is an bottom value")
    end
end

struct InferenceErrorFunCallArgs <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
    i::Vector{Int}
    kwi::Vector{Int}
    fargs::Vector{FlowNode}
    kwargs::Vector{Pair{Symbol, FlowNode}}
    isDotcall::Bool
end

function reportErrorFunCallArgs(eng::Engine, ast::JuAST, ms::MethodCallStruct, i::Vector{Int}, 
                                kwi::Vector{Int}, 
                                fargs::Vector{FlowNode}, 
                                kwargs::Vector{Pair{Symbol, FlowNode}},
                                isDotcall::Bool)::Union{}
    throwInferenceError(InferenceErrorFunCallArgs(eng, ast, ms, i, kwi, fargs, kwargs, isDotcall))
end

@nocheck function explainAbstractType(typ::CompileType)::String
    base = toString(typ)
    val = typ.typ
    if val isa UnionAll
        while val isa UnionAll
            val = val.body
        end
        base *= ", \nfully applied type should be $(val)"
    end
    return base
end

@nocheck function isBuiltinEqual(tt::CompileType)::Bool
    if tt.val === (Core.:(===))
        return true
    elseif tt.val ===(Base.:(!==))
        return true
    else
        return false
    end
end

function displayErrorFunCallArgs(err::InferenceErrorFunCallArgs)::Nothing
    eng = err.eng
    ast = err.ast
    ms = err.ms
    i = err.i
    kwi = err.kwi
    fargs = err.fargs
    kwargs = err.kwargs
    isDotcall = err.isDotcall
    str = getSignature(ms)
    if isDotcall
        printErrorHead(eng, ast, "ArgumentError: broadcast call argument is of abstract type")
    else
        printErrorHead(eng, ast, "ArgumentError: Function argument is of abstract type")
    end
    for ii in i
        if ii != 1
            println(eng.errio, "  $(ii-1)-th argument is of abstract type $(explainAbstractType(fargs[ii].typ))")
        else
            println(eng.errio, "  the called function is of abstract type $(explainAbstractType(fargs[ii].typ))")
        end
    end
    for ii in kwi
        name, node = kwargs[ii]
        println(eng.errio, "  keyword argument $(name) is of abstract type $(explainAbstractType(node.typ))")
    end
    if isBuiltinEqual(ms.fargs[1].typ)
        println(eng.errio, "  Don't use ===/!== to compare values of different types, use (<var> isa Nothing) to split type of a variable explicitly")
    end
end

struct InferenceErrorNoConstructor <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
end

function reportErrorNoConstructor(eng::Engine, ast::JuAST, ms::MethodCallStruct)::Union{}
    throwInferenceError(InferenceErrorNoConstructor(eng, ast, ms))
end

function displayErrorNoConstructor(err::InferenceErrorNoConstructor)::Nothing
    eng = err.eng
    ast = err.ast
    ms = err.ms
    str = getSignature(ms)
    printErrorHead(eng, ast, "ConstructorError : constructor $str returns Union{}")
end

struct InferenceErrorBroadcastBottom <: InferenceError
    eng::Engine
    ast::JuAST
    ms::MethodCallStruct
end

function reportErrorBroadcastBottom(eng::Engine, ast::JuAST, ms::MethodCallStruct)::Union{}
    throwInferenceError(InferenceErrorBroadcastBottom(eng, ast, ms))
end

function displayErrorBroadcastBottom(err::InferenceErrorBroadcastBottom)::Nothing
    eng = err.eng
    ast = err.ast
    ms = err.ms
    str = getSignature(ms)
    printErrorHead(eng, ast, "BroadcastError : broadcast $str returns Union{}")
end

struct InferenceErrorApplyTypeFailure <: InferenceError
    eng::Engine
    ast::JuAST
    args::Vector{FlowNode}
end

function reportErrorApplyTypeFailure(eng::Engine, ast::JuAST, args::Vector{FlowNode})::Union{}
    throwInferenceError(InferenceErrorApplyTypeFailure(eng, ast, args))
end

function displayErrorApplyTypeFailure(err::InferenceErrorApplyTypeFailure)::Nothing
    eng = err.eng
    ast = err.ast
    args = err.args
    str = getTypeSignature(args)
    printErrorHead(eng, ast, "TypeError: fail to construct type $str")
end

struct InferenceErrorFieldType <: InferenceError
    eng::Engine
    ast::JuAST
    arg::FlowNode
    isSet::Bool
end

function reportErrorFieldType(eng::Engine, ast::JuAST, arg::FlowNode, isSet::Bool)::Union{}
    throwInferenceError(InferenceErrorFieldType(eng, ast, arg, isSet))
end

function displayErrorFieldType(err::InferenceErrorFieldType)::Nothing
    eng = err.eng
    ast = err.ast
    arg = err.arg
    isSet = err.isSet
    accesstype = ifelse(!isSet, "getproperty", "setproperty!")
    printErrorHead(eng, ast, "FieldError: calling $accesstype on non-concrete type $(toString(arg.typ))")
end

struct InferenceErrorNoField <: InferenceError
    eng::Engine
    ast::JuAST
    arg::FlowNode
    p::Symbol
end

function reportErrorNoField(eng::Engine, ast::JuAST, arg::FlowNode, p::Symbol)::Union{}
    throwInferenceError(InferenceErrorNoField(eng, ast, arg, p))
end

function displayErrorNoField(err::InferenceErrorNoField)::Nothing
    eng = err.eng
    ast = err.ast
    arg = err.arg
    p = err.p
    printErrorHead(eng, ast, "FieldError: type $(toString(arg.typ)) has no field $p")
end

struct InferenceErrorSetFieldTypeIncompatible <: InferenceError
    eng::Engine
    ast::JuAST
    x::FlowNode
    v::FlowNode
    ft::CompileType
    p::Symbol
end

function reportErrorSetFieldTypeIncompatible(eng::Engine, ast::JuAST, x::FlowNode, v::FlowNode, ft::CompileType, p::Symbol)::Union{}
    throwInferenceError(InferenceErrorSetFieldTypeIncompatible(eng, ast, x, v, ft, p))
end

function displayErrorSetFieldTypeIncompatible(err::InferenceErrorSetFieldTypeIncompatible)::Nothing
    eng = err.eng
    ast = err.ast
    x = err.x
    v = err.v
    ft = err.ft
    p = err.p
    printErrorHead(eng, ast, "FieldError: assigning value of type $(toString(v.typ)) to $(toString(x.typ)) 's field $p, which has type $(toString(ft))")
end

struct InferenceErrorAssignInitIncompatible <: InferenceError
    eng::Engine
    ast::JuAST
    ass::CompileType
    init::CompileType
end

function reportErrorAssignInitIncompatible(eng::Engine, ast::JuAST, ass::CompileType, init::CompileType)::Union{}
    throwInferenceError(InferenceErrorAssignInitIncompatible(eng, ast, ass, init))
end

function displayErrorAssignInitIncompatible(err::InferenceErrorAssignInitIncompatible)::Nothing
    eng = err.eng
    ast = err.ast
    ass = err.ass
    init = err.init
    printErrorHead(eng, ast, "AssignError: initializer's type $(toString(init)) is incompatible with asserted type of $(toString(ass))")
    println(eng.errio, "Note implicit conversion is disallowed for assignment.")
end

struct InferenceErrorReturnEnlargeType <: InferenceError
    eng::Engine
    r1::FlowNode
    r2::FlowNode
end

function reportErrorReturnEnlargeType(eng::Engine, r1::FlowNode, r2::FlowNode)::Union{}
    throwInferenceError(InferenceErrorReturnEnlargeType(eng, r1, r2))
end

function displayErrorReturnEnlargeType(err::InferenceErrorReturnEnlargeType)::Nothing
    eng = err.eng
    r1 = err.r1
    r2 = err.r2
    printErrorHead(eng, r2.ast, "ReturnError: return type is enlarged")
    println(eng.errio, "Previous type is $(toString(r1.typ)), ", formatLocation(r1.ast.loc))
    println(eng.errio, "Current type is $(toString(r2.typ)), ", formatLocation(r2.ast.loc))
end

struct InferenceErrorIsaLHSBadType <: InferenceError
    eng::Engine
    ast::JuAST
    val::CompileType
end

function reportErrorIsaLHSBadType(eng::Engine, ast::JuAST, val::CompileType)::Union{}
    throwInferenceError(InferenceErrorIsaLHSBadType(eng, ast, val))
end

function displayErrorIsaLHSBadType(err::InferenceErrorIsaLHSBadType)::Nothing
    eng = err.eng
    ast = err.ast
    val = err.val
    printErrorHead(eng, ast, "IsaError: rhs $(toString(val)) is not a valid splitted target")
end

struct InferenceErrorCondNotBool <: InferenceError
    eng::Engine
    ast::JuAST
    cond::FlowNode
end

function reportErrorCondNotBool(eng::Engine, cond::FlowNode)::Union{}
    throwInferenceError(InferenceErrorCondNotBool(eng, cond.ast, cond))
end

function displayErrorCondNotBool(err::InferenceErrorCondNotBool)::Nothing
    eng = err.eng
    ast = err.ast
    cond = err.cond
    printErrorHead(eng, cond.ast, "TypeError: non-boolean type $(toString(cond.typ)) used as condition")
end

struct InferenceErrorMismatchedSplit <: InferenceError
    eng::Engine
    ast::JuAST
    xnode::FlowNode
    rhsnode::FlowNode
end

function reportErrorMismatchedSplit(eng::Engine, ast::JuAST, xnode::FlowNode, rhsnode::FlowNode)::Union{}
    throwInferenceError(InferenceErrorMismatchedSplit(eng, ast, xnode, rhsnode))
end

function displayErrorMismatchedSplit(err::InferenceErrorMismatchedSplit)::Nothing
    eng = err.eng
    ast = err.ast
    xnode = err.xnode
    rhsnode = err.rhsnode
    printErrorHead(eng, ast, "IsaError: can't split type $(toString(xnode.typ)) to $(toString(rhsnode.typ))")
end

struct InferenceErrorIsaBadForm <: InferenceError
    eng::Engine
    ast::JuAST
    msg::String
end
    
function reportErrorIsaBadForm(eng::Engine, ast::JuAST, msg::String)::Union{}
    throwInferenceError(InferenceErrorIsaBadForm(eng, ast, msg))
end

function displayErrorIsaBadFor(err::InferenceErrorIsaBadForm)::Nothing
    eng = err.eng
    ast = err.ast
    msg = err.msg
    printErrorHead(eng, ast, msg)
end

struct InferenceErrorIfEnlargeType <: InferenceError
    eng::Engine
    r1::FlowNode
    r2::FlowNode
end

function reportErrorIfEnlargeType(eng::Engine, r1::FlowNode, r2::FlowNode)::Union{}
    throwInferenceError(InferenceErrorIfEnlargeType(eng, r1, r2))
end

function displayErrorIfEnlargeType(err::InferenceErrorIfEnlargeType)::Nothing
    eng = err.eng
    r1 = err.r1
    r2 = err.r2
    printErrorHead(eng, r2.ast, "IfError: if type is enlarged")
    println(eng.errio, "Previous type is $(toString(r1.typ)), ", formatLocation(r1.ast.loc))
    println(eng.errio, "Current type is $(toString(r2.typ)), ", formatLocation(r2.ast.loc))
end

struct InferenceErrorFailedToDestruct <: InferenceError
    eng::Engine
    ast::JuAST
    msg::String
end

function reportErrorFailedToDestruct(eng::Engine, ast::JuAST, msg::String)::Union{}
    throwInferenceError(InferenceErrorFailedToDestruct(eng, ast, msg))
end

function displayErrorFailedToDestruct(err::InferenceErrorFailedToDestruct)::Nothing
    eng = err.eng
    ast = err.ast
    msg = err.msg
    printErrorHead(eng, ast, "AssignError: Failed to destruct rhs to lhs : $msg")
end

struct InferenceErrorIsaRHSNonconst <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorIsaRHSNonconst(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorIsaRHSNonconst(eng, ast))
end

function displayErrorIsaRHSNonconst(err::InferenceErrorIsaRHSNonconst)
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "TypeError: rhs of isa is not a constant")
end

struct InferenceErrorTypeAssertNonconst <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorTypeAssertNonconst(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorTypeAssertNonconst(eng, ast))
end

function displayErrorTypeAssertNonconst(err::InferenceErrorTypeAssertNonconst)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "TypeError: the typed used for type assertion is not a constant")
end


struct InferenceErrorkeywordNotDefined <: InferenceError
    eng::Engine
    ast::JuAST
    sym::Symbol
end

function reportErrorkeywordNotDefined(eng::Engine, ast::JuAST, sym::Symbol)::Union{}
    throwInferenceError(InferenceErrorkeywordNotDefined(eng, ast, sym))
end

function displayErrorkeywordNotDefined(err::InferenceErrorkeywordNotDefined)::Nothing
    eng = err.eng
    ast = err.ast
    sym = err.sym
    printErrorHead(eng, ast, "UndefinedError : keyword $sym is required but not inputed")
end

struct InferenceErrorElementNonSameType <: InferenceError
    eng::Engine
    ast::JuAST
    t1::FlowNode
    t2::FlowNode
    typed::Bool
end

function reportErrorElementNonSameType(eng::Engine, ast::JuAST, t1::FlowNode, t2::FlowNode, typed::Bool)::Union{}
    throwInferenceError(InferenceErrorElementNonSameType(eng, ast, t1, t2, typed))
end

function displayErrorElementNonSameType(err::InferenceErrorElementNonSameType)::Nothing
    eng = err.eng
    ast = err.ast
    t1 = err.t1
    t2 = err.t2
    printErrorHead(eng, ast, "Array element must have the same type")
end


struct InferenceErrorEmptyAnyArray <: InferenceError
    eng::Engine
    ast::JuAST
end

function reportErrorEmptyAnyArray(eng::Engine, ast::JuAST)::Union{}
    throwInferenceError(InferenceErrorEmptyAnyArray(eng, ast))
end

function displayErrorEmptyAnyArray(err::InferenceErrorEmptyAnyArray)::Nothing
    eng = err.eng
    ast = err.ast
    printErrorHead(eng, ast, "Use `Any[]` instead of `[]` to construct Any array")
end
