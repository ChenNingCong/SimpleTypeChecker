struct NoDerivedExpr
    parent::Int
end

## LHS of assignemnt expression

# (x, y) = v
struct DerivedExprAssignLHSTuple
    parent::Union{JuExpr, DerivedExpr}
    children::Vector{DerivedExpr}
end

# x = v
struct DerivedExprAssignLHSVar
    parent::Union{JuExpr, DerivedExpr}
    var::Symbol
end

# x[i] = v
struct DerivedExprAssignLHSArrayRef
    parent::Union{JuExpr, DerivedExpr}
    arr::JuExpr
    i::Vector{JuExpr}
end

# x[i] = v
struct DerivedExprAssignLHSetProperty
    parent::Union{JuExpr, DerivedExpr}
    x::JuExpr
    # must be DerivedExprSetField
    p::DerivedExpr
end

struct DerivedExprSetField
    parent::DerivedExpr
    p::Symbol
end

## LHS of declarartion expression
## we disallow construction like local (x,y)

# local x
struct DerivedExprDeclarationUntyped
    parent::Union{JuExpr, DerivedExpr}
    var::Symbol
end

# local x::Int
struct DerivedExprDeclarationTyped
    parent::JuExpr
    # must be DerivedExprDeclarationUntyped
    var::DerivedExpr
    typ::JuExpr
end

## Parameter name of a function call

# keyword parameter
struct DerivedExprFunCallExplicitKwParam
    parent::DerivedExpr # must be DerivedExprFunDefParameterList
    var::Symbol
    rhs::JuExpr
end

struct DerivedExprFunCallImplicitKwParam
    parent::DerivedExpr # must be DerivedExprFunDefParameterList
    var::Symbol
end

# list of all keyword parameter
struct DerivedExprFunDefParameterList
    parent::JuExpr
    # Union{DerivedExprFunCallExplicitKwParam, 
    #       DerivedExprFunCallImplicitKwParam}
    pair::Vector{DerivedExpr}
end

## let expression, similiar to local expression
## we disallow construction like let (x,y) = ...
struct DerivedExprLetList
    parent::JuExpr
    pair::Vector{DerivedExpr}
end

# let x
struct DerivedExprLetUntyped
    # must be DerivedExprLetList or DerivedExprLetTyped
    parent::DerivedExpr
    var::Symbol
end

# let x::Int
struct DerivedExprLetTyped
    # must be DerivedExprLetList
    parent::DerivedExpr
    # must be DerivedExprLetUntyped
    var::DerivedExpr
    typ::JuExpr
end

## for assigment
struct DerivedExprForLHSTuple
    parent::Union{JuExpr, DerivedExpr}
    # DerivedExprForLHSTuple or DerivedExprForLHSVar
    children::Vector{DerivedExpr}
end

struct DerivedExprForLHSVar
    parent::Union{JuExpr, DerivedExpr}
    var::Symbol
end

## If expression
struct DerivedExprIfStmt
end

struct DerivedExprElseIfStmt
end

## Field Reference
struct DerivedExprGetField
    parent::JuExpr
    var::Symbol
end

## Function Definition
struct DerivedExprFunName
    parent::JuAST
    name::Symbol
end

struct DerivedExprFunParam
    parent::Int
    var::Symbol
end

struct DerivedExprFunParamList
    parent::Int
end

struct DerivedExprReturnType
    parent::Int
end

struct DerivedExprFunDefArg
    parent::Int
    sym::Symbol
end

struct DerivedExprFunDefArgPos
    parent::Int
    i::Int
end

struct DerivedExprFunSignature
    parent::Int
end