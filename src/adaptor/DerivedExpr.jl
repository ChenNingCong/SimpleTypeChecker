struct NoDerivedExpr
    parent::JuExpr
end

struct DerivedExprKwArg
    parent::JuExpr
    var::Symbol
end

struct DerivedExprKwArgLHS
    parent::DerivedExprKwArg
end

struct DerivedExprParameter
    parent::JuExpr
end

struct DerivedExprImplicitKwArg
    parent::JuExpr
    var::Symbol
end

struct DerivedExprModuleName
    parent::JuExpr
end

struct DerivedExprForVar
    parent::JuExpr
end

struct DerivedExprField
    parent::JuExpr
end

struct DerivedExprAssignLHS
    parent::JuExpr
end

struct DerivedExprFunName
    parent::JuExpr
end

struct DerivedExprFunParam
    parent::JuExpr
    var::Symbol
end

struct DerivedExprFunParamList
    parent::JuExpr
end

struct DerivedExprReturnType
    parent::JuExpr
end

struct DerivedExprFunDefArg
    parent::JuExpr
    sym::Symbol
end

struct DerivedExprFunDefArgPos
    parent::JuExpr
    i::Int
end

struct DerivedExprFunSignature
    parent::JuExpr
end

struct DerivedExprArrayRef
    parent::JuExpr
end

struct DerivedExprArraySet
    parent::JuExpr
end

struct DerivedExprForAssign
    parent::JuExpr
end

struct DerivedExprElseIf
    parent::JuExpr
end