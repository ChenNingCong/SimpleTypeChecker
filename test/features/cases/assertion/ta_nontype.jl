module TaNonType
# The asserted "type" is actually a runtime value, not a Type. Expected to
# flag InferenceErrorApplyTypeNonconst.
function f(x::Int)
    nottype = 42
    return x::nottype
end
end
