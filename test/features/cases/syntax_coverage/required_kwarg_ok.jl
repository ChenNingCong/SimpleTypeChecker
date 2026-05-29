module RequiredKwargOk

# A required keyword argument — `type::String` has a type annotation and
# no default. By default the checker flags this as `keywordNotDefined`
# because Julia is entitled to decline specialization on kwarg types
# (the same hazard as `::Function` positionals — abstract dispatch). The
# `permitRequiredKwargs` flag turns the warning off for downstream
# packages that lean on this idiom and accept that risk.

function render(uml::String; type::String)
    return string(uml, "-", type)
end

function caller()::String
    return render("a"; type="b")
end

end
