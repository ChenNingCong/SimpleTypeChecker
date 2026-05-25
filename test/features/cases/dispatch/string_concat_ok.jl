module DispatchStringConcat
# Negative control: `string(::String, ::Int)` LOOKS like it could go dynamic
# (it walks the open-world print chain in general), but the compiler
# resolves this exact specialization statically to an :invoke of
# print_to_string. Verifies the checker doesn't false-positive on it.
function ok()::String
    return string("count = ", 42)
end
end
