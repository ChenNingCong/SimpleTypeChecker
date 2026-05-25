module VecEmpty
# Bare `[]` — expected to flag InferenceErrorEmptyAnyArray (with the
# guidance to use `Any[]` instead).
f() = []
end
