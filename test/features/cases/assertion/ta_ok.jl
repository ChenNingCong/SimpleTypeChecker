module TaOk
# Value already has the asserted type — no error.
function f()::Int
    x = 1
    y = x::Int
    return y + 1
end
end
