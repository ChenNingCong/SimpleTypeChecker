module A

module Definition

struct Bad
    x::Int
end
struct Good
    x::Int
end

end

module Adaptor
    import ..Definition:Good, Bad
    println(Bad(1))
end

end