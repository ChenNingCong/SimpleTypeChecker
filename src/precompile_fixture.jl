# Tiny fixture exercised by precompile.jl during package precompile.
# Triggers the dispatch-error code path (abstract field call) so the
# error-display code gets cached too.
struct Holder
    f::Any
end
go(h::Holder, x::Int) = h.f(x)
