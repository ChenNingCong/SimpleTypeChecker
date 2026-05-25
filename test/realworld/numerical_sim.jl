module NumericalSim

# A small but representative numerical workload — exercises:
#   - Untyped vector literals [a, b, c]
#   - Lambdas passed to higher-order functions
#   - Type assertions (::) as runtime casts
#   - `<:` and `where` in expression context
#   - @inbounds passthrough macro
#
# Run with:
#   julia --project=. -e 'using SimpleTypeChecker; include("test/realworld/numerical_sim.jl"); \
#     ctx = SimpleTypeChecker.Inference.GlobalContext(); \
#     SimpleTypeChecker.API.addFile!(ctx, NumericalSim, abspath("test/realworld/numerical_sim.jl")); \
#     SimpleTypeChecker.API.runCheck!(ctx)'

# --- Simple Lorenz attractor step (numerical integration) ---

struct LorenzParams
    sigma::Float64
    rho::Float64
    beta::Float64
end

function lorenz_step(state::Vector{Float64}, p::LorenzParams, dt::Float64)::Vector{Float64}
    x = @inbounds state[1]
    y = @inbounds state[2]
    z = @inbounds state[3]
    dx = p.sigma * (y - x)
    dy = x * (p.rho - z) - y
    dz = x * y - p.beta * z
    return [x + dx * dt, y + dy * dt, z + dz * dt]
end

function simulate(steps::Int, dt::Float64)::Vector{Vector{Float64}}
    p = LorenzParams(10.0, 28.0, 8.0/3.0)
    state = [1.0, 1.0, 1.0]
    history = Vector{Float64}[]
    i = 0
    while i < steps
        push!(history, state)
        state = lorenz_step(state, p, dt)
        i = i + 1
    end
    return history
end

# --- Higher-order helpers — exercise lambdas ---

function magnitudes(history::Vector{Vector{Float64}})::Vector{Float64}
    return map(s -> sqrt(s[1]*s[1] + s[2]*s[2] + s[3]*s[3]), history)
end

function above_threshold(history::Vector{Vector{Float64}}, threshold::Float64)::Vector{Vector{Float64}}
    # Same closure-capture limitation as `rescale_z` (see below). With the
    # broader dynamic-dispatch detection, capturing `threshold` and routing
    # the comparison through `filter`'s machinery surfaces a real dynamic
    # `>(::Float64, ::Any)` deep in the call tree — caused by the checker
    # losing the captured-variable type at module-level lambda eval, not by
    # the user's source. For-loop hoists the captured value to keep the
    # comparison statically typed.
    result = Vector{Vector{Float64}}()
    for s in history
        if s[3] > threshold
            push!(result, s)
        end
    end
    return result
end

function rescale_z(history::Vector{Vector{Float64}}, factor::Float64)::Vector{Vector{Float64}}
    # NOTE on a current limitation: lambdas that capture a local variable
    # from the enclosing function (here `factor`) cause Julia's inferencer
    # to widen the result type of `map(...)` to Vector{Any}, because the
    # checker eval's the lambda at the module level where `factor` is not
    # yet bound. Workaround: hoist the captured value out using a for-loop.
    result = Vector{Float64}[]
    for s in history
        push!(result, [s[1], s[2], s[3] * factor])
    end
    return result
end

# --- `<:` and `::` exercise ---

function is_floatvec_subtype()::Bool
    return Vector{Float64} <: AbstractVector
end

function cast_to_int(x::Float64)::Int
    rounded = round(Int, x)
    return rounded::Int
end

function sum_inbounds(v::Vector{Float64})::Float64
    total = 0.0
    i = 1
    while i <= length(v)
        total = total + @inbounds v[i]
        i = i + 1
    end
    return total
end

end # module
