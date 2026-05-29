module NcatOk

# N-dimensional concatenation (`;;`, `;;;`, ...). JuliaSyntax packs the
# dim into the upper byte of the node's flag — dim 2 → flag 512,
# dim 3 → flag 768. Previously fired `Bad flag : … ncat : 512` because
# the adaptor's whitelist only knew the lower-byte boolean flags.

function row_stack()::Matrix{Int}
    # `;;` = dim 2 ncat — like hcat.
    return [1 ;; 2 ;; 3]
end

function box_stack()::Array{Int, 3}
    # `;;;` = dim 3 ncat — stacks along the third axis.
    return [1 ;;; 2]
end

function typed_dim2()::Matrix{Float64}
    # `T[... ;; ...]` = typed_ncat — same flag layout with the leading
    # type tag.
    return Float64[1.0 ;; 2.0 ;; 3.0]
end

function dummy(x::Matrix{Int})::Int
    return length(x)
end

function caller()::Int
    return dummy(row_stack())
end

end
