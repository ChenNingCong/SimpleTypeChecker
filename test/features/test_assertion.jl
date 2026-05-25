# `::` as a standalone expression (type assertion / runtime cast).

@testset "`::` as standalone expression (type assertion)" begin
    expect_clean("assertion/ta_ok.jl", :TaOk)
    expect_error("assertion/ta_bad.jl", :TaBad, "AssignInitIncompatible")
    expect_error("assertion/ta_nontype.jl", :TaNonType, "ApplyTypeNonconst")
end
