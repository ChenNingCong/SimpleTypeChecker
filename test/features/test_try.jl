# try / catch / finally support.
#
# The checker treats variables defined inside any of the four sub-blocks
# (try / catch / else / finally) as local to that block — an exception
# can fire partway, so a binding inside the block may never actually
# materialize. Only the catch variable lives one level deep (visible in
# the catch body only, not after).

@testset "try / catch / finally" begin
    expect_clean("try/try_basic_ok.jl",         :TryBasicOk)
    expect_clean("try/try_no_catch_var_ok.jl",  :TryNoCatchVarOk)
    expect_clean("try/try_finally_ok.jl",       :TryFinallyOk)
    expect_clean("try/try_catch_var_scoped.jl", :TryCatchVarScoped)
end
