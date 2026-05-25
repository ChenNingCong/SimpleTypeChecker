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
    # Variables assigned in BOTH try and catch must propagate as
    # initialized past the try/catch.
    expect_clean("try/try_both_branches_assign_ok.jl", :TryBothBranchesAssignOk)
    # When the catch always returns/rethrows (Bottom), try's
    # post-context propagates so a var assigned at the end of try is
    # considered initialized.
    expect_clean("try/try_catch_returns_ok.jl", :TryCatchReturnsOk)
end
