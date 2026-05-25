# Deep dynamic-dispatch detection (InferenceErrorTopLevelDispatch).
#
# This whole file exercises the OPT-IN recursive detection — every case
# passes `deep=true`. The shallow default (only flag when Julia widened
# the return type to `Any`) is exercised implicitly by the rest of the
# feature tests, which never set the flag.
#
# The deep mode walks Julia's inferred IR for every top-level user call
# and looks for post-inference `:call` statements whose callee isn't a
# Builtin / IntrinsicFunction (and whose result isn't a Const, which
# would mean constprop folded the dispatch). If any such call exists
# anywhere in the transitive tree, we attribute the blame to the user's
# top-level callsite — that's the user-actionable point. Code below this
# layer (Base, third-party) is out of the user's reach.

const DYN_ERR = "InferenceErrorTopLevelDispatch"
const ABS_ERR = "InferenceErrorFunCallArgs"

@testset "dyn-dispatch detection (deep / recursive mode)" begin
    # Positive cases — code that is fully resolvable, should NOT flag.
    expect_clean("dispatch/pure_arith_ok.jl",      :DispatchPureArith;     deep=true)
    expect_clean("dispatch/user_chain_ok.jl",      :DispatchUserChain;     deep=true)
    expect_clean("dispatch/tuple_ok.jl",            :DispatchTuple;         deep=true)
    expect_clean("dispatch/getfield_ok.jl",         :DispatchGetfield;      deep=true)
    expect_clean("dispatch/struct_field_ok.jl",     :DispatchStructField;   deep=true)
    expect_clean("dispatch/callable_struct_ok.jl",  :DispatchCallableStruct;deep=true)
    expect_clean("dispatch/kwarg_ok.jl",            :DispatchKwargOk;       deep=true)
    # Negative control: looks suspicious (multi-arg string), but this exact
    # specialization resolves statically — make sure the checker doesn't
    # false-positive even in deep mode.
    expect_clean("dispatch/string_concat_ok.jl",    :DispatchStringConcat;  deep=true)

    # Negative cases — concrete args at user site, but dynamic dispatch
    # deep in the chain (open-world IO behind print/println; default-IO
    # indirection in readline). Only fire in deep mode.
    expect_error("dispatch/println_bad.jl",  :DispatchPrintln,  DYN_ERR; deep=true)
    expect_error("dispatch/print_bad.jl",    :DispatchPrint,    DYN_ERR; deep=true)
    expect_error("dispatch/readline_bad.jl", :DispatchReadline, DYN_ERR; deep=true)

    # @nocheck must suppress everything, including the new dyn-dispatch error.
    expect_clean("dispatch/nocheck_silent.jl", :DispatchNocheck; deep=true)

    # Precedence — when the user's own call site has an abstract arg
    # (here `stdout :: IO`), the abstract-arg check fires first; we never
    # reach the dyn-dispatch check at this site. Assert the right error
    # fires AND that the dyn-dispatch error does NOT.
    @testset "abstract-arg path takes precedence over dyn-dispatch" begin
        out = check_case("dispatch/show_abstract_arg.jl", :DispatchShowAbstractArg; deep=true)
        @test has_error_matching(out, ABS_ERR)
        @test !has_error_matching(out, DYN_ERR)
    end

    # Shallow vs. deep — the same fixture (println) fires in deep mode but
    # NOT in shallow mode (println returns Nothing, not Any).
    @testset "shallow mode does not flag println" begin
        out = check_case("dispatch/println_bad.jl", :DispatchPrintln; deep=false)
        @test !has_error_matching(out, DYN_ERR)
    end
end
