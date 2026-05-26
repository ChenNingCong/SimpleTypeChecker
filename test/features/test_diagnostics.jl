# Format-level tests for the rustc-shape diagnostic renderer.
#
# These tests check what users actually *see* — error code, source-line
# gutter, secondary `note:` blocks, `help:` suggestions. The other test
# files assert on exception types (does the right diagnostic fire?); this
# file asserts the diagnostic is rendered intelligibly.

@testset "diagnostics rendering (rustc-shape)" begin
    @testset "JTC-DISPATCH: abstract argument with field-origin" begin
        s = rendered_output("error_format/dispatch_field_origin.jl",
                            :DispatchFieldOrigin)

        # Error code + headline
        @test occursin("error[JTC-DISPATCH]", s)
        @test occursin("call has an argument of abstract type", s)

        # Primary span names the variable, not a slot index
        @test occursin("argument `vv` has type Any", s)

        # Gutter renders the source line for the call site
        @test occursin("consume(vv,", s)

        # Caret line uses `^` for primary spans
        @test occursin("^^", s)

        # Secondary `note:` block points at the origin
        @test occursin("note:", s)
        @test occursin("`vv` became Any here", s)
        # Label names *which field* and *of what receiver* with what
        # declared type — far more actionable than the old generic
        # "field access `.val`" line. Holder.val is declared as Any
        # (no annotation in the fixture), so the label must spell that
        # out.
        @test occursin("`.val`", s)
        @test occursin("Holder.val", s)
        @test occursin("::Any", s)

        # Origin span shows its source line
        @test occursin("vv = h.val", s)

        # Secondary spans use `-` underline. For a property-access origin
        # we narrow the underline to just the `.field` segment, so the
        # underline length matches `.val` (4 chars).
        @test occursin("----", s)
        # …but NOT the broader `h.val` (5+ chars) — would mean we forgot to
        # narrow and re-introduced the "which field?" ambiguity.
        @test !occursin("-----", s)

        # Two rustc-style help options — guard at the call site, or fix
        # the struct. Tight imperatives, no info duplicated from the label.
        @test occursin("help:", s)
        @test occursin("vv isa T", s)
        @test occursin("fix the struct", s)
        # Field-decl is named once (in the label), not in the help.
        # `Holder.val` may appear in the label; the help refers to "the
        # field" instead of repeating the name.
        @test !occursin("declare `Holder.val`", s)

        # Checker-internal frame line MUST NOT appear by default
        @test !occursin("DispatchFieldOrigin.caller(::Holder)", s)
        @test !occursin("[dispatch]", s)
    end
end
