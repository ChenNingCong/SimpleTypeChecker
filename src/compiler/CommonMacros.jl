# Hardcoded result-type table for the small set of standard library
# string-and-cmd macros that show up everywhere in real packages. The
# generic macro-expansion path tries to walk the macro's body and loses
# span / tracing info doing so — for these well-known macros we instead
# skip expansion and directly stamp the well-known result type onto a
# FlowNode. The analyzer side calls `builtinMacroResultType(rtctx, name)`;
# if it gets back a non-Bottom CompileType, that's the macro's result
# type. Bottom means "we don't know this macro, fall through to the
# generic warning path".
#
# The table is intentionally narrow. We only list macros where:
#   - the result type is statically known regardless of the literal
#     contents (i.e. it never depends on the argument string),
#   - failing to model it produces cascading errors downstream
#     (e.g. `match(::Nothing, ::String)` after `r"..."` is treated as
#     `nothing`).
#
# Adding a new macro is a one-line change at the bottom of this `if`
# chain — but only when the criteria above hold. For macros whose result
# depends on the argument (e.g. `Base.@kwdef`, `@enum`), don't add them
# here; those require real expansion.

@nocheck function builtinMacroResultType(name::String)::CompileType
    if name == "@r_str"
        return makeType(Regex)
    elseif name == "@b_str"
        # `b"..."` returns a `Base.CodeUnits{UInt8,String}` which is
        # `AbstractVector{UInt8}` — widen to that for consumers that
        # expect `Vector{UInt8}`-shaped data.
        return makeType(Base.CodeUnits{UInt8, String})
    elseif name == "@raw_str"
        return makeType(String)
    elseif name == "@v_str"
        return makeType(VersionNumber)
    elseif name == "@s_str"
        return makeType(Base.SubstitutionString{String})
    elseif name == "@cmd"
        return makeType(Cmd)
    elseif name == "@big_str"
        return makeType(BigInt)
    elseif name == "@artifact_str"
        # Pkg.@artifact_str returns a String (a filesystem path) — but
        # we resolve it by name to avoid forcing a Pkg dependency at the
        # driver-import seam.
        return makeType(String)
    elseif name == "@MIME_str"
        return makeType(MIME)
    elseif name == "@html_str"
        return makeType(Docs.HTML{String})
    elseif name == "@text_str"
        return makeType(Docs.Text{String})
    elseif name == "@doc_str"
        return makeType(Base.Docs.DocStr)
    else
        # Sentinel: not a known builtin. Caller falls through.
        return makeBottomTypeOf()
    end
end
