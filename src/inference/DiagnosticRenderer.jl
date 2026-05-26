# rustc-shape diagnostic IR + renderer.
#
# Every `displayError*` builds a `Diagnostic` value and hands it to
# `renderDiagnostic`. The renderer owns the visual layout — gutter, line
# numbers, carets, secondary `note:` blocks, `help:` lines — so individual
# emitters never touch presentation details. See rustc-dev-guide
# "Errors and lints" for the structural reference.
#
# Output shape:
#
#   error[CODE]: headline
#      --> file:line:col
#       |
#    NN |     <source line>
#       |     ^^^^ <label on the primary span>
#       |
#   note: <secondary headline>
#      --> file:line:col
#       |
#    MM |     <source line>
#       |     ---- <label on the secondary span>
#       |
#   help: <actionable suggestion>

import ..SyntaxDefinition.convert2line

# Style of an underlined span. Primary uses `^`, secondary uses `-` —
# same convention as rustc, easy to skim apart on a terminal.
struct LabeledSpan
    loc::Location
    label::String
    primary::Bool
end

struct DiagNote
    headline::String
    spans::Vector{LabeledSpan}
end

struct Diagnostic
    # Stable identifier — `JTC-DISPATCH`, `JTC-NO-FIELD`. Future
    # `--explain JTC-DISPATCH` lookups key on this. Headlines drift across
    # versions; codes don't.
    code::String
    # `error` / `warning`. No `note` severity at the top level — notes are
    # always children of a primary diagnostic.
    isError::Bool
    headline::String
    primary::Vector{LabeledSpan}
    notes::Vector{DiagNote}
    helps::Vector{String}
end

# Repeat `c` `n` times into a String. Used for gutter padding + carets.
# Kept tiny and concretely typed so self-check sees no dynamic dispatch.
function _diagRepeat(c::Char, n::Int)::String
    if n <= 0
        return ""
    end
    buf = IOBuffer()
    i = 1
    while i <= n
        write(buf, c)
        i += 1
    end
    return String(take!(buf))
end

# Extract the source bytes for one 1-based line as a String, trailing
# newline / carriage-return stripped. Returns "" when the line is out of
# range — defensive; we don't want a diagnostic to crash the checker.
function _diagExtractLine(file::JuliaSyntax.SourceFile, line::Int)::String
    starts = file.line_starts
    code = file.code
    if line < 1 || line > length(starts)
        return ""
    end
    lo = Int(starts[line])
    hi::Int = line < length(starts) ? Int(starts[line + 1]) - 1 : sizeof(code)
    while hi >= lo && hi <= sizeof(code) && (code[hi] == '\n' || code[hi] == '\r')
        hi -= 1
    end
    if hi < lo
        return ""
    end
    return String(SubString(code, lo, hi))
end

# Pretty filename for the `-->` line. Mirrors what `formatLocation`
# already does for the legacy path so paths look consistent.
function _diagFilename(file::JuliaSyntax.SourceFile)::String
    ff = file.filename
    if ff isa Nothing
        return "(none)"
    end
    return ff
end

# Write `   --> file:line:col` + the gutter + source line + caret/underline +
# label. `gutterWidth` is determined by the line number's digit count so
# adjacent spans line up.
function _renderSpan(io::ErrorLogger, span::LabeledSpan)::Nothing
    loc = span.loc
    file = loc.file
    startPos = convert2line(file.line_starts, Int(loc.span[1]))
    line = startPos.first
    col = startPos.second
    endPos = convert2line(file.line_starts, Int(loc.span[2]))
    endLine = endPos.first
    endCol = endPos.second

    lineNumStr = string(line)
    gutterWidth = length(lineNumStr)
    # rustc gutter convention: content line is `<space><line>< space>|`, so
    # bar lines need (gutterWidth + 2) leading spaces to put the `|` at the
    # same column. (Old code used + 1 and produced an off-by-one shift.)
    pad = _diagRepeat(' ', gutterWidth + 2)

    writelog!(io, "   --> ")
    writelog!(io, _diagFilename(file))
    writelog!(io, ':')
    writelog!(io, line)
    writelog!(io, ':')
    writelog!(io, col)
    writelogln!(io)

    # opening gutter line (purely visual; separates the file ref from code)
    writelog!(io, pad)
    writelogln!(io, "|")

    # source line
    src = _diagExtractLine(file, line)
    writelog!(io, ' ')
    writelog!(io, lineNumStr)
    writelog!(io, " | ")
    writelogln!(io, src)

    # underline. Single-line spans get end_col - col carets; multi-line
    # spans run to the end of the rendered source line.
    writelog!(io, pad)
    writelog!(io, "| ")
    if col > 1
        writelog!(io, _diagRepeat(' ', col - 1))
    end
    local underlineLen::Int
    if endLine == line
        # Spans are inclusive byte ranges (matches `code[span[1]:span[2]]`),
        # so the caret count is `endCol - col + 1`.
        underlineLen = endCol - col + 1
        if underlineLen < 1
            underlineLen = 1
        end
    else
        srcLen = length(src)
        underlineLen = srcLen - (col - 1)
        if underlineLen < 1
            underlineLen = 1
        end
    end
    caret = span.primary ? '^' : '-'
    writelog!(io, _diagRepeat(caret, underlineLen))
    if span.label != ""
        writelog!(io, ' ')
        writelog!(io, span.label)
    end
    writelogln!(io)

    # closing gutter — visually closes the snippet, leaves a one-line
    # breather before the next section
    writelog!(io, pad)
    writelogln!(io, "|")
    return nothing
end

# Build + render a Diagnostic for a SyntaxError-style failure
# (`reportASTError`, `reportUnimplementedASTError`, or any future
# pre-inference rejection). The legacy emission printed only
# "file:line:col\n<msg>" — no source line, no error code, no hint at
# what shape the checker expected. This puts a primary span at the
# offending AST so the reader sees the actual code, tags it with
# `JTC-UNSUPPORTED` (checker hasn't implemented this AST shape) vs
# `JTC-SYNTAX` (rejected outright).
function renderSyntaxError(io::ErrorLogger, ast::JuAST, msg::String)::Nothing
    isUnsupported = startswith(msg, "Unsupported")
    code = isUnsupported ? "JTC-UNSUPPORTED" : "JTC-SYNTAX"
    primary = LabeledSpan[LabeledSpan(ast.loc, "", true)]
    helps = String[]
    if isUnsupported
        # Tell the reader this is a checker limitation, not a bug in
        # their code — analogous to how rustc tells you when something
        # is "currently unsupported" vs "this is wrong."
        push!(helps,
            "this is a checker limitation — the AST shape is valid Julia "
          * "but the checker has not implemented it yet. Rewrite using a "
          * "supported form (e.g. typed-comprehension `T[expr for x in xs]` "
          * "→ explicit `map`-with-typed-collect), or add `@nocheck` to "
          * "opt the enclosing method out.")
    end
    diag = Diagnostic(code, true, msg, primary, DiagNote[], helps)
    renderDiagnostic(io, diag)
    return nothing
end

function renderDiagnostic(io::ErrorLogger, diag::Diagnostic)::Nothing
    writelog!(io, diag.isError ? "error[" : "warning[")
    writelog!(io, diag.code)
    writelog!(io, "]: ")
    writelogln!(io, diag.headline)

    for s in diag.primary
        _renderSpan(io, s)
    end

    for note in diag.notes
        writelog!(io, "note: ")
        writelogln!(io, note.headline)
        for s in note.spans
            _renderSpan(io, s)
        end
    end

    for h in diag.helps
        writelog!(io, "help: ")
        writelogln!(io, h)
    end
    return nothing
end
