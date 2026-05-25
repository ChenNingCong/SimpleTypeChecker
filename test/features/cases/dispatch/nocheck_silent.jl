module DispatchNocheck
using SimpleTypeChecker.Utility: @nocheck
# @nocheck opts the whole body out of checking — no diagnostics should land
# even though the body has a real dynamic-dispatch call.
@nocheck function silent()::Nothing
    println("hello")
    return nothing
end
end
