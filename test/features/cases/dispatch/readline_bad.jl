module DispatchReadline
# `readline()` — even with zero args — internally calls back through a
# SlotNumber/Argument callee (the default-IO indirection) whose target
# isn't a Builtin/Intrinsic. This exercises the SSA/Slot-callee branch
# of `_is_static_callee`, not just the GlobalRef path.
function bad()::String
    return readline()
end
end
