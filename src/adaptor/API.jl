import ..SimpleTypeChecker
import ..SimpleTypeChecker.Utility.@nocheck
import ..SimpleTypeChecker.Inference.GlobalContext
import ..SimpleTypeChecker.Inference.addFile!
import ..SimpleTypeChecker.Inference.runCheck!

function check(ctx::SimpleTypeChecker.Inference.GlobalContext, f, tt)
    ci = Base.code_typed(f,tt)[1].first
    if ci isa Core.CodeInfo
        mi = ci.parent
        push!(ctx.queue, mi)
        runCheck!(ctx)
    end
    return
end

export addFile!, runCheck!, GlobalContext, @nocheck, check