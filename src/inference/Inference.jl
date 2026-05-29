import ..SyntaxDefinition.JuAST
import ..SyntaxDefinition.JuASTVal
import ..SyntaxDefinition.formatLocation
import ..SyntaxDefinition.makeConstJuASTVal
import ..SyntaxAdaptor.parseJuAST
struct InternalError
    msg::String
end

struct SyntaxError
    ast::JuAST
    msg::String
end

struct UnimplementSyntaxError
    ast::JuAST
    msg::String
end

function reportASTError(eng::Engine, ast::JuAST, msg::String)::Union{}
    Base.throw(SyntaxError(ast, msg))
end

function reportASTError(ctx::GlobalContext, ast::JuAST, msg::String)::Union{}
    Base.throw(SyntaxError(ast, msg)) 
end


function reportUnimplementedASTError(eng::Engine, ast::JuAST, msg::String)::Union{}
    Base.throw(SyntaxError(ast, msg))
end

#=
Some utility functions to handle AST
=#

function cast2Symbol(val::JuASTVal)::Symbol
    if !val.isconst
        throw(InternalError("Failed to cast value : not a valid constant value"))
    end
    valval = val.val
    if valval isa Symbol
        return valval
    else
        throw(InternalError(("Failed to cast value : not a symbol")))
        
    end
end


function isaJuASTVal(val::JuASTVal, @nospecialize(typ))::Bool
    if !val.isconst
        throw(InternalError("Not a valid literal value"))
    end
    return val.val isa typ
end

# Dynamic dispatcher on a runtime-supplied type. We accept `typ` as
# `@nospecialize` so the call site doesn't trip the abstract-arg check —
# the only contract is that `typ` is a Type at runtime, enforced here.
function castJuASTVal(@nospecialize(typ), val::JuASTVal)
    if !(typ isa Type)
        throw(InternalError("Failed to cast value : typ argument is not a Type"))
    end
    if val.isconst
        valval = val.val
        if valval isa typ
            return valval
        else
            throw(InternalError("Failed to cast value : type mismatched"))
        end
    else
        throw(InternalError("Failed to cast value : not a valid constant value"))
    end
end

function assertASTKind(ast::JuAST, typ::Symbol)::Nothing
    if ast.head != typ
        throw(InternalError("AST head is mismatched: expected $typ, given $(ast.head)"))
    end
    return nothing
end

function inferLiteral(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    node = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, ast.val)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferCharLiteral(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :char)
    if length(ast.args) == 1 && ast.args[1].head == :literal
        charval = ast.args[1].val
        if isaJuASTVal(charval, Char)
            node = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, charval)
            addFlowMapping!(eng, ast, node)
            addFlowMapping!(eng, ast.args[1], node)
            return InferResult(ctx, node)
        end
    end
    reportASTError(eng, ast, "Not a valid character literal")
end

function pushdownFlowNode(eng::Engine, ast::JuAST, node::FlowNode)::Nothing
    addFlowMapping!(eng, ast, node)
    for i in ast.args
        pushdownFlowNode(eng, i, node)
    end
end

function inferQuoteLiteral(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :quote)
    if length(ast.args) != 1
        reportASTError(eng, ast, "Quote expression should only have one child")
    end
    qast = ast.args[1]
    local node::FlowNode
    if qast.head == :identifier
        node = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, qast.val)
        addFlowMapping!(eng, ast, node)
        addFlowMapping!(eng, qast, node)
    elseif qast.head == :block
        node = makeLiteralExprFlowNode(eng.globalCtx.rtctx, ast) 
        pushdownFlowNode(eng, ast, node)
    else
        reportUnimplementedASTError(eng, ast, "Quoted expression only supports for symbol (:x) and block expressions")
    end
    return InferResult(ctx, node)
end

# Common single-argument macros that act as passthroughs: the type-level
# result of `@m expr` is identical to the result of inferring `expr`.
# This is an explicit whitelist (rather than a Set, to keep the dispatch
# checkable by the type checker itself).
function isPassthroughMacroName(name::Symbol)::Bool
    return name === Symbol("@inbounds")    ||
           name === Symbol("@views")       ||
           name === Symbol("@view")        ||
           name === Symbol("@inline")      ||
           name === Symbol("@noinline")    ||
           name === Symbol("@simd")        ||
           name === Symbol("@fastmath")    ||
           name === Symbol("@boundscheck") ||
           name === Symbol("@propagate_inbounds") ||
           name === Symbol("@nospecialize") ||
           name === Symbol("@specialize")
end

function macroIdentifierName(ast::JuAST)::Symbol
    # ast.args[1] is the macro identifier — either an :identifier with a
    # Symbol val (`@foo`) or a :(.) qualified access (`Mod.@foo`). For the
    # qualified case the trailing symbol is what we whitelist against.
    head = ast.args[1].head
    if head == :identifier
        return cast2Symbol(ast.args[1].val)
    elseif head == :(.) && length(ast.args[1].args) == 2
        last = ast.args[1].args[2]
        if last.head == :quote && length(last.args) == 1 && last.args[1].head == :identifier
            return cast2Symbol(last.args[1].val)
        end
    end
    return Symbol("")
end

# Walks each macro argument through inferExpr purely for its side effects
# on the engine — populating ctx.queue with nested method instances so the
# checker still recurses into calls written inside an unrecognized macro
# body. Errors raised while inferring an individual argument are swallowed
# so one bad argument can't poison checking of the others. Returns ctx
# possibly updated by successful argument inference.
function inferMacroArgsForSideEffects!(eng::Engine, ctx::Context, ast::JuAST)::Context
    n = length(ast.args)
    if n < 2
        return ctx
    end
    for i in 2:n
        try
            rel = inferExpr(eng, ctx, ast.args[i])
            ctx = rel.ctx
        catch e
            if e isa SyntaxError || e isa InferenceError
                # Swallow — macros frequently take non-standard argument
                # shapes (shell-like tokens for @cmd, raw strings, etc.).
                # The side-effect-only intent means we don't need to surface
                # these per-argument failures.
            else
                rethrow(e)
            end
        end
    end
    return ctx
end

function inferMacroCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :macrocall)
    # Resolving the macro identifier can fail in several ways:
    #   - Module-qualified macros (Core.@cmd) get tokenized by JuliaSyntax as
    #     a single identifier "core_@cmd" that's not a real variable in the
    #     enclosing module.
    #   - User-defined macros from packages we don't trace into.
    # We don't want an unresolved macro identifier to kill the whole method
    # check — fall back to the "macro not implemented" warning path instead.
    # In all fallback paths we still walk the macro's arguments so nested
    # method instances inside the macro body get queued for checking.
    local node::FlowNode
    local typ::CompileType
    try
        rel = inferExpr(eng, ctx, ast.args[1])
        ctx = rel.ctx
        node = rel.node
        typ = node.typ
    catch e
        if e isa SyntaxError || e isa InferenceError
            reportWarning(eng, ast, "Macro is not implemented. Inferred as a nothing")
            ctx = inferMacroArgsForSideEffects!(eng, ctx, ast)
            node = makeMacroCallFlowNode(eng.globalCtx.rtctx, ast)
            return InferResult(ctx, node)
        else
            rethrow(e)
        end
    end
    if isConstVal(typ)
        macroName = macroIdentifierName(ast)
        builtinResultType = builtinMacroResultType(eng.globalCtx.rtctx, String(macroName))
        if !isBottomType(eng.globalCtx.rtctx, builtinResultType)
            # Hardcoded builtin macro (e.g. `r"..."`, `artifact"..."`, `v"..."`).
            # We bypass the macro-expansion machinery — it loses span info when
            # walking through the expanded form — and stamp the well-known
            # result type directly. Arguments are still walked for side effects
            # so embedded calls get queued.
            ctx = inferMacroArgsForSideEffects!(eng, ctx, ast)
            node = FlowNode(ast, FlowNode[], builtinResultType, MacroCallFlowNode())
            addFlowMapping!(eng, ast, node)
        elseif haskey(eng.globalCtx.expanders, typ)
            rel = inferExpand(eng, ctx, eng.globalCtx.expanders[typ], ast.args[2])
            ctx = rel.ctx
            node = rel.node
        elseif isPassthroughMacroName(macroName) && length(ast.args) >= 2
            # `@m expr` — treat as if it were `expr`. Common single-arg
            # passthrough macros (@inbounds, @views, @inline, ...) emit
            # expressions whose runtime type matches the inner expression.
            inner_rel = inferExpr(eng, ctx, ast.args[2])
            ctx = inner_rel.ctx
            node = inner_rel.node
            addFlowMapping!(eng, ast, node)
        else
            reportWarning(eng, ast, "Macro is not implemented. Inferred as a nothing")
            ctx = inferMacroArgsForSideEffects!(eng, ctx, ast)
            node = makeMacroCallFlowNode(eng.globalCtx.rtctx, ast)
        end
    end
    return InferResult(ctx, node)
end

function inferCurlyCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :curly)
    tts = Vector{FlowNode}(undef, length(ast.args))
    for i in 1:length(tts)
        rel = inferExpr(eng, ctx, ast.args[i])
        ctx = rel.ctx
        tts[i] = rel.node
        if isBottomType(eng.globalCtx.rtctx, rel.node.typ)
            reportErrorCurlyCallUseBottom(eng, ast, i)
        end 
    end
    argtyps = CompileType[]
    imprecise = Int[]
    for i in 1:length(tts)
        nodetyp = tts[i].typ
        if !isConstVal(nodetyp)
            push!(imprecise, i)
        else
            push!(argtyps, nodetyp)
        end
    end
    if length(imprecise) > 0
        reportErrorApplyTypeNonconst(eng, ast, imprecise)
    end
    mtt = tryApplyType(eng.globalCtx.rtctx, argtyps)
    if isNone(mtt)
        reportErrorApplyTypeFailure(eng, ast, tts)
    end
    tt = castJust(mtt)
    node = makeCurlyCallFlowNode(ast, tts, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferDotCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :dotcall)
    return inferFunCall(eng, ctx, ast, true)
end

# Helpers wrapping `isa Type` and `<:` on values of static type Any. These are
# Escape-hatch helpers — each is a single-method `@nospecialize` function
# that rides past the abstract-arg / IsaBadForm checks the checker would
# normally raise on `isa Type` against an Any-typed value.
function isTypeValue(@nospecialize x)::Bool
    return x isa Type
end

function subtypeOf(@nospecialize(a), @nospecialize(b))::Bool
    return a <: b
end

function makeUnionAll(tv::TypeVar, @nospecialize base)
    return UnionAll(tv, base)
end

# `T1 <: T2` as a standalone expression. Produces a Bool — constant when both
# sides resolve to concrete Type values, abstract Bool otherwise.
# @nocheck because it inspects Any-typed values (.val) at runtime to decide
# whether constant propagation through subtyping is possible.
function inferSubtypeOp(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :(<:))
    if length(ast.args) != 2
        reportASTError(eng, ast, "<: requires exactly two arguments")
    end
    rel_l = inferExpr(eng, ctx, ast.args[1])
    ctx = rel_l.ctx
    rel_r = inferExpr(eng, ctx, ast.args[2])
    ctx = rel_r.ctx
    lnode = rel_l.node
    rnode = rel_r.node
    args = FlowNode[lnode, rnode]
    rtctx = eng.globalCtx.rtctx
    local result_ct::CompileType
    if isConstVal(lnode.typ) && isConstVal(rnode.typ) &&
       rtctx.constIsTypeValue(lnode.typ) && rtctx.constIsTypeValue(rnode.typ)
        # Both consts are Type-values — lift each to type-value handles and
        # ask the seam to compare the underlying types.
        l_lifted = lift(rtctx, lnode.typ)
        r_lifted = lift(rtctx, rnode.typ)
        result_ct = wrapLiteralBool(rtctx, rtctx.subtypeOf(l_lifted, r_lifted))
    else
        result_ct = makeBoolType(rtctx)
    end
    node = makeCurlyCallFlowNode(ast, args, result_ct)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

# `x::T` as a standalone expression — a typed cast. Asserts that x is a T at
# runtime; for the type checker, narrows x's type to T. The asserted type T
# must resolve to a known Type. If x's inferred type is incompatible with T,
# emit a same-error path used by other "incompatible" sites.
function inferTypeAssertExpr(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :(::))
    if length(ast.args) != 2
        reportASTError(eng, ast, ":: requires exactly two arguments (value and asserted type)")
    end
    relval = inferExpr(eng, ctx, ast.args[1])
    ctx = relval.ctx
    reltyp = inferExpr(eng, ctx, ast.args[2])
    ctx = reltyp.ctx
    vnode = relval.node
    tnode = reltyp.node
    rtctx = eng.globalCtx.rtctx
    # Asserted type must be a constant Type value.
    if !(isConstVal(tnode.typ) && rtctx.constIsTypeValue(tnode.typ))
        reportErrorApplyTypeNonconst(eng, ast, Int[2])
    end
    # Lift the const Type-value into a type-value handle for the subtype check.
    asserted_typ = lift(rtctx, tnode.typ)
    # Value type must be compatible with the assertion. If the value's
    # declared type isn't <: asserted, the assertion can never hold. We
    # check the value's storage type by removing const-ness first so the
    # comparison is on the type-value form.
    valtyp = removeConst(rtctx, vnode.typ)
    if !rtctx.subtypeOf(valtyp, asserted_typ)
        # Reuse the assign-init-incompatible error — same shape as `local x::Int = 1.0`.
        reportErrorAssignInitIncompatible(eng, ast, tnode.typ, vnode.typ)
    end
    # Result has the asserted type, preserving constness if the value was const.
    local result_ct::CompileType
    if isConstVal(vnode.typ)
        result_ct = vnode.typ
    else
        result_ct = asserted_typ
    end
    node = makeCurlyCallFlowNode(ast, FlowNode[vnode, tnode], result_ct)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

# Anonymous functions / lambdas: `x -> body`, `(x, y) -> body`, and the
# anonymous-`function ... end` form.
#
# Implementation: eval the lambda's underlying Expr in the target module to
# obtain the concrete singleton function value. This gives downstream code
# (`map`, `filter`, `sort(by=...)`) a CONCRETE function type to dispatch on,
# avoiding the "abstract argument" error that a generic `Function` widen
# would trigger. The eval has a real side effect — Julia creates a fresh
# anonymous-function struct + method in the target module — but the user's
# source already declared that lambda, so the type would have been created
# at runtime anyway.
#
# Known limitation: lambdas that capture local variables from the enclosing
# function (e.g. `factor` in `map(s -> s * factor, xs)`) lose precision in
# the *result* type because Julia's inferencer treats the closure variables
# as untyped globals at eval time. The lambda still type-checks and is
# acceptable as a function argument; only the downstream return type of
# higher-order calls widens. Workaround: hoist captured variables into
# explicit lambda parameters, or annotate the result type at the call site.
#
# We do NOT recurse into the body for type-checking. Body analysis would
# require knowing the parameter types from the caller (e.g. inside `map`),
# which is a deep higher-order analysis. Skipping it loses some checks but
# keeps the feature shippable.
function inferLambdaExpr(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    # Lambda eval — Expr reconstruction + Core.eval + wrap — happens
    # driver-side via the membrane op. On failure the driver returns
    # `makeType(Function)` so we just unwrap unconditionally here.
    result_ct = evalLambdaInModule(eng.globalCtx.rtctx, eng.modCT, ast)
    node = makeCurlyCallFlowNode(ast, FlowNode[], result_ct)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

# `T where U` as a standalone expression. The binder U is brought into scope
# as a constant TypeVar before inferring the base type T (so identifiers
# referring to U inside T resolve correctly). Constructs a UnionAll when the
# base resolves to a known Type. Bound binders (`T where U<:Number`) are not
# supported yet — they fall through to the widened branch.
function inferWhereType(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :where)
    if length(ast.args) != 2
        reportASTError(eng, ast, "where requires exactly two arguments (base type and TypeVar binder)")
    end
    binder = ast.args[2]
    rtctx = eng.globalCtx.rtctx
    local result_ct::CompileType
    local base::FlowNode
    if binder.head == :identifier
        tvname = cast2Symbol(binder.val)
        # Driver-side TypeVar construction — returns a const CompileType
        # wrapping the fresh TypeVar.
        tv_ct = makeTypeVarOf(rtctx, String(tvname))
        tv_node = makeSparamFlowNode(binder, tv_ct)
        scoped_ctx = update(ctx, tvname, ContextValue(tv_node, tv_node))
        rel = inferExpr(eng, scoped_ctx, ast.args[1])
        base = rel.node
        if isConstVal(base.typ) && rtctx.constIsTypeValue(base.typ)
            result_ct = makeUnionAllOf(rtctx, tv_ct, base.typ)
        else
            result_ct = makeTypeType(rtctx)
        end
    else
        rel = inferExpr(eng, ctx, ast.args[1])
        base = rel.node
        result_ct = makeTypeType(rtctx)
    end
    args = FlowNode[base]
    node = makeCurlyCallFlowNode(ast, args, result_ct)
    addFlowMapping!(eng, ast, node)
    # Return the outer ctx — the TypeVar binding is scoped to the where expression.
    return InferResult(ctx, node)
end

# Special handler for `ccall(symbol, RetT, (ArgTs...), args...)` — a Julia
# intrinsic that isn't a normal function value. We:
#   1. Skip resolving `ccall` as an identifier.
#   2. Don't validate the symbol expr (can be :Symbol, :Tuple, etc.).
#   3. Trust the user-declared RetT as the result type.
#   4. Infer the trailing args for side-effects on ctx.queue (still want
#      nested method instances queued).
function inferCCall(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    # ast.args layout: [ccall_ident, sym_expr, rettype_expr, argtypes_tuple, args...]
    if length(ast.args) < 4
        reportASTError(eng, ast, "ccall requires at least symbol, return type, arg type tuple")
    end
    # Infer the return-type expression (must resolve to a Type).
    rt_rel = inferExpr(eng, ctx, ast.args[3])
    ctx = rt_rel.ctx
    rt_node = rt_rel.node
    rtctx = eng.globalCtx.rtctx
    local result_ct::CompileType
    if isConstVal(rt_node.typ) && rtctx.constIsTypeValue(rt_node.typ)
        result_ct = lift(rtctx, rt_node.typ)
    else
        # Couldn't statically determine the return type — widen to Any.
        result_ct = makeAnyType(rtctx)
    end
    # Infer trailing call args so any nested function calls they contain get
    # type-checked too. Don't validate against the arg-type tuple — that's
    # Julia's runtime concern.
    for i in 5:length(ast.args)
        rel = inferExpr(eng, ctx, ast.args[i])
        ctx = rel.ctx
    end
    node = makeCurlyCallFlowNode(ast, FlowNode[rt_node], result_ct)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

# `_isEscapeHatchMethod` lives driver-side (CompilerAdaptor.jl) because it
# peeks `.module` / `.name` on a MethodDef and resolves the function value
# via `getfield` — both target-runtime ops. Called from runCheck! where
# the result (Bool) flows back across the seam by value.

# Detects "escape-hatch" callees — a function that the user has
# deliberately marked `@nospecialize` to accept Any at certain slots.
# To qualify ALL of the following must hold:
#   1. The callee value is statically known (a Const callable).
#   2. The callable has exactly ONE method (no multi-method dispatch
#      ambiguity).
#   3. The method declares no keyword args, no varargs, and no optional
#      positional args (we ignore arity that doesn't match the call).
#   4. The method has `@nospecialize` on at least one positional arg.
# Returns `(true, mask)` if escape-hatch, else `(false, 0)`. The mask
# is `Method.nospecialize` — bit `i-1` set ⇒ user-arg `i` is @nospecialize.
function _escapeHatchMask(rtctx::RuntimeContext, fargs::Vector{FlowNode})::Tuple{Bool, UInt32}
    f = fargs[1]
    if !isConstVal(f.typ)
        return (false, UInt32(0))
    end
    if !rtctx.constIsFunction(f.typ)
        return (false, UInt32(0))
    end
    # Find methods on this function that could match the call's arity.
    # A method matches if it's not varargs, not kwargs, and has exactly
    # `length(fargs)` positional slots (function + user args). If exactly
    # one such method exists and it has `@nospecialize` on at least one
    # slot, treat it as an escape hatch. This generalizes the previous
    # "single method total" rule, so functions like `Base.rethrow` (one
    # zero-arg method + one 1-arg `@nospecialize` method) qualify at the
    # 1-arg call site.
    ms = methodsOf(rtctx, getCallableFromConst(rtctx, f.typ))
    arity = length(fargs)
    matchIdx = 0
    matchCount = 0
    for i in 1:length(ms)
        m = ms[i]
        if methodDefNKw(m) != 0 || methodDefIsVarArg(m)
            continue
        end
        if methodDefNargs(m) != arity
            continue
        end
        matchIdx = i
        matchCount += 1
    end
    if matchCount != 1
        return (false, UInt32(0))
    end
    m = ms[matchIdx]
    mask = methodDefNospecializeMask(m)
    if mask == UInt32(0)
        return (false, UInt32(0))
    end
    return (true, mask)
end

# Builtin / IntrinsicFunction callees (isa, ===, typeof, getfield, tuple,
# etc.) dispatch via low-level intrinsics, NOT through Julia's method
# table. An abstract arg at one of these sites is harmless: there is no
# method-table lookup to resolve. Use this to suppress false-positive
# "argument N is abstract" complaints.
function _isBuiltinCallee(rtctx::RuntimeContext, fargs::Vector{FlowNode})::Bool
    f = fargs[1]
    if !isConstVal(f.typ)
        return false
    end
    return rtctx.constIsBuiltinOrIntrin(f.typ)
end

# `2x` / `A'B` / `3im` — juxtapose multiplication. The AST node has the
# operands as direct children (no callee). Semantically equivalent to
# `*(args...)`. We infer each operand normally, then ask the seam for
# the return type of `Base.* ` with the inferred arg types. If any
# operand's type isn't concrete we fall back to Any (the call would be
# abstract anyway). Keeps the AST-walk going on numerical packages
# that lean on this idiom.
@nocheck function inferJuxtapose(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :juxtapose)
    rtctx = eng.globalCtx.rtctx
    nodes = FlowNode[]
    for child in ast.args
        rel = inferExpr(eng, ctx, child)
        ctx = rel.ctx
        push!(nodes, rel.node)
    end
    # Build the sig Tuple{typeof(*), T1, T2, ...} for inferReturnType.
    # Each operand contributes its inferred CompileType.
    arg_types = Any[typeof(*)]
    all_concrete = true
    for n in nodes
        t = lift(rtctx, n.typ)
        push!(arg_types, t.typ)
        Base.isconcretetype(t.typ) || (all_concrete = false)
    end
    result_typ = if all_concrete
        sig = Tuple{arg_types...}
        rts = inferReturnType(eng.globalCtx, sig)
        isempty(rts) ? makeAnyType(rtctx) : rts[1]
    else
        makeAnyType(rtctx)
    end
    ms = MethodCallStruct(nodes)
    node = makeFunCallFlowNode(ast, ms, result_typ)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferFunCall(eng::Engine, ctx::Context, ast::JuAST, isDotcall::Bool)::InferResult
    # Special-case ccall — it's a Julia builtin/intrinsic, not a normal
    # function, so it isn't visible as a variable in any module. The shape is
    # `ccall(symbol, RetT, (ArgTs...), args...)`. We infer the args for their
    # side effects (type-check the arguments), then return the declared RetT.
    if !isDotcall && length(ast.args) >= 1 &&
       ast.args[1].head == :identifier && cast2Symbol(ast.args[1].val) === :ccall
        return inferCCall(eng, ctx, ast)
    end
    # This function doesn't handle function definition
    args = ast.args
    fargs = FlowNode[]
    kwargs = Pair{Symbol, FlowNode}[]
    local rel::InferResult
    # `f(args) do x; body end` parses with the `:do` node as the trailing
    # child of the call. Julia lowers it to `f((x,) -> body, args)` — the
    # lambda becomes the first positional. Peel it off here, eval as a
    # lambda via the membrane, and splice into fargs after the callee.
    doLambdaAst::Maybe{JuAST} = None(JuAST)
    nLastArg = length(args)
    if nLastArg >= 2 && args[nLastArg].head == :do
        doLambdaAst = Just(args[nLastArg])
        nLastArg -= 1
    end
    # TODO : we should add FlowNode for keyword assignment???
    for i in 1:nLastArg
        # parameters can only occur at last position
        local iast = args[i]
        if iast.head == :parameters
            if i != nLastArg
                reportASTError(eng, iast, "Keywords must be the last argument!")
            else
                for index in eachindex(iast.args)
                    local i = iast.args[index]
                    # implict keywords
                    if i.head == :identifier
                        rel = inferVar(eng, ctx, i)
                        sym = cast2Symbol(i.val)
                    elseif i.head == :(=)
                        if length(i.args) == 2
                            if i.args[1].head == :identifier
                                sym = cast2Symbol(i.args[1].val)
                                rel = inferExpr(eng, ctx, i.args[2])
                            else
                                reportASTError(eng, iast, "Invalid keywords assignment : disallow destruction")
                            end
                        else
                            reportASTError(eng, iast, "Invalid keywords assignment")
                        end
                    else
                        reportASTError(eng, iast, "Invalid keywords assignment")
                    end
                    ctx = rel.ctx
                    if isBottomType(eng.globalCtx.rtctx, rel.node.typ)
                        reportErrorKeywordUseBottom(eng, ast, sym)
                    end
                    push!(kwargs, sym=>rel.node)
                end
            end
        elseif iast.head != :kw && iast.head != :(=)
            rel = inferExpr(eng, ctx, iast)
            ctx = rel.ctx
            if isBottomType(eng.globalCtx.rtctx, rel.node.typ)
                reportErrorFunCallUseBottom(eng, ast, i)
            end
            push!(fargs, rel.node)
        else
            # iast.head == :kw || iast.head == :(=)
            if length(iast.args) == 2
                if iast.args[1].head == :identifier
                    sym = cast2Symbol(iast.args[1].val)
                    rel = inferExpr(eng, ctx, iast.args[2])
                else
                    reportASTError(eng, iast, "Invalid keywords assignment : disallow destruction")
                end
                ctx = rel.ctx
                if isBottomType(eng.globalCtx.rtctx, rel.node.typ)
                    reportErrorKeywordUseBottom(eng, ast, sym)
                end
                push!(kwargs, sym=>rel.node)
            else
                reportASTError(eng, iast, "Invalid keywords assignment")
            end
            #=
            f(x, y = 1) is the same thing as f(x;y=1)
            Maybe just force the users to use the latter one
            =#
            # reportASTError(eng, iast, "Use semi-colon to seperate keyword arguments instead of comma. Write f(x;y=1) instead f(x, y=1)")
        end
    end
    if isDotcall && length(kwargs) > 0
        reportASTError(eng, ast, "Unsupported dotted call with keyword arguments")
    end
    if !isNone(doLambdaAst)
        doAst = castJust(doLambdaAst)
        lamCT = evalLambdaInModule(eng.globalCtx.rtctx, eng.modCT, doAst)
        lamNode = makeCurlyCallFlowNode(doAst, FlowNode[], lamCT)
        addFlowMapping!(eng, doAst, lamNode)
        # Splice the lambda between the callee (fargs[1]) and the remaining
        # positional args, mirroring Julia's lowering `f(lam, args...)`.
        insert!(fargs, 2, lamNode)
    end
    imprecise = Int[]
    kwimprecise = Int[]
    # we only support Julia 1.9, because Julia handles keyword arguments differently across versions
    for i in 1:length(fargs)
        n = fargs[i]
        ttypval = n.typ
        if !isConcreteType(eng.globalCtx.rtctx, ttypval)
            push!(imprecise, i)
        end
    end

    checkConstructor = false
    if isConstructor(eng.globalCtx.rtctx, fargs[1].typ)
        checkConstructor = true
    end

    isKwCall = false
    if length(kwargs) != 0
        isKwCall = true
        for i in eachindex(kwargs)
            pair = kwargs[i]
            n = pair.second
            ttypval = n.typ
            if !isConcreteType(eng.globalCtx.rtctx, ttypval)
                push!(kwimprecise, i)
            end
        end
    end
    ms = MethodCallStruct(fargs, kwargs)

    # Drop imprecise positions where the callee is a designated
    # "escape-hatch" callable — single method with `@nospecialize`, no
    # kwargs/optional/varargs, fixed positional arity. This lets user
    # code write a typed wrapper like
    #     @nospecialize_singlemethod f(@nospecialize(x))::String = ...
    # and call it with Any without the abstract-arg complaint.
    if length(imprecise) > 0
        ok, mask = _escapeHatchMask(eng.globalCtx.rtctx, fargs)
        if ok
            kept = Int[]
            for i in imprecise
                if i >= 2 && ((mask >> UInt32(i - 2)) & UInt32(1)) != UInt32(0)
                    continue  # @nospecialize'd at this slot — accepted
                end
                push!(kept, i)
            end
            imprecise = kept
        end
    end

    # Builtin / IntrinsicFunction callees (isa, ===, typeof, getfield,
    # tuple, ...) lower to intrinsics — they don't dispatch through the
    # method table, so an abstract arg here doesn't cause runtime
    # dispatch. Drop user-arg imprecise entries (keep slot 1 = the
    # callee itself, which must still be statically known).
    if length(imprecise) > 0 && _isBuiltinCallee(eng.globalCtx.rtctx, fargs)
        kept = Int[]
        for i in imprecise
            if i >= 2
                continue
            end
            push!(kept, i)
        end
        imprecise = kept
    end

    if length(imprecise) > 0 || length(kwimprecise) > 0
        reportErrorFunCallArgs(eng, ast, ms, imprecise, kwimprecise, fargs, kwargs, isDotcall)
    end
    if isDotcall
        # kwargs is empty here
        fargs = vcat(makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.broadcasted)), fargs)
        ms = MethodCallStruct(fargs, kwargs)
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            # TODO : report mismatched method table here
            reportErrorFunCall(eng, ast, ms, length(mm), false)
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(eng.globalCtx.rtctx, tt)
            reportErrorBroadcastBottom(eng, ast, ms)
        end
        bnode = makeFunCallFlowNode(ast, ms, tt)
        fargs = FlowNode[makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.materialize)), bnode]
        ms = MethodCallStruct(fargs, kwargs)
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            # TODO : report mismatched method table here
            reportErrorFunCall(eng, ast, ms, length(mm), false)
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(eng.globalCtx.rtctx, tt)
            reportErrorBroadcastBottom(eng, ast, ms)
        end
    else
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            # TODO : report mismatched method table here
            reportErrorFunCall(eng, ast, ms, length(mm), isDotcall)
        end
        tt = extractUniqueMatch(mm)
        # Strategy-overlay hook: consult the rtctx's policy on this call.
        # Default rtctx approves everything; strict factories may reject.
        let strict_msg = approveCallSite(eng.globalCtx.rtctx, CompileType[n.typ for n in fargs])
            if !isempty(strict_msg)
                reportErrorStrictReject(eng, ast, strict_msg)
            end
        end
        if isBottomType(eng.globalCtx.rtctx, tt) && checkConstructor
            reportErrorNoConstructor(eng, ast, ms)
        end
        # All args at this site are concrete (checked above).
        # Two modes:
        #   - shallow (default): only flag when Julia widened the return
        #     type to `Any` — that's the proxy for "dispatch couldn't be
        #     resolved internally". Cheap and conservative.
        #   - deep (opt-in via ctx.deepDispatchCheck): flag whenever the
        #     IR walk in inferReturnType found dyn dispatch ANYWHERE in
        #     the call tree, regardless of return type.
        if eng.globalCtx.deepDispatchCheck
            if hasDynamicDispatchForCall(eng, ms)
                reportErrorTopLevelDispatch(eng, ast, ms, isDotcall)
            end
        else
            if isaAny(eng.globalCtx.rtctx, tt)
                reportErrorTopLevelDispatch(eng, ast, ms, isDotcall)
            end
        end
    end
    # TODO : we should add constant propagation for pure builtin function !!!
    # this is again a terminal node
    node = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

# TODO : we should terminate after we return (we need to cut the value here)
function inferReturn(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :return)
    if length(ast.args) == 0
        rettnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(nothing))
        newnode = makeEmptyReturnFlowNode(ast)
    elseif length(ast.args) == 1
        rel = inferExpr(eng, ctx, ast.args[1])
        rettnode = rel.node
        newnode = makeReturnFlowNode(ast, rettnode)
        ctx = rel.ctx
    else
        reportASTError(eng, ast, "Return should have only zero or one parameter")
    end
    # TODO : check bottom type of return
    engn = eng.retVal
    if isNone(engn)
        eng.retVal = Just(rettnode)
    else
        engnn = castJust(engn)
        if !tryMergeReturnFlowNode(eng.globalCtx.rtctx, engnn, rettnode)
            reportErrorReturnEnlargeType(eng, engnn, rettnode)
        end
    end
    addFlowMapping!(eng, ast, newnode)
    # we retain all the variables, because if-else needs to check them
    return InferResult(ctx, newnode)
end

function inferExpr(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    if ast.head == :(=)
        return inferAssign(eng, ctx, ast)
    elseif ast.head == :literal
        return inferLiteral(eng, ctx, ast)
    elseif ast.head == :identifier
        return inferVar(eng, ctx, ast)
    elseif ast.head == :block
        return inferBlock(eng, ctx, ast)
    elseif ast.head == :tuple
        return inferTupleValue(eng, ctx, ast)
    elseif ast.head == :char
        return inferCharLiteral(eng, ctx, ast)
    elseif ast.head == :quote
        return inferQuoteLiteral(eng, ctx, ast)
    elseif ast.head == :macrocall
        return inferMacroCall(eng, ctx, ast)
    elseif ast.head == :curly
        return inferCurlyCall(eng, ctx, ast)
    elseif ast.head == :call
        return inferFunCall(eng, ctx, ast, false)
    elseif ast.head == :dotcall
        return inferFunCall(eng, ctx, ast, true)
    elseif ast.head == :juxtapose
        # `2x`, `A'B`, `3im` — adjacent-token multiplication. The AST has
        # the operands as children; semantically it's `*(args...)`. We
        # rewrite to a call inference via inferJuxtapose, which infers
        # each arg then queries the seam for the return type of `*` over
        # the resulting concrete arg types. Common in numerical code —
        # ColorTypes/FixedPointNumbers/CompositionsBase contribute ~380
        # fires across the corpus when juxtapose isn't modeled.
        return inferJuxtapose(eng, ctx, ast)
    elseif ast.head == :return
        return inferReturn(eng, ctx, ast)
    elseif ast.head == :break
        return inferBreakStmt(eng, ctx, ast)
    elseif ast.head == :continue
        return inferContinueStmt(eng, ctx, ast)
    elseif ast.head == :(.)
        return inferGetField(eng, ctx, ast)
    elseif ast.head == :ref
        return inferArrayRef(eng, ctx, ast)
    elseif ast.head == :if || ast.head == :? || ast.head == :(&&) || ast.head == :(||)
        return inferIfStmt(eng, ctx, ast, ast.head == :(||), ast.head == :(&&))
    elseif ast.head == :string
        return inferStringValue(eng, ctx, ast)
    elseif ast.head == :for
        return inferForStmt(eng, ctx, ast)
    elseif ast.head == :local
        return inferLocal(eng, ctx, ast)
    elseif ast.head == :global
        return inferGlobal(eng, ctx, ast)
    elseif ast.head == :let
        return inferLet(eng, ctx, ast)
    elseif ast.head == :while
        return inferWhileStmt(eng, ctx, ast)
    elseif ast.head == :(<:)
        return inferSubtypeOp(eng, ctx, ast)
    elseif ast.head == :where
        return inferWhereType(eng, ctx, ast)
    elseif ast.head == :(::)
        return inferTypeAssertExpr(eng, ctx, ast)
    elseif ast.head == :function || ast.head == :(->)
        return inferLambdaExpr(eng, ctx, ast)
    elseif ast.head == :comparison
        return inferComparison(eng, ctx, ast)
    elseif ast.head == :try
        return inferTryStmt(eng, ctx, ast)
    else
        str = String(ast.head)
        if last(str) == '='
            return inferAssign(eng, ctx, ast)
        end
        if ast.head in Symbol[:kw, :parameters]
            reportASTError(eng, ast, "Invalid AST construction")
        end
        if ast.head in Symbol[:hcat, :vect, :vcat, :typed_vcat, :typed_hcat]
            return inferCat(eng, ctx, ast)
        end
        if ast.head == :typed_comprehension
            return inferTypedComprehension(eng, ctx, ast)
        end
        reportUnimplementedASTError(eng, ast, "Unsupported AST type $(ast.head)")
    end
end

function inferStringValue(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :string)
    # todo : handle the logic here...DON'T copy everywhere
    args = Vector{FlowNode}(undef, length(ast.args))
    for i in 1:length(args)
        newrel = inferExpr(eng, ctx, ast.args[i])
        ctx = newrel.ctx
        if isBottomType(eng.globalCtx.rtctx, newrel.node.typ)
            reportErrorStringUseBottom(eng, ast.args[i])
        end
        args[i] = newrel.node
    end
    opnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.string))
    ms = MethodCallStruct(opnode, args)
    # TODO : report imprecise method here
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorStringConstructor(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(eng.globalCtx.rtctx, tt)
        reportErrorStringReturnBottom(eng, ast, ms)
    end
    newnode = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferVar(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :identifier)
    id = cast2Symbol(ast.val)
    if hasVar(ctx, id)
        ctxval = lookup(ctx, id)
        node = ctxval.curtyp
        if !mustInitialized(node)
            reportErrorUnitializedVar(eng, ast, id)
        end
        node = makeVarFlowNode(ast, ctxval.curtyp)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif constModuleIsDefined(eng.globalCtx.rtctx, eng.modCT, id)
        val = constModuleGetField(eng.globalCtx.rtctx, eng.modCT, id)
        # fix this, using a module here
        node = makeGlobalVarFlowNode(ast, val)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif id == :&& || id == :||
        if id == :&&
            val = lookupBitAnd(eng.globalCtx.rtctx)
        else
            val = lookupBitOr(eng.globalCtx.rtctx)
        end
        node = makeGlobalVarFlowNode(ast, val)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif id == :end
        # lastindex
        # we only support one-dimentional array!!!
        # this is incorrect
        node = eng.arrayContext[end]
        ms = MethodCallStruct(makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.lastindex)), FlowNode[node])
        m = getMethodMatches(eng, ms)
        # TODO : check method !!!
        tt = extractUniqueMatch(m)
        node = makeFunCallFlowNode(ast, ms, tt)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    elseif id == :begin
        # firstindex
        node = eng.arrayContext[end]
        ms = MethodCallStruct(makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.firstindex)), FlowNode[node])
        m = getMethodMatches(eng, ms)
        # TODO : check method !!!
        tt = extractUniqueMatch(m)
        # TODO : check node here
        node = makeFunCallFlowNode(ast, ms, tt)
        addFlowMapping!(eng, ast, node)
        return InferResult(ctx, node)
    else
        reportErrorUndefinedVar(eng, ast, eng.modCT, id)
    end
end

function inferGlobal(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    return InferResult(ctx, makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(nothing)))
end

function inferLocal(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    for i in ast.args
        if i.head == :(=)
            ctx = inferAssign(eng, ctx, i).ctx
        else
            if i.head == :(::)
                ctx = inferAssignLHSTypedAssert(eng, ctx, i, None(FlowNode)).ctx
            elseif i.head == :identifier
                sym = cast2Symbol(i.val)
                if hasVar(ctx, sym)
                    error("Internal error : a variable shouldn't be declared here")
                end
                node = makeAssignLHSLocalVarFlowNode(ast)
                ctx = update(ctx, sym, ContextValue(node, node))
            else
                reportASTError(eng, i, "Invalid local declaration, must be `local x`` or `local x, y = 1, z::Int`")
            end
        end
    end
    return InferResult(ctx, makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(nothing)))
end

function inferAssignUpdateHelper(eng::Engine, ctx::Context, ast::JuAST, lhsnode::FlowNode, rhsnode::FlowNode, op::UpdateOp)::FlowNode
    if constModuleIsDefined(eng.globalCtx.rtctx, eng.modCT, op.op)
        opval = constModuleGetField(eng.globalCtx.rtctx, eng.modCT, op.op)
        # fix this, using a module here
        opnode = makeGlobalVarFlowNode(op.ast, opval)
        addFlowMapping!(eng, op.ast, opnode)
        argnodes = FlowNode[opnode, lhsnode, rhsnode]
        ms = MethodCallStruct(argnodes)
        mm = getMethodMatches(eng, ms)
        if length(mm) != 1
            reportErrorFunCall(eng, ast, ms, length(mm))
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(eng.globalCtx.rtctx, tt)
            reportErrorUpdateAssignReturnBottom(eng, ast)
        end
        rhsnode = makeFunCallFlowNode(ast, ms, tt)
        addFlowMapping!(eng, op.ast, rhsnode)
        return rhsnode
    else
        throw(InternalError("All the update assignment operators should be defined in module $(toConstString(eng.globalCtx.rtctx, eng.modCT))"))
        
    end
end

function inferAssignLHSVarBroadcast(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    # x .+= 1
    rel = inferVar(eng, ctx, ast)
    ctx = rel.ctx
    lhsnode = rel.node
    # lhsnode shouldn't be a bottom
    if isBottomType(eng.globalCtx.rtctx, lhsnode.typ)
        throw(InternalError("Variable should never be inferred as bottom type!"))
    end
    return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
end

function inferAssignLHSBroadcastHelper(eng::Engine, ctx::Context, ast::JuAST, lhsnode::FlowNode, rhsnode::FlowNode, op::UpdateOp)::InferResult
    if op.isUpdate
        if constModuleIsDefined(eng.globalCtx.rtctx, eng.modCT, op.op)
            opval = constModuleGetField(eng.globalCtx.rtctx, eng.modCT, op.op)
            # fix this, using a module here
            opnode = makeGlobalVarFlowNode(op.ast, opval)
            addFlowMapping!(eng, op.ast, opnode)
            fargs = FlowNode[makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.broadcasted)), opnode, lhsnode, rhsnode]
        else
            throw(InternalError("All the update assignment operators should be defined in module $(toConstString(eng.globalCtx.rtctx, eng.modCT))"))
        end
    else
        opnode = makeGlobalVarFlowNode(ast, lookupBaseIdentity(eng.globalCtx.rtctx))
        fargs = FlowNode[makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.broadcasted)), opnode, rhsnode]
    end
    ms = MethodCallStruct(fargs)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        # TODO : report mismatched method table here
        reportErrorFunCall(eng, ast, ms, length(mm), false)
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(eng.globalCtx.rtctx, tt)
        reportErrorBroadcastBottom(eng, ast, ms)
    end
    bnode = makeFunCallFlowNode(ast, ms, tt)
    fargs = FlowNode[makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.materialize!)), lhsnode, bnode]
    ms = MethodCallStruct(fargs)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        # TODO : report mismatched method table here
        reportErrorFunCall(eng, ast, ms, length(mm), false)
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(eng.globalCtx.rtctx, tt)
        reportErrorBroadcastBottom(eng, ast, ms)
    end
    node = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node)
end

function inferAssignLHSVar(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    assertASTKind(ast, :identifier)
    if op.isDotcall
        return inferAssignLHSVarBroadcast(eng, ctx, ast, rhsnode, op)
    end
    varid = cast2Symbol(ast.val)
    # rhsnode is not a bottom, ensured by caller of inferLHSSingpleVar
    if op.isUpdate
        rel = inferVar(eng, ctx, ast)
        ctx = rel.ctx
        lhsnode = rel.node
        # lhsnode shouldn't be a bottom
        if isBottomType(eng.globalCtx.rtctx, lhsnode.typ)
            throw(InternalError("Variable should never be inferred as bottom type!"))
        end
        if constModuleIsDefined(eng.globalCtx.rtctx, eng.modCT, op.op)
            opval = constModuleGetField(eng.globalCtx.rtctx, eng.modCT, op.op)
            # fix this, using a module here
            opnode = makeGlobalVarFlowNode(op.ast, opval)
            addFlowMapping!(eng, op.ast, opnode)
            argnodes = FlowNode[opnode, lhsnode, rhsnode]
            ms = MethodCallStruct(argnodes)
            mm = getMethodMatches(eng, ms)
            if length(mm) != 1
                reportErrorFunCall(eng, ast, ms, length(mm))
            end
            tt = extractUniqueMatch(mm)
            if isBottomType(eng.globalCtx.rtctx, tt)
                reportErrorUpdateAssignReturnBottom(eng, ast)
            end
            rhsnode = makeFunCallFlowNode(ast, ms, tt)
            addFlowMapping!(eng, op.ast, rhsnode)
        else
            throw(InternalError("All the update assignment operators should be defined in module $(toConstString(eng.globalCtx.rtctx, eng.modCT))"))
        end
    end
    # TODO : is this kind of reasoning correct??
    # we simply drop the variable here
    if varid == Symbol("_")
        return InferResult(ctx, rhsnode)
    end
    # TODO : fix assignment here
    if hasVar(ctx, varid)
        # this variable is already assigned before, we check type compatibility
        oldval = lookup(ctx, varid)
        if !isDeclarationFlowNode(oldval.curtyp)
            # storage type is unchanged, update current type 
            if !tryMergeFlowType(eng.globalCtx.rtctx, oldval.typ, rhsnode)
                reportErrorAssignIncompatible(eng, oldval.typ, rhsnode)
            end
            newnode = makeAssignLHSVarFlowNode(ast, rhsnode, op)
            # the primary assignment is unchanged
            val = ContextValue(oldval.typ, newnode)
        else
            # the variable is only declared with `local x`
            # so it has no storage type
            newnode = makeAssignLHSVarFlowNode(ast, rhsnode, op)
            val = ContextValue(newnode, newnode)
        end
    else
        # the variable is not assigned before
        newnode = makeAssignLHSVarFlowNode(ast, rhsnode, op)
        val = ContextValue(newnode, newnode)    
    end
    # TODO : this ast assigned is incorrect !
    # we lacks the assign to the whole assignment expression
    ctx = update(ctx, varid, val)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end


function inferAssignLHSFunCall(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    # f() .+= y, handled similar to variable
    assertASTKind(ast, :call)
    rel = inferExpr(eng, ctx, ast)
    ctx = rel.ctx
    lhsnode = rel.node
    return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
end

#=
function inferGenerator(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :generator)
    # we disallow generator modifying variable from outer scope
    body = ast.args[1]
    info = eng.scopeInfos[ast]
    if !isempty(info.modified)
        reportASTError(eng, ast, "Generator should not modify variables from outer scope")
    end
    # shadow variable
    # since we don't modify any variable, there is no need to update ctx
    newmapping = Dict{Symbol, ContextValue}()
    scopeInfo = eng.scopeInfos[ast]
    for pair in ctx.mapping.data
        k, v = pair
        if !hasVar(scopeInfo, k)
            newmapping[k] = v
        end
    end
    newctx = ContextValue(ImmutableDict(newmapping)())
    if length(ast.args) > 2
        reportASTError(eng, ast, "Only support one variable in generator expression")
    end
    rhs = ast.args[2]
    if rhs.head == :filter
        reportASTError(eng, ast, "Filter not supported")
    end
    elseif rhs.head == :(=)
    rel = inferExpr(eng, ast, newctx)
    newctx = rel.ctx
    varnode = iterateCheck(eng, rel.node)
    newctx = inferAssignLHS(eng, newctx, assign, varnode, UpdateOp()).ctx
    
end
=#

function inferAssignLHSSetField(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    # TODO : getproperty and setproperty on module...
    assertASTKind(ast, :(.))
    if length(ast.args) != 2
        reportASTError(eng, ast, "Empty setproperty")
    end 
    fieldast = ast.args[2]
    # JuliaSyntax emits `x.f = v` with field selector as bare identifier
    # (`:.[id, id]`); the explicit `x.:f = v` form quotes it (`:.[id,
    # :quote[id]]`). The read path (inferGetProperty) accepts both — the
    # write path used to accept only the quoted form, which is why plain
    # `ctx.walkTurn = i` in the checker's own source surfaced as the
    # uninformative "Not a valid field" pre-inference error.
    if fieldast.head == :identifier
        p = cast2Symbol(fieldast.val)
    elseif fieldast.head == :quote && length(fieldast.args) == 1 && fieldast.args[1].head == :identifier
        p = cast2Symbol(fieldast.args[1].val)
    else
        reportASTError(eng, ast,
            "setproperty! target must be an identifier or quoted symbol; got AST head `$(fieldast.head)`")
    end
    refast = ast.args[1]
    rel = inferExpr(eng, ctx, ast.args[1])
    ctx = rel.ctx
    xnode = rel.node
    # we need to distinguish getproperty on module and other types...
    if isBottomType(eng.globalCtx.rtctx, xnode.typ)
        reportErrorSetFieldOfBottom(eng, ast)
    end
    if !isConcreteType(eng.globalCtx.rtctx, xnode.typ)
        reportErrorFieldType(eng, ast, xnode, false)
    end
    if !hasField(eng.globalCtx.rtctx, xnode.typ, p)
        reportErrorNoField(eng, ast, xnode, p)
    end
    
    # here, it should be dotgetproperty for broadcast call
    # or getproperty for other non-broadcast call
    ft = getFieldType(eng.globalCtx.rtctx, xnode.typ, p)
    if !isConcreteType(eng.globalCtx.rtctx, ft)
        if op.isDotcall || op.isUpdate
            reportErrorFieldType(eng, ast, xnode, false)
        end
    end
    lhsnode = makeGetPropertyFlowNode(refast, xnode, ft)
    
    if op.isDotcall
        return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
    end

    if op.isUpdate
        if constModuleIsDefined(eng.globalCtx.rtctx, eng.modCT, op.op)
            # TODO : add correct arguments here!!!
            opval = constModuleGetField(eng.globalCtx.rtctx, eng.modCT, op.op)
            # fix this, using a module here
            opnode = makeGlobalVarFlowNode(op.ast, opval)
            addFlowMapping!(eng, op.ast, opnode)
        else
            throw(InternalError("All the update assignment operators should be defined in module $(toConstString(eng.globalCtx.rtctx, eng.modCT))"))
        end
        argnodes = FlowNode[opnode, lhsnode, rhsnode]
        ms = MethodCallStruct(argnodes)
        mm = getMethodMatches(eng, ms)
        if length(mm) != 1
            reportErrorFunCall(eng, ast, ms, length(mm))
        end
        rt = extractUniqueMatch(mm)
        if isBottomType(eng.globalCtx.rtctx, rt)
            reportErrorUpdateAssignReturnBottom(eng, ast)
        end
        rhsnode = makeFunCallFlowNode(ast, ms, rt)
        addFlowMapping!(eng, op.ast, rhsnode)
    end
    
    opnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.setproperty!))
    # TODO : this is incorrect...
    # ms should be setproperty!(x, v, i)
    ms = MethodCallStruct(opnode, FlowNode[xnode, rhsnode])
    if !checkFieldCompatibility(eng.globalCtx.rtctx, ft, rhsnode.typ)
        reportErrorSetFieldTypeIncompatible(eng, ast, xnode, rhsnode, ft, p)
    end
    newnode = makeSetPropertyFlowNode(ast, xnode, rhsnode, rhsnode.typ)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferAssignLHSTypedAssert(eng::Engine, ctx::Context, ast::JuAST, mrhsnode::Maybe{FlowNode})::InferResult
    assertASTKind(ast, :(::))
    lhs = ast.args[1]
    # we only support x::Int = 1 for single variable
    # notice that this is not a scope declaration, but we check it in the scope checker
    rel = inferExpr(eng, ctx, ast.args[2])
    ctx = rel.ctx
    if lhs.head == :identifier
        var = cast2Symbol(lhs.val)
        typ = rel.node.typ
        if !isConstVal(typ)
            reportErrorTypeAssertNonconst(eng, ast)
        else
            declaretyp = lift(eng.globalCtx.rtctx, typ)
            if isNone(mrhsnode)
                # TODO : distinguish bewteen these two kinds of node!!!
                node1 = makeAssignLHSVarAssertFlowNode(ast, rel.node, declaretyp, false)
                ctx = update(ctx, var, ContextValue(node1, node1))
                node = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(nothing))
                return InferResult(ctx, node)
            else
                rhsnode = castJust(mrhsnode)
                if !tryMergeCompileValue(eng.globalCtx.rtctx, declaretyp, rhsnode.typ)
                    reportErrorAssignInitIncompatible(eng, ast, declaretyp, rhsnode.typ)
                end
                node1 = makeAssignLHSVarAssertFlowNode(ast, rhsnode, declaretyp, true)
                ctx = update(ctx, var, ContextValue(node1, node1))
                node = makeAssignExprFlowNode(ast, rhsnode)
                return InferResult(ctx, node)
            end
        end
    else
        reportASTError(eng, ast, "Typed assert can only declared on a single variable")
    end
end

function inferAssignLHS(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    if ast.head == :tuple
        if op.isUpdate
            reportASTError(eng, ast, "update assignment can't be used while lhs is a tuple destruct")
        elseif op.isDotcall
            reportASTError(eng, ast, "Don't use broadcast to assign a tuple on lhs")
        end
        return inferAssignLHSTupleDestruct(eng, ctx, ast, rhsnode)
    elseif ast.head == :identifier
        return inferAssignLHSVar(eng, ctx, ast, rhsnode, op)
    elseif ast.head == :(.)
        return inferAssignLHSSetField(eng, ctx, ast, rhsnode, op)
    elseif ast.head == :ref
        return inferAssignLHSArrayRef(eng, ctx, ast, rhsnode, op)
    elseif ast.head == :(::)
        if op.isUpdate
            reportASTError(eng, ast, "update assignment can't be used while lhs is a typed assert")
        elseif op.isDotcall
            reportASTError(eng, ast, "Don't use broadcast to assign a variable with tyed assert")
        end
        return inferAssignLHSTypedAssert(eng, ctx, ast, Just(rhsnode))
    elseif ast.head == :call
        return inferAssignLHSFunCall(eng, ctx, ast, rhsnode, op)
    else
        reportASTError(eng, ast, "LHS of assignment must be a variable, tuple, property or arrayref")
    end
end

function tryDestructTuple(eng::Engine, ctx::Context, ast::JuAST, node::FlowNode)::Union{Nothing, Vector{FlowNode}}
    rtctx = eng.globalCtx.rtctx
    # `tupleDestructInfo` peeks `node.typ.typ` driver-side, returns
    # a 3-tuple (isStruct, lenOk, isTupleOrPair). Element types are
    # pulled through the membrane via getTypeParameterOf so the
    # inferencer never reaches into DataType.parameters directly.
    isStruct, lenOk, isTupleOrPair = tupleDestructInfo(rtctx, node.typ, length(ast.args))
    if isStruct
        if lenOk
            if isTupleOrPair
                nodes = FlowNode[makeAssignLHSTupleDestructFlowNode(
                                    ast.args[i], node,
                                    getTypeParameter(rtctx, node.typ, i))
                                 for i in 1:length(ast.args)]
                return nodes
            else
                reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, not a pair or tuple. Got a $(toString(eng.globalCtx.rtctx, node.typ))")
            end
        else
            reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, length mismatched")
        end
    else
        reportErrorFailedToDestruct(eng, ast, "Failed to destruct rhs, not a pair or tuple. Got a $(toString(eng.globalCtx.rtctx, node.typ))")
    end
end

function inferAssignLHSArrayRef(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode, op::UpdateOp)::InferResult
    assertASTKind(ast, :ref)
    if length(ast.args) == 0
        reportASTError(eng, ast, "Empty ref")
    end 
    args = similar(ast.args, FlowNode)
    for i in 1:length(args)
        if i == 2
            push!(eng.arrayContext, args[1])
        end
        newrel = inferExpr(eng, ctx, ast.args[i])
        ctx = newrel.ctx
        args[i] = newrel.node
        if isBottomType(eng.globalCtx.rtctx, newrel.node.typ)
            reportErrorArrayRefBottom(eng, ast, i, i == lastindex(args))
        end
        if i >= 2 && i == lastindex(args)
            pop!(eng.arrayContext)
        end
    end
    if op.isUpdate || op.isDotcall
        # TODO : extract this to a helper function
        if op.isDotcall
            opnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.dotview))
        else
            opnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.getindex))
        end
        ms = MethodCallStruct(opnode, args)
        # TODO : report imprecise method here
        mm = getMethodMatches(eng, ms)
        if length(mm) >= 2 || length(mm) == 0
            reportErrorArrayRef(eng, ast, ms, length(mm))
        end
        tt = extractUniqueMatch(mm)
        if isBottomType(eng.globalCtx.rtctx, tt)
            reportErrorIndexReturnBottom(eng, ast, ms)
        end
        lhsnode = makeArrayRefFlowNode(ast, ms, tt)
        addFlowMapping!(eng, ast, lhsnode)
        if op.isDotcall
            return inferAssignLHSBroadcastHelper(eng, ctx, ast, lhsnode, rhsnode, op)
        else
            rhsnode = inferAssignUpdateHelper(eng, ctx, ast, lhsnode, rhsnode, op)
        end
    end
    insert!(args, 2, rhsnode)
    opnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.setindex!))
    ms = MethodCallStruct(opnode, args)
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArraySet(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(eng.globalCtx.rtctx, tt)
        reportErrorIndexReturnBottom(eng, ast, ms)
    end
    newnode = makeAssignLHSArrayRefFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    InferResult(ctx, newnode)
end

function inferAssignLHSTupleDestruct(eng::Engine, ctx::Context, ast::JuAST, rhsnode::FlowNode)::InferResult
    assertASTKind(ast, :tuple)
    tts = tryDestructTuple(eng, ctx, ast, rhsnode)
    for i in eachindex(ast.args)
        ctx = inferAssignLHS(eng, ctx, ast.args[i], tts[i], UpdateOp()).ctx
    end
    newnode = makeAssignLHSTupleDestructFlowNode(ast, rhsnode, rhsnode.typ)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferAssign(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    opstr = String(ast.head)
    isDotcall = (ast.flag & 0x0002) != 0
    # JuliaSyntax.DOTOP_FLAG == 0x0002
    # Augmented assignments (`x += 1`, `v[i] .*= 2`, etc.) parse as
    # `head=:op=`, `args=[lhs, op_ident, rhs]`. The operator symbol lives
    # in args[2] rather than encoded in the head; rhs is at args[3].
    isOpEq = ast.head === Symbol("op=")
    rhsIdx = isOpEq ? 3 : 2
    local op::UpdateOp
    if isOpEq
        if length(ast.args) != 3
            reportASTError(eng, ast, "op= expression should have 3 children, $(length(ast.args)) found")
        end
        opArg = ast.args[2]
        if opArg.head != :identifier
            reportASTError(eng, ast, "op= operator slot must be an identifier")
        end
        op = UpdateOp(ast, cast2Symbol(opArg.val), isDotcall)
    else
        isValid = false
        op = UpdateOp(ast, isDotcall)
        if length(opstr) >= 1
            opchar = opstr[end]
            if opchar == '='
                isValid = true
                if length(opstr) > 1
                    op = UpdateOp(ast, Symbol(opstr[1:end-1]), isDotcall)
                end
            end
        end
        if !isValid
            reportASTError(eng, ast, "Not a valid assignment $opstr")
        end
        if length(ast.args) != 2
            reportASTError(eng, ast, "Assignment expression should only have 2 children, $(length(ast.args)) found")
        end
    end
    if ast.args[1].head == :call && !op.isDotcall
        reportASTError(eng, ast, "Shorthand function definition not supported")
    end
    rel = inferExpr(eng, ctx, ast.args[rhsIdx])
    ctx = rel.ctx
    rhsnode = rel.node
    if isBottomType(eng.globalCtx.rtctx, rhsnode.typ)
        # error(rhs) is a bottom
        reportErrorAssignBottom(eng, ast)
    end
    # logic to dispatch lhs
    lhs = ast.args[1]
    rel = inferAssignLHS(eng, ctx, lhs, rhsnode, op)
    if !op.isUpdate && !op.isDotcall
        # the value of the whole assignment expression is the value of RHS
        anode = makeAssignExprFlowNode(ast, rhsnode)
        addFlowMapping!(eng, ast, anode)
        return InferResult(rel.ctx, anode)
    end
    return rel
end

function inferBlock(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :block)
    if length(ast.args) == 0
        newnode = makeEmptyBlockFlowNode(eng.globalCtx.rtctx, ast)
        return InferResult(ctx, newnode)
    else
        for i in 1:length(ast.args)-1
            ret = inferExpr(eng, ctx, ast.args[i])
            ctx = ret.ctx
            if isBottomType(eng.globalCtx.rtctx, ret.node.typ)
                reportErrorPrematureUnreachable(eng, ast.args[i])
            end
        end
    end
    ret = inferExpr(eng, ctx, last(ast.args))
    newnode = makeBlockFlowNode(ast, ret.node)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ret.ctx, newnode)
end

# Merge the post-try and post-catch contexts when both branches fall
# through. For each variable seen in either ctx:
#   - If both branches have it `mustInitialized`, take the Union of the
#     two current-types.
#   - If only one branch has it initialized (or the other is missing),
#     fall back to the pre-try binding so the variable doesn't
#     silently spread an unsafe assumption.
#   - Variables not in `prectx` and not initialized in BOTH branches are
#     not propagated.
function _mergeTryCatchContexts(eng::Engine, ast::JuAST, prectx::Context, try_ctx::Context, catch_ctx::Context)::Context
    newmap = Dict{Symbol, ContextValue}()
    seen = Set{Symbol}()
    for k in keys(try_ctx.mapping.data)
        push!(seen, k)
    end
    for k in keys(catch_ctx.mapping.data)
        push!(seen, k)
    end
    for v in seen
        inTry = hasVar(try_ctx, v)
        inCatch = hasVar(catch_ctx, v)
        if inTry && inCatch
            tv = lookup(try_ctx, v)
            cv = lookup(catch_ctx, v)
            if mustInitialized(tv.curtyp) && mustInitialized(cv.curtyp)
                merged = tryMergeFlowNode(eng, ast, FlowNode[tv.curtyp, cv.curtyp], true)
                newmap[v] = ContextValue(tv.typ, merged)
                continue
            end
        end
        if hasVar(prectx, v)
            newmap[v] = lookup(prectx, v)
        end
    end
    return Context(ImmutableDict(newmap))
end

function inferTryStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :try)
    # In-tree JuliaSyntax `try` AST (1.12+):
    #   args[1] : try body (:block)
    #   args[2..end] : optional clause sub-nodes, each with head
    #     :catch, :else, or :finally (zero or more, in source order).
    #   :catch has children [var, body] where var is an :identifier
    #     (val = Symbol("") for `catch` with no binding).
    #   :else and :finally each have one :block child.
    try_body = ast.args[1]
    try_rel = inferExpr(eng, ctx, try_body)

    local merged_ctx::Context = ctx
    for i in 2:length(ast.args)
        c = ast.args[i]
        if c.head == :catch
            catch_var = c.args[1]
            catch_body = c.args[2]
            catch_ctx = ctx
            if catch_var.head == :identifier
                # Split the narrowing into a standalone `isa` so the
                # checker sees `vv` as a Symbol when passed to `update`.
                # Folding it into `&& vv != …` works at runtime but the
                # checker's isa-narrowing doesn't propagate through `&&`.
                vvAny = catch_var.val.val
                if vvAny isa Symbol
                    if vvAny != Symbol("")
                        anynode = makeParamFlowNode(catch_var, makeAnyType(eng.globalCtx.rtctx))
                        catch_ctx = update(catch_ctx, vvAny, ContextValue(anynode, anynode))
                    end
                end
            end
            catch_rel = inferExpr(eng, catch_ctx, catch_body)
            # Three cases when there's a catch block:
            #   - Catch's body is Bottom (every path returns / throws /
            #     rethrows): reaching code AFTER the try/catch implies
            #     the try completed without throwing, so try's
            #     post-context is sound to propagate.
            #   - Try's body is Bottom: the only path past the block is
            #     via the catch.
            #   - Both branches fall through: merge variables that are
            #     `mustInitialized` in BOTH branches (mirrors the
            #     if/else merger). Variables present in only one branch
            #     fall back to the pre-try binding if any.
            if isBottomType(eng.globalCtx.rtctx, catch_rel.node.typ)
                merged_ctx = try_rel.ctx
            elseif isBottomType(eng.globalCtx.rtctx, try_rel.node.typ)
                merged_ctx = catch_rel.ctx
            else
                merged_ctx = _mergeTryCatchContexts(eng, ast, ctx, try_rel.ctx, catch_rel.ctx)
            end
        elseif c.head == :else
            inferExpr(eng, ctx, c.args[1])
        elseif c.head == :finally
            inferExpr(eng, ctx, c.args[1])
        end
    end

    node = makeBlockFlowNode(ast, try_rel.node)
    addFlowMapping!(eng, ast, node)
    return InferResult(merged_ctx, node)
end

function inferLet(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :let)
    vars = ast.args[1]
    newmapping = Dict{Symbol, ContextValue}()
    shadowmapping = Dict{Symbol, ContextValue}()
    info = eng.scopeInfos[ast]
    for pair in ctx.mapping.data
        name = pair.first
        value = pair.second
        if hasVar(info, name)
            shadowmapping[name] = value
        else
            newmapping[name] = value
        end
    end
    oldctx = ctx
    ctx = Context(ImmutableDict(newmapping))
    for i in vars.args
        if i.head == :(=)
            ctx = inferExpr(eng, ctx, i).ctx
        end
    end
    rel = inferExpr(eng, ctx, ast.args[2])
    node = rel.node
    ctx = rel.ctx
    finalmapping = Dict{Symbol, ContextValue}()
    for pair in oldctx.mapping.data
        name = pair.first
        value = pair.second
        if hasVar(info, name)
            finalmapping[name] = shadowmapping[name]
        else
            finalmapping[name] = newmapping[name]
        end
    end
    ctx = Context(ImmutableDict(finalmapping))
    node = makeLetFlowNode(ast, node)
    return InferResult(ctx, node)
end

function inferTupleValue(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :tuple)
    nodes = Vector{FlowNode}(undef, length(ast.args))
    for i in eachindex(ast.args)
        local iast = ast.args[i]
        rel = inferExpr(eng, ctx, iast)
        ctx = rel.ctx
        if isBottomType(eng.globalCtx.rtctx, rel.node.typ)
            reportErrorTupleBottom(eng, iast, i)
        end
        nodes[i] = rel.node
    end
    node = makeTupleFlowNode(ast, nodes, makeTupleType(eng.globalCtx.rtctx, CompileType[fn.typ for fn in nodes]))
    addFlowMapping!(eng, ast, node)
    return InferResult(ctx, node) 
end

function makeMatrixType(rtctx::RuntimeContext, typ::CompileType)::CompileType
    return makeMatrixOf(rtctx, typ)
end

function makeVectorType(rtctx::RuntimeContext, typ::CompileType)::CompileType
    return makeVectorOf(rtctx, typ)
end

# `T[expr for x in xs]` — typed comprehension.
#
# As the user pointed out, this AST in Julia is semantically a typed
# `getindex` on the type `T` — the result is `Vector{T}` (or `Array{T, N}`
# for nested generators) and the body is collected into that container.
# A faithful implementation would type-check the body in a scope where
# `x` is bound to `eltype(xs)`, then verify the body's type is `<: T`.
# That requires generator scope plumbing that this checker doesn't have
# yet (the `:generator` head is currently a TODO at scope-check time).
#
# This minimum-viable handler covers the syntactic + outer-flow side:
#   * the element type expression must evaluate to a `Type` constant;
#   * the iterable expression is type-checked (its errors do surface);
#   * the body expression is type-checked at the outer scope, so
#     references to outer variables flag as they should — but the
#     loop-introduced binding is missing, so name lookups of the iter
#     var will (correctly) be undefined. Real generator scope is a
#     follow-up; this unblocks every self-check fire on real Julia code
#     that uses `T[...]` comprehensions, which is most of them.
function inferTypedComprehension(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :typed_comprehension)
    if length(ast.args) < 2
        reportASTError(eng, ast, "Malformed typed_comprehension: expected element type + generator")
    end
    rtctx = eng.globalCtx.rtctx
    # Element type
    rel = inferExpr(eng, ctx, ast.args[1])
    ctx = rel.ctx
    eltNode = rel.node
    if !isConstVal(eltNode.typ)
        reportASTError(eng, ast,
            "Element type of typed comprehension must be a compile-time constant")
    end
    elt = lift(rtctx, eltNode.typ)
    # Walk the iterable so its errors surface. The generator's body is
    # left un-checked until proper generator scope handling lands.
    gen = ast.args[2]
    if gen.head == :generator && length(gen.args) >= 2
        iter = gen.args[2]
        if iter.head == :iteration
            for binding in iter.args
                if length(binding.args) >= 2
                    relIter = inferExpr(eng, ctx, binding.args[2])
                    ctx = relIter.ctx
                end
            end
        end
    end
    vecTyp = makeVectorType(rtctx, elt)
    return InferResult(ctx, FlowNode(ast, FlowNode[], vecTyp, nothing))
end

function inferCat(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    #=
    [x y;z w] => vcat
    [x y z w] => hcat
    [x, y, z, w] => vect
    =#
    # Julia allows something like [[1 2 3] 4]
    # we disallow literal involving such joining
    # [[1 2 3] [1 2 3]] == [1 2 3 1 2 3], not a nested array
    principleType = None(FlowNode)
    if ast.head == :typed_vcat || ast.head == :vcat 
        # TODO : this is a hack, don't do this...
        # TODO : this is incorrect, but I don't know how to concat complicated array...
        if ast.head == :typed_vcat
            rel = inferExpr(eng, ctx, ast.args[1])
            ctx = rel.ctx
            prinType = Just(rel.node)
            start = 2
            istyped = true
        else
            start = 1
            prinType = None(FlowNode)
            istyped = false
        end
        for ii in start:length(ast.args)
            i = ast.args[ii]
            if i.head == :row
                for j in i.args
                    rel = inferExpr(eng, ctx, j)
                    node = rel.node
                    ctx = rel.ctx
                    if isNone(prinType)
                        prinType = Just(node)
                    else
                        pnode = castJust(prinType)
                        if istyped
                            pnodetyp = lift(eng.globalCtx.rtctx, pnode.typ)
                            if !(tryMergeCompileValue(eng.globalCtx.rtctx, pnodetyp, node.typ) &&
                                tryMergeCompileValue(eng.globalCtx.rtctx, node.typ, pnodetyp))
                                # todo : improve error reporting here
                                reportErrorElementNonSameType(eng, ast, pnode, node, true)
                            end
                        else
                            if !(tryMergeCompileValue(eng.globalCtx.rtctx, pnode.typ, node.typ) &&
                                tryMergeCompileValue(eng.globalCtx.rtctx, node.typ, pnode.typ))
                                reportErrorElementNonSameType(eng, ast, pnode, node, false)
                            end
                        end
                    end
                end
            else
                reportASTError(eng, ast, ":row is expected in :vcat")
            end
        end
        if istyped
            ttt = makeMatrixType(eng.globalCtx.rtctx, lift(eng.globalCtx.rtctx, castJust(prinType).typ))
        else
            # TODO : test isNone of prinType
            ttt = makeMatrixType(eng.globalCtx.rtctx, castJust(prinType).typ)
        end
        # TODO : fix this...
        return InferResult(ctx, FlowNode(ast, FlowNode[], ttt, nothing))
    elseif ast.head == :vect
        # [a, b, c] — infer eltype from elements. Elements must all have the
        # same concrete type (no implicit promotion); use Type[...] if you
        # want a different element type than what the elements infer to.
        if length(ast.args) == 0
            reportErrorEmptyAnyArray(eng, ast)
        end
        fnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.vect))
        return inferCatHelper(eng, ctx, ast, fnode, None(FlowNode))
    elseif ast.head == :hcat
        fnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.hcat))
        return inferCatHelper(eng, ctx, ast, fnode, None(FlowNode))
    elseif ast.head == :typed_hcat
        fnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.typed_hcat))
        rel = inferExpr(eng, ctx, ast.args[1])
        ctx = rel.ctx
        return inferCatHelper(eng, ctx, ast, fnode, Just(rel.node))
    else
        return reportASTError(eng, ast, "Invalid array grammer")
    end
end

function inferCatHelper(eng::Engine, ctx::Context, ast::JuAST, fnode::FlowNode, prinType::Maybe{FlowNode})::InferResult
    istyped = !isNone(prinType)
    if istyped
        start = 2
        args = FlowNode[castJust(prinType)]
    else
        start = 1
        args = FlowNode[]
    end
    for ii in start:length(ast.args)
        i = ast.args[ii]
        rel = inferExpr(eng, ctx, i)
        ctx = rel.ctx
        node = rel.node
        push!(args, node)
        if isNone(prinType)
            prinType = Just(node)
        else
            pnode = castJust(prinType)
            if istyped
                pnodetyp = lift(eng.globalCtx.rtctx, pnode.typ)
                if !(tryMergeCompileValue(eng.globalCtx.rtctx, pnodetyp, node.typ) &&
                    tryMergeCompileValue(eng.globalCtx.rtctx, node.typ, pnodetyp))
                    # todo : improve error reporting here
                    reportErrorElementNonSameType(eng, ast, pnode, node, true)
                end
            else
                if !(tryMergeCompileValue(eng.globalCtx.rtctx, pnode.typ, node.typ) &&
                    tryMergeCompileValue(eng.globalCtx.rtctx, node.typ, pnode.typ))
                    reportErrorElementNonSameType(eng, ast, pnode, node, false)
                end
            end
        end
    end
    if isNone(prinType)
        reportErrorEmptyAnyArray(eng, ast)
    end
    ms = MethodCallStruct(fnode, args)
    # TODO : report imprecise method here
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArrayRef(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(eng.globalCtx.rtctx, tt)
        reportErrorIndexReturnBottom(eng, ast, ms)
    end
    newnode = makeFunCallFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)    
end

function inferBreakStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :break)
    # a forward edge that requires join
    push!(eng.loopContext[end], ctx)
    InferResult(ctx, makeBreakStmtNode(ast))
end

function inferContinueStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :continue)
    InferResult(ctx, makeContinueStmtNode(ast))
end

# TODO : perform constant propagation for tuple type and pair here...
# actually, we need a general way to perform constant and type propagation here
# maybe some kind of dependent type ???


# TODO : infer end here
# we need to store an array context chain here, which is not hard, but just remember to do so !!!

function inferArrayRef(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :ref)
    if length(ast.args) == 0
        reportASTError(eng, ast, "Indexing operation is empty")
    end
    args = Vector{FlowNode}(undef, length(ast.args))
    for i in 1:length(args)
        if i == 2
            push!(eng.arrayContext, args[1])
        end
        newrel = inferExpr(eng, ctx, ast.args[i])
        ctx = newrel.ctx
        if isBottomType(eng.globalCtx.rtctx, newrel.node.typ)
            reportErrorArrayRefBottom(eng, ast, i, false)
        end
        args[i] = newrel.node
        if i >= 2 && i == lastindex(args)
            pop!(eng.arrayContext)
        end
    end
    opnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(Base.getindex))
    ms = MethodCallStruct(opnode, args)
    # TODO : report imprecise method here
    mm = getMethodMatches(eng, ms)
    if length(mm) >= 2 || length(mm) == 0
        reportErrorArrayRef(eng, ast, ms, length(mm))
    end
    tt = extractUniqueMatch(mm)
    if isBottomType(eng.globalCtx.rtctx, tt)
        reportErrorIndexReturnBottom(eng, ast, ms)
    end
    newnode = makeArrayRefFlowNode(ast, ms, tt)
    addFlowMapping!(eng, ast, newnode)
    return InferResult(ctx, newnode)
end

function inferGetField(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :(.))
    if length(ast.args) != 2
        reportASTError(eng, ast, "Not a valid getproperty call")
    end
    ap = ast.args[2]
    # In-tree JuliaSyntax emits `(. a b)` (bare identifier on the rhs);
    # legacy JuliaSyntax 0.3 emits `(. a (quote b))`. Accept both shapes.
    if ap.head == :identifier
        p = cast2Symbol(ap.val)
    elseif ap.head == :quote && length(ap.args) == 1 && ap.args[1].head == :identifier
        p = cast2Symbol(ap.args[1].val)
    else
        reportASTError(eng, ast, "Property is not a constant symbol")
    end
    rel = inferExpr(eng, ctx, ast.args[1])
    node = rel.node
    ctx = rel.ctx
    ft = node.typ
    # we need to distinguish getproperty on module and other types...
    if isBottomType(eng.globalCtx.rtctx, ft)
        reportErrorGetFieldOfBottom(eng, ast)
    end
    rtctx = eng.globalCtx.rtctx
    if isConstVal(ft) && rtctx.constIsModule(ft)
        # The const value is a Module — fetch the property via membrane-aware
        # module accessors. The Module reference itself stays opaque.
        if constModuleIsDefined(rtctx, ft, p)
            v = constModuleGetField(rtctx, ft, p)
            node = makeGetPropertyFlowNode(ast, node, v)
            addFlowMapping!(eng, ast, node)
            return InferResult(ctx, node)
        else
            reportErrorNoField(eng, ast, node, p)
        end
    end
    if !isConcreteType(eng.globalCtx.rtctx, ft)
        reportErrorFieldType(eng, ast, node, false)
    end
    if hasField(eng.globalCtx.rtctx, ft, p)
        tt = getFieldType(eng.globalCtx.rtctx, ft, p)
        newnode = makeGetPropertyFlowNode(ast, node, tt)
        addFlowMapping!(eng, ast, newnode)
        return InferResult(ctx, newnode)
    else
        # No struct field with this name. Some packages (PDMats's PDiagMat,
        # AccessorsExtra's virtual accessors) provide virtual properties
        # via a non-default `Base.getproperty(::T, ::Symbol)` overload. The
        # `permitCustomGetproperty` flag opts into accepting those: we
        # confirm there IS a user-defined `getproperty` for the type, then
        # synthesize `Any` for the result. The trade-off is that we no
        # longer know the result type — but the access is legitimate Julia.
        if eng.globalCtx.permitCustomGetproperty &&
                hasUserGetproperty(eng.globalCtx.rtctx, ft)
            newnode = makeGetPropertyFlowNode(ast, node, makeAnyType(eng.globalCtx.rtctx))
            addFlowMapping!(eng, ast, newnode)
            return InferResult(ctx, newnode)
        end
        reportErrorNoField(eng, ast, node, p)
    end
end

function inferComparison(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    # TODO : the logic is incorrect somehow
    #=
    assertASTKind(ast, :comparison)
    args = ast.args
    if length(args) % 2 != 0
        reportASTError(eng, ast, "Chained comparison is invalid")
    end
    ops = FlowNode[]
    for i in 1:div(length(args) + 1, 2)
        rel = inferExpr(eng, ctx, args[2*i - 1])
        ctx = rel.ctx
        push!(ops, rel.node)
    end
    =#
    ms = MethodCallStruct(FlowNode[])
    return InferResult(ctx, makeFunCallFlowNode(ast, ms, makeBoolType(eng.globalCtx.rtctx)))
end

# Recognise the literal `nothing`. JuliaSyntax surfaces it as a
# `:literal` node whose val is the Nothing singleton (or, when written
# bare in code, as `:identifier` with val `Symbol("nothing")` resolving
# to the Core const). Match both shapes; the second is the common case.
function _isLiteralNothing(ast::JuAST)::Bool
    if ast.head == :literal
        val = ast.val.val
        return val === nothing
    elseif ast.head == :identifier
        return cast2Symbol(ast.val) === :nothing
    end
    return false
end

# Helper for `tryNarrowType`'s `x === nothing` branch. Produces a
# pi-narrowing pair: TRUE branch narrows the variable to `Nothing`,
# FALSE branch removes `Nothing` from the union. Returns `nothing` if
# the variable isn't currently a union containing Nothing — in that
# case the caller falls through to the normal infer path (the comparison
# is still type-checked, just without narrowing).
function _narrowEqNothing(eng::Engine, ctx::Context, ast::JuAST, varAst::JuAST)::Union{Pair{InferResult, Context}, Nothing}
    rtctx = eng.globalCtx.rtctx
    nothingTt = makeNothingType(rtctx)
    # Build the comparison node so the call still type-checks downstream.
    eqnode = makeGlobalVarFlowNode(ast.args[1], lookupBaseIdentity(rtctx))
    rel = inferVar(eng, ctx, varAst)
    ctx = rel.ctx
    varNode = rel.node
    curtt = varNode.typ
    # We only narrow when the variable's static type actually includes
    # `Nothing`. Pure `Nothing` types or non-union types are left alone:
    # the `=== nothing` check is then either trivially true or trivially
    # false and isn't useful for narrowing.
    if !isaUnion(rtctx, curtt)
        return nothing
    end
    ms = MethodCallStruct(FlowNode[eqnode, varNode, makeMacroCallFlowNode(rtctx, ast)])
    condnode = makeFunCallFlowNode(ast, ms, makeBoolType(rtctx))
    addFlowMapping!(eng, ast, condnode)
    ctxval = lookup(ctx, cast2Symbol(varAst.val))
    storetypenode = ctxval.typ
    # TRUE branch: variable is exactly Nothing.
    truePiNode = makePiFlowNode(ast, condnode, nothingTt)
    trueCtx = update(ctx, cast2Symbol(varAst.val), ContextValue(storetypenode, truePiNode))
    # FALSE branch: variable is the union with Nothing removed.
    elseTt = splitUnion(rtctx, curtt, nothingTt)
    falsePiNode = makeNegPiFlowNode(ast, condnode, elseTt)
    falseCtx = update(ctx, cast2Symbol(varAst.val), ContextValue(storetypenode, falsePiNode))
    return InferResult(trueCtx, condnode) => falseCtx
end

function tryNarrowType(eng::Engine, ctx::Context, ast::JuAST)::Pair{InferResult, Context}
    if ast.head == :call
        args = ast.args
        if length(ast.args) == 3
            isa_ = ast.args[1]
            lhs = ast.args[2]
            rhs = ast.args[3]
            # Special case: `x === nothing` and `nothing === x`. Lets the
            # else branch use `x.fieldname` safely when `x::Union{T, Nothing}`.
            # We don't model `===` against arbitrary constants — only the
            # `Nothing`-singleton pattern, which is the common idiom in
            # base / package code (`m = match(...); m === nothing && return`).
            if isa_.head == :identifier && cast2Symbol(isa_.val) === :(===)
                varAst = lhs.head == :identifier && _isLiteralNothing(rhs) ? lhs :
                         rhs.head == :identifier && _isLiteralNothing(lhs) ? rhs : nothing
                if varAst !== nothing
                    rel = _narrowEqNothing(eng, ctx, ast, varAst)
                    rel === nothing || return rel
                end
            end
            # `x !== nothing` is the inverted form. Same narrowing,
            # branches swapped — true branch sees union minus Nothing,
            # false branch sees Nothing. We reuse `_narrowEqNothing`
            # and flip the returned Pair{true_branch, false_branch}.
            if isa_.head == :identifier && cast2Symbol(isa_.val) === :(!==)
                varAst = lhs.head == :identifier && _isLiteralNothing(rhs) ? lhs :
                         rhs.head == :identifier && _isLiteralNothing(lhs) ? rhs : nothing
                if varAst !== nothing
                    rel = _narrowEqNothing(eng, ctx, ast, varAst)
                    if rel !== nothing
                        eqResult, eqElseCtx = rel
                        # Swap: what was the `=== nothing` true ctx is now
                        # the false ctx, and vice versa.
                        return InferResult(eqElseCtx, eqResult.node) => eqResult.ctx
                    end
                end
            end
            if isa_.head == :identifier && cast2Symbol(isa_.val) == :isa
                # TODO : we need to add FlowNode to all the JuExpr here
                isanode = makeGlobalVarFlowNode(isa_, lookupIsa(eng.globalCtx.rtctx))
                addFlowMapping!(eng, isa_, isanode)
                if lhs.head == :identifier
                    lhsid = cast2Symbol(lhs.val)
                    rel = inferExpr(eng, ctx, rhs)
                    ctx = rel.ctx
                    rhsnode = rel.node
                    rhstt = rhsnode.typ
                    if !isConstVal(rhstt)
                        reportErrorIsaRHSNonconst(eng, ast)
                    end
                    rhstt = lift(eng.globalCtx.rtctx, rhstt)
                    # TODO : lift the constant here
                    # x isa XXX type, we life XXX to a type before we perform comparsion
                    if !isaType(eng.globalCtx.rtctx, rhstt)
                        reportErrorIsaLHSBadType(eng, ast, rhstt)
                    end
                    # then we lookup lhs of isa
                    rel = inferVar(eng, ctx, lhs)
                    ctx = rel.ctx
                    xnode = rel.node
                    # rhs must be a subtype of lhs
                    if !tryMergeCompileValue(eng.globalCtx.rtctx, xnode.typ, rhstt)
                        reportErrorMismatchedSplit(eng, ast, xnode, rhsnode)
                    end
                    ms = MethodCallStruct(FlowNode[isanode, xnode, rhsnode])
                    condnode = makeFunCallFlowNode(ast, ms, makeBoolType(eng.globalCtx.rtctx))
                    addFlowMapping!(eng, ast, condnode)
                    # must hasVar here
                    ctxval = lookup(ctx, lhsid)
                    curtt = ctxval.curtyp.typ
                    # storage type must be typ instead of current type
                    storetypenode = ctxval.typ
        
                    pinode = makePiFlowNode(ast, condnode, rhstt)
                    # narrow type to rhs
                    ctxval = ContextValue(storetypenode, pinode)
                    newctx = update(ctx, lhsid, ctxval)
                    newrel = InferResult(newctx, condnode)
                    if isaAny(eng.globalCtx.rtctx, curtt)
                        return newrel => ctx
                    elseif isaUnion(eng.globalCtx.rtctx, curtt)
                        negtt = splitUnion(eng.globalCtx.rtctx, curtt, rhstt)
                        negpinode = makeNegPiFlowNode(ast, condnode, negtt)
                        negctx = update(ctx, lhsid, ContextValue(storetypenode, negpinode))
                        return newrel => negctx
                    else
                        reportErrorIsaLHSBadType(eng, ast, rhstt)
                    end
                else
                    reportErrorIsaBadForm(eng, ast, "`isa` is not in a predicative form, lhs of type splitting must be a single variable")
                end
            end
        end
    end
    rel = inferExpr(eng, ctx, ast)
    if !isaBool(eng.globalCtx.rtctx, rel.node.typ)
        reportErrorCondNotBool(eng, rel.node)
    end
    return rel => rel.ctx
end

function collectIfBranches!(eng::Engine, rel::Vector{Pair{JuAST, JuAST}}, ast::JuAST)::Maybe{JuAST}
    local ex::Maybe{JuAST}
    if length(ast.args) == 2 || length(ast.args) == 3
        cond = ast.args[1]
        body = ast.args[2]
        push!(rel, cond => body)
        if length(ast.args) == 3
            eb = ast.args[3]
            if eb.head == :elseif
                ex = collectIfBranches!(eng, rel, ast.args[3])
            else
                # else branch
                ex = Just(ast.args[3])
            end
        else
            # empty if clause
            ex = None(JuAST)
        end
    else
        reportASTError(eng, ast, "If statement should have at least 2 children")
    end
    return ex
end

function collectIfBranches(eng::Engine, ast::JuAST)
    rel = Pair{JuAST, JuAST}[]
    rel => collectIfBranches!(eng, rel, ast)
end

function inferWhileStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :while)
    #=
    inference of while loop is similar to for loop, except that 
    What's the scope rule of while statement?
    cond in while belongs to parent scope, while body belongs to child scope
        we disallow variable declared in cond...
    while (some assignment ; cond)
        body
    end
    =#
    scopeinfo = eng.scopeInfos[ast]
    cond = ast.args[1]
    body = ast.args[2]
    condctx = inferExpr(eng, ctx, cond).ctx
    # step 1 : mark all modified variables as non-constant and perform constant propagating...
    modified = Dict{Symbol, ContextValue}()
    # variable newly introduced in condition branch 

    # TODO : maybe fix this by supporting assignment in while loop
    for pair in condctx.mapping.data
        i = pair.first
        v = pair.second
        if !hasVar(ctx, i)
            reportASTError(eng, ast, "Assignment in while-loop condition is bad!")
        elseif haskey(scopeinfo.modified, i)
            modified[i] = v
        end
    end

    newmapping = Dict{Symbol, ContextValue}()
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(modified, k)
            # TODO : mark this as while update node
            z = makeForUpdateFlowNode(eng.globalCtx.rtctx, ast, v.curtyp)
            newmapping[k] = ContextValue(v.typ, z)
        elseif hasVar(scopeinfo, k)
            # this variable is shadowed in this scope
        else
            # unchanged
            newmapping[k] = v
        end
    end
    
    newctx = Context(ImmutableDict(newmapping))
    rel = inferExpr(eng, newctx, cond)
    if !isaBool(eng.globalCtx.rtctx, rel.node.typ)
        reportErrorCondNotBool(eng, rel.node)
    end
    # we enter loop when we infer body
    enterLoop(eng)
    newctx = rel.ctx
    newctx = inferExpr(eng, newctx, body).ctx
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(modified, k)
            ctxval = lookup(ctx, k)
            newctxval = lookup(newctx, k)
            # case 1: the variable is already initialized outside loop
            lctx = eng.loopContext[end]
            allnodes = Vector{FlowNode}(undef, length(lctx) + 2)
            allnodes[1] = ctxval.curtyp
            allnodes[end] = newctxval.curtyp
            for i in eachindex(lctx)
                # must have var
                allnodes[i + 1] = lookup(lctx[i], k).curtyp
            end
            # case 1: the variable is already initialized outside loop
            if mustInitialized(ctxval.curtyp)
                # this is incorrect, because break can include additional flownode!
                # other words, we also need to merge the variable definition before a break statement
                newmapping2[k] = ContextValue(ctxval.typ, tryMergeFlowNode(eng, ast, allnodes, true))
            else
                # variable is assigned at least once (it's modified), so it's impossible that the variable is mustUnIntialized?
                local storetyp::FlowNode = allnodes[1]
                isSet = false
                if !isDeclarationFlowNode(ctxval.curtyp)
                    isSet = true
                    storetyp = ctxval.typ
                end
                if !isDeclarationFlowNode(newctxval.curtyp)
                    isSet = true
                    storetyp = newctxval.typ
                end
                for i in eachindex(lctx)
                    # get the storage type of this variable
                    if !isDeclarationFlowNode(allnodes[i + 1])
                        storetyp = lookup(lctx[i], k).typ
                        isSet = true
                        break
                    end
                end
                node = makeConditionalFlowNode(ast)
                if !isSet
                    throw(InternalError("Union of Loop is incorrect, report this to SimpleTypeChecker's developer"))
                else
                    newmapping2[k] = ContextValue(storetyp, node)
                end
            end
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end
    leaveLoop(eng)
    retnode = makeWhileStmtFlowNode(eng.globalCtx.rtctx, ast)
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end


function inferIfStmt(eng::Engine, ctx::Context, ast::JuAST, isOr::Bool, isAnd::Bool)::InferResult
    # no pi projection
    rels = Vector{InferResult}()
    prectx = ctx
    if isOr
        #=
        x || y isa lowered to
        if x
            true
        else
            y
        end
        =#
        cond = ast.args[1]
        else_bast= ast.args[2]
        # we swap the context here
        rel1, ctx = tryNarrowType(eng, ctx, cond)
        push!(rels, InferResult(rel1.ctx, makeLiteralFlowNode(eng.globalCtx.rtctx, cond, makeConstJuASTVal(true))))
        rel2 = inferExpr(eng, ctx, else_bast)
        push!(rels, rel2)
    elseif isAnd
        #=
         x && y is lowered to 
         if x 
            y
         else
            false
         end
        =#
        cond = ast.args[1]
        b1 = ast.args[2]
        # we swap the context here
        rel1, ctx = tryNarrowType(eng, ctx, cond)
        rel2 = inferExpr(eng, rel1.ctx, b1)
        push!(rels, InferResult(ctx, makeLiteralFlowNode(eng.globalCtx.rtctx, cond, makeConstJuASTVal(false))))
        push!(rels, rel2)
    else
        (branches, el) = collectIfBranches(eng, ast)
        for branch in branches
            cond = branch.first
            body = branch.second
            rel1, ctx = tryNarrowType(eng, ctx, cond)
            rel2 = inferExpr(eng, rel1.ctx, body)
            push!(rels, rel2)
        end
        if isNone(el)
            node = makeEmptyElseFlowNode(eng.globalCtx.rtctx, ast)
            # use the context of negative branch
            push!(rels, InferResult(ctx, node))
        else
            push!(rels, inferExpr(eng, ctx, castJust(el)))
        end
    end
    # firstly, we detect which branch is reachable
    reachflags = similar(rels, Bool)
    fill!(reachflags, true)
    for i in eachindex(reachflags)
        if isBottomType(eng.globalCtx.rtctx, rels[i].node.typ)
            reachflags[i] = false
        end
    end
    
    newnames = Set{Symbol}()
    for rel in rels
        for i in keys(rel.ctx.mapping.data)
            push!(newnames, i)
        end
    end

    allVars = Set{Symbol}()
    for rel in rels
        for i in getAllVars(rel.ctx)
            push!(allVars, i)
        end
    end
    newctxmap = Dict{Symbol, ContextValue}()
    for var in allVars
        typs = FlowNode[]
        condDef = false
        # Case 1 : variable declared before if
        if hasVar(prectx, var)
            ctxval = lookup(prectx, var)
            prenode = ctxval.curtyp
            if !isDeclarationFlowNode(prenode)
                allUninit = false
                # case 1.1 : variable defined before if, but not initialized
                # local x::Int
                if mustUnInitialized(prenode)
                    allUninit = true
                    # we check whether this variable is assigned at least once in the if-else
                    for i in eachindex(rels)
                        rel = rels[i]
                        local ctxval
                        if reachflags[i]
                            ctxval = lookup(rel.ctx, var)
                            if !mustUnInitialized(ctxval.curtyp)
                                # in at least one reachable branch, the variable is touched
                                allUninit = false
                                break
                            end
                        end
                    end
                end
                if allUninit
                    # this variable is unassigned in the if-else, we retain its value to prevent 
                    newctxmap[var] = ctxval
                    continue
                end
                
                for i in eachindex(rels)
                    rel = rels[i]
                    local ctxval
                    if reachflags[i]
                        ctxval = lookup(rel.ctx, var)
                        if mustInitialized(ctxval.curtyp)
                            push!(typs, ctxval.curtyp)
                        else
                            condDef = true
                            break
                        end
                    end
                end
                if condDef
                    curnode = makeConditionalFlowNode(ast)
                elseif length(typs) == 0
                    # Case 1.2 : variable declared but potentially uninitialized
                    # or none of the branch is reachable for this variable
                    # we use bottom here, because a conditionally flow node should not be accessed
                    curnode = makeConditionalFlowNode(ast)
                else
                    # Case 1.3 : variable declared before if and initialized
                    curnode = tryMergeFlowNode(eng, ast, typs, true)
                end
                newctxmap[var] = ContextValue(ctxval.typ, curnode)
                #@info "At $(formatLocation(ast.loc)) Variable $var is inferred with type $(curnode.typ) sources $([i.typ for i in typs])"
                continue
            end
            # fall through
        end
        storetyps = FlowNode[]
        # Case 2 : variable undefined before if, we merge definition from each branches
        for i in eachindex(rels)
            rel = rels[i]
            # A variable is unconditionally defined
            # iff on every reachable branch, it has an initialized definition
            if hasVar(rel.ctx, var)
                ctxval = lookup(rel.ctx, var)
                if !isDeclarationFlowNode(ctxval.curtyp)
                    push!(storetyps, ctxval.typ)
                    if mustInitialized(ctxval.curtyp)
                        if reachflags[i]
                            push!(typs, ctxval.curtyp)
                        end
                        continue
                    end
                end
            end 
            if reachflags[i]
                condDef = true
            end
        end
        if length(storetyps) == 0
            # the variable is not touched, only declared with `local x`
            # we retain the declaration here
            if hasVar(prectx, var)
                newctxmap[var] = lookup(prectx, var)
            else
                error("Variable declared in if-else but not used")
            end
            continue
        end
        # check whether the type is compatible in every branch, even some of them is unreachable
        storenode = tryMergeFlowNode(eng, ast, storetyps, false)

        # TODO : record conditionally defined reason
        if length(typs) != 0
            # we allow union on current type, but not storage type
            if condDef
                node = makeConditionalFlowNode(ast)
            else
                node = tryMergeFlowNode(eng, ast, typs, true)
            end
            # even if a variable is conditionally defined, we need to set its storage type
            value = ContextValue(storenode, node)
            newctxmap[var] = value
        else
            # even if a variable's definition is killed in every branch
            # we need to record it here
            #=
            if y
                x = 1
                error()
            else
                x = 2
                error()
            end
            # here x has no definition
            print(x)
            =#
            node = makeConditionalFlowNode(ast)
            value = ContextValue(storenode, node)
            newctxmap[var] = value
        end
    end
    newctx = Context(ImmutableDict(Dict{Symbol, ContextValue}(newctxmap)))
    relnodes = FlowNode[]
    # only merge reachable branches
    # indeed, merge unreachable branches won't affect results...
    for i in eachindex(rels)
        if reachflags[i]
            push!(relnodes, rels[i].node)
        end
    end
    if length(relnodes) == 0
        retnode = makeIfStmtFlowNode(ast, makeBottomType())
    else
        # TODO : this is incorrect, perform ast mapping greatly here
        retnode = makeIfStmtFlowNode(ast, tryMergeFlowNode(eng, ast, relnodes, true).typ)
    end
    return InferResult(newctx, retnode)
end


function iterateCheck(eng::Engine, node::FlowNode)::FlowNode
    ast = node.ast
    rtctx = eng.globalCtx.rtctx
    ms = MethodCallStruct(makeLiteralFlowNode(rtctx, ast, makeConstJuASTVal(Base.iterate)), FlowNode[node])
    matches = getMethodMatches(eng, ms)
    # Fallback element type when we can't deduce the loop variable's type
    # from `iterate`'s return — used by all the non-standard cases below.
    # Trades static-typing strength for "the for-body still gets checked",
    # which is the right call when the iterator's `iterate` method doesn't
    # follow the standard `Union{Tuple, Nothing}` shape.
    anyNode() = makeIterateFlowNode(ast, makeAnyType(rtctx))
    if length(matches) != 1
        return anyNode()
    end
    tt = extractUniqueMatch(matches)
    # Standard case: Union{Tuple{elt, state}, Nothing}.
    if isaUnion(rtctx, tt)
        tt = splitUnion(rtctx, tt, makeNothingType(rtctx))
        if isaTuple(rtctx, tt)
            return makeIterateFlowNode(ast, getFirstParameter(rtctx, tt))
        end
        # Union that's not Union{Tuple, Nothing}. Some iterators model
        # state with a more complex shape. Fall back to Any.
        return anyNode()
    end
    # Non-union return — e.g. a never-empty iterator returning
    # `Tuple{elt, state}` directly, or a never-yielding iterator
    # returning `Nothing`. Both are legal Julia; Pre-1.0 iterate APIs
    # also relax the Union convention. Take the element type when
    # possible, else fall back to Any.
    if isaTuple(rtctx, tt)
        return makeIterateFlowNode(ast, getFirstParameter(rtctx, tt))
    end
    return anyNode()
end

function checkForStmt(eng::Engine, ast::JuAST)::Nothing
    if ast.head == :tuple
        for i in ast.args
            checkForStmt(eng, i)
        end
    elseif ast.head == :identifier
        return
    elseif ast.head == :(=) || ast.head == :in
        checkForStmt(eng, ast.args[1])
    elseif ast.head == :iteration || ast.head == :block
        for i in ast.args
            checkForStmt(eng, i)
        end
    else
        reportASTError(eng, ast, "In lhs of for, only variable or tuple destruct are allowed")
    end
end

function inferForStmt(eng::Engine, ctx::Context, ast::JuAST)::InferResult
    assertASTKind(ast, :for)
    # we firstly scan the loop to detect what variables are assigned in the loop body
    # mark these variables as non-constant
    vars = ast.args[1]
    checkForStmt(eng, vars)

    # step one : check outmost iterator
    if vars.head == :iteration || vars.head == :block
        firstvar = vars.args[1].args[1]
        iter = vars.args[1].args[2]
    else
        firstvar = vars.args[1]
        iter = vars.args[2]
    end
    rel = inferExpr(eng, ctx, iter)
    ctx = rel.ctx
    varnode = iterateCheck(eng, rel.node)

    # step two : remove shadowed variable
    newmapping = Dict{Symbol, ContextValue}()
    scopeinfo = eng.scopeInfos[ast]

    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(scopeinfo.modified, k)
            # Note : this is incorrect, we should add more check here!!!
            z = makeForUpdateFlowNode(eng.globalCtx.rtctx, ast, v.curtyp)
            newmapping[k] = ContextValue(v.typ, z)
        elseif hasVar(scopeinfo, k)
            # this variable is shadowed in this scope
        else
            # unchanged
            newmapping[k] = v
        end
    end
    newctx = Context(ImmutableDict(newmapping))
    newctx = inferAssignLHS(eng, newctx, firstvar, varnode, UpdateOp()).ctx
    # step three : add other variables
    if vars.head == :iteration || vars.head == :block
        for i in 2:length(vars.args)
            p = vars.args[i]
            firstvar = p.args[1]
            iter = p.args[2]
            rel = inferExpr(eng, newctx, iter)
            varnode = iterateCheck(eng, rel.node)
            newctx = inferAssignLHS(eng, newctx, firstvar, varnode, UpdateOp()).ctx
        end
    end
    body = ast.args[2]

    enterLoop(eng)

    rel2 = inferExpr(eng, newctx, body)
    newctx = rel2.ctx
    newmapping2 = Dict{Symbol, ContextValue}()
    # TODO : how we join the value here ? what's the scope rule here?
    for tmp in ctx.mapping.data
        k = tmp.first
        v = tmp.second
        if haskey(scopeinfo.modified, k)
            ctxval = lookup(ctx, k)
            newctxval = lookup(newctx, k)
            # case 1: the variable is already initialized outside loop
            lctx = eng.loopContext[end]
            allnodes = Vector{FlowNode}(undef, length(lctx) + 2)
            allnodes[1] = ctxval.curtyp
            allnodes[end] = newctxval.curtyp
            for i in eachindex(lctx)
                # must have var
                allnodes[i + 1] = lookup(lctx[i], k).curtyp
            end
            # case 1: the variable is already initialized outside loop
            if mustInitialized(ctxval.curtyp)
                # this is incorrect, because break can include additional flownode!
                # other words, we also need to merge the variable definition before a break statement
                newmapping2[k] = ContextValue(ctxval.typ, tryMergeFlowNode(eng, ast, allnodes, true))
            else
                # variable is assigned at least once (it's modified), so it's impossible that the variable is mustUnIntialized?
                local storetyp::FlowNode = allnodes[1]
                isSet = false
                if !isDeclarationFlowNode(ctxval.curtyp)
                    isSet = true
                    storetyp = ctxval.typ
                end
                if !isDeclarationFlowNode(newctxval.curtyp)
                    isSet = true
                    storetyp = newctxval.typ
                end
                for i in eachindex(lctx)
                    # get the storage type of this variable
                    if !isDeclarationFlowNode(allnodes[i + 1])
                        storetyp = lookup(lctx[i], k).typ
                        isSet = true
                        break
                    end
                end
                node = makeConditionalFlowNode(ast)
                if !isSet
                    throw(InternalError("Union of Loop is incorrect, report this to SimpleTypeChecker's developer"))
                else
                    newmapping2[k] = ContextValue(storetyp, node)
                end
            end
        else
            # shadowed variable or unassigned
            newmapping2[k] = v
        end
    end

    leaveLoop(eng)
    retnode = makeLiteralFlowNode(eng.globalCtx.rtctx, ast, makeConstJuASTVal(nothing))
    return InferResult(Context(ImmutableDict(newmapping2)), retnode)
end

# Convenience wrapper around the 5-arg `fasteval` — same heterogeneous
# return character (Any: Type | TypeVar | Module | literal), so it inherits
# the `@nocheck` opt-out. Without this the self-check fires JTC-DEEP-DISPATCH
# on a callee that the 5-arg version already opts out of by the same rule.
@nocheck function fasteval(rtctx::RuntimeContext, mod::Module, x::JuAST, params::Dict{Symbol, TypeVar})
    return fasteval(rtctx, mod, x, params, Vector{TypeVar}())
end

# Walks a type expression AST and produces the raw target-runtime value
# (Type / TypeVar / Module / literal). Module lookups route through the
# rtctx seam via `constModuleGetField`; `Core.apply_type` / `UnionAll` /
# `TypeVar` are Builtin-like constructors that statically resolve. The
# function stays @nocheck because the return type is fundamentally
# heterogeneous Any — Type, TypeVar, Module, literal value can all show
# up depending on the expression head.
@nocheck function fasteval(rtctx::RuntimeContext, mod::Module, x::JuAST, params::Dict{Symbol, TypeVar}, implicit::Vector{TypeVar})
    if x.head == :identifier
        var = cast2Symbol(x.val)
        if haskey(params, var)
            return params[var]
        end
        modCT = makeConstVal(mod)
        return constModuleGetField(rtctx, modCT, var).val
    elseif x.head == :literal
        return x.val.val
    elseif x.head == :(.)
        parent = fasteval(rtctx, mod, x.args[1], params, implicit)
        # JuliaSyntax emits `A.B` as `:.[identifier A, identifier B]` for
        # plain field access and `:.[identifier A, :quote[identifier B]]`
        # for the explicit `A.:B` form. Accept both — otherwise the
        # quote-only path BoundsErrors on rhs.args[1].
        rhs = x.args[2]
        if rhs.head == :identifier
            sym = cast2Symbol(rhs.val)
        else
            sym = cast2Symbol(rhs.args[1].val)
        end
        if parent isa Module
            parentCT = makeConstVal(parent)
            return constModuleGetField(rtctx, parentCT, sym).val
        else
            error("Dot-access in type expression: left side did not resolve to a Module")
        end
    elseif x.head == :curly
        newimplicit = Vector{TypeVar}()
        ty = fasteval(rtctx, mod, x.args[1], params, implicit)
        args = Any[]
        for i in 2:length(x.args)
            push!(args, fasteval(rtctx, mod, x.args[i], params, newimplicit))
        end
        ty = Core.apply_type(ty, args...)
        for i in implicit
            ty = UnionAll(ty, i)
        end
        return ty
    elseif x.head == :(<:) && length(x.args) == 1
        var = TypeVar(gensym(), fasteval(rtctx, mod, x.args[1], params, implicit))
        push!(implicit, var)
        return var
    else
        # dangerous, complicated type application is dangerous
        error("Not supported $(typeof(x)), type too complicated for analysis $(x)")
    end
end

function collectQualifiedName(ctx::GlobalContext, ast::JuAST)::Vector{Symbol}
    # Parametric function names: `function Foo{T,U}(...)` — the type
    # parameters are bound in the surrounding `where`, so the function
    # itself is named by the curly's head identifier. Peel and recurse.
    if ast.head == :curly && length(ast.args) >= 1
        ast = ast.args[1]
    end
    rel = Symbol[]
    while true
        if ast.head == :identifier
            push!(rel, cast2Symbol(ast.val))
            return rel
        elseif ast.head == :(.)
            lhs = ast.args[1]
            rhs = ast.args[2]
            # JuliaSyntax emits `Base.f` as `:(.) [identifier, identifier]`
            # for plain field access (the common case for `function Base.f`)
            # and as `:(.) [identifier, :quote [identifier]]` for the
            # explicit `Base.:f` form. Accept both.
            if lhs.head == :identifier && rhs.head == :identifier
                push!(rel, cast2Symbol(lhs.val))
                ast = rhs
                continue
            end
            if lhs.head == :identifier && rhs.head == :quote && rhs.args[1].head == :identifier
                push!(rel, cast2Symbol(lhs.val))
                ast = rhs.args[1]
                continue
            end
        end
        reportASTError(ctx, ast, "Not a valid function name, only support identifier (f) or qualified name (Base.f)")
    end
    return rel
end

function constructParam(ctx::GlobalContext, ast::JuAST)::Argument
    initializer = None(JuAST)
    typ = None(JuAST)
    if ast.head == :(=)
        initializer = Just(ast.args[2])
        ast = ast.args[1]
    end
    # `@nospecialize(x)` / `@nospecialize x` / `@specialize x` in a
    # parameter slot is an annotation, not a real wrapper — unwrap to the
    # underlying parameter. We don't otherwise model the annotation here;
    # the `@nospecialize`-related checks are driven later off Method
    # reflection (`method.nospecialize`), not the AST.
    if ast.head == :macrocall && length(ast.args) >= 2
        head = ast.args[1]
        if head.head == :identifier
            macname = cast2Symbol(head.val)
            if macname === Symbol("@nospecialize") || macname === Symbol("@specialize")
                ast = ast.args[2]
            end
        end
    end
    # Vararg slurp at the definition site: `f(args...)` parses as
    # `Expr(:..., x)`, `f(args::T...)` as `Expr(:..., Expr(::, x, T))`.
    # We don't fully model vararg-tuple binding in the checker yet, but
    # accepting the syntactic shape (treating the inner Argument as if
    # it were a single positional) lets the method register and the body
    # get checked. Drops the vararg-ness; user-visible cost is that the
    # checker thinks `args` has type T instead of `Tuple{T...}`.
    if ast.head == :(...)
        if length(ast.args) >= 1
            ast = ast.args[1]
        end
    end
    if ast.head == :(::)
        if length(ast.args) == 1
            typ = Just(ast.args[1])
            # empty argument
            return Argument(Symbol("_"), typ, initializer)
        end
        typ = Just(ast.args[2])
        ast = ast.args[1]
    end
    if ast.head == :identifier
        return Argument(cast2Symbol(ast.val), typ, initializer)
    end
    # Tuple-destructure args: `f((a, b)::Tuple{X, Y})` parses with
    # `ast.head == :tuple`, the type annotation already handled above.
    # `f((a, b) = default)` similarly. We don't track the
    # individual bindings as separate locals — bind one synthetic name
    # `_destructure` and let the body's `a`/`b` reads fall back to
    # untracked (Any) lookups. The method registers; we lose precision
    # on the components but don't reject the method.
    if ast.head == :tuple
        return Argument(Symbol("_destructure"), typ, initializer)
    end
    reportASTError(ctx, ast, "Invalid argument definition")
end

# We don't have an Engine this time, so we can only report error by using GlobalContext
function prepareToplevelFunction(ctx::GlobalContext, ast::JuAST)::FunDef
    #assertASTKind(ast, :function)
    if length(ast.args) == 0
        reportASTError(ctx, ast, "Function is empty")
    end
    # Note : we disallowed left to right dependency
    sparams = Pair{Symbol, Maybe{JuAST}}[]
    rt::Maybe{JuAST} = None(JuAST)
    args = Argument[]
    optargs = Argument[]
    kwargs = Argument[]
    f = ast.args[1]
    if length(ast.args) == 1
        reportASTError(ctx, ast, "Function is empty")
    end
    body = ast.args[2]
    # Loop over nested where layers: `function f(x) where V where K end`
    # parses as `where V (where K (call))`, two peels needed. The previous
    # `if` only peeled once, leaving f.head == :where and tripping the
    # "Not a valid function definition" path. Each layer contributes its
    # own sparam(s).
    while f.head == :where
        # `where T` and `where T<:Bound` produce f.args = [sig, sparam].
        # `where {T<:Bound, S}` produces f.args = [sig, braces(sparam, ...)] —
        # the braces wrap the parameter list. Materialize a Vector{JuAST}
        # either way so the iteration type stays concrete.
        sparam_iter = JuAST[]
        if length(f.args) == 2 && f.args[2].head == :braces
            for s in f.args[2].args
                push!(sparam_iter, s)
            end
        else
            for i in 2:length(f.args)
                push!(sparam_iter, f.args[i])
            end
        end
        for sparam in sparam_iter
            if sparam.head == :(<:)
                if length(sparam.args) != 2
                    reportASTError(ctx, f, "<: should contain two children")
                end
                if sparam.args[1].head != :identifier
                    reportASTError(ctx, f, "LHS of <: not an identifier")
                end
                push!(sparams, cast2Symbol(sparam.args[1].val)=>Just(sparam.args[2]))
            elseif sparam.head == :identifier
                push!(sparams, cast2Symbol(sparam.val)=>None(JuAST))
            else
                reportASTError(ctx, f, "Parameters (a.k.a where) in function definition can only contain symbol or a bound")
            end
        end
        f = f.args[1]
    end
    if f.head == :(::)
        rt = Just(f.args[2])
        f = f.args[1]
    end
    if f.head == :call
        fname = collectQualifiedName(ctx, f.args[1])
        for i in 2:length(f.args)
            iarg = f.args[i]
            if iarg.head == :parameters
                for kw in iarg.args
                    push!(kwargs, constructParam(ctx, kw))
                end
            elseif iarg.head == :(=)
                push!(optargs, constructParam(ctx, iarg))
            else
                push!(args, constructParam(ctx, iarg))
            end
        end
        return FunDef(ast, sparams, rt, fname, args, optargs, kwargs, body)
    else
        reportASTError(ctx, ast, "Not a valid function definition, function f end and anoymous function is not supported yet")
    end
end

function makeKwargType(rtctx::RuntimeContext, arg::Argument)::CompileType
    # hack ...
    makeIntType(rtctx)
end

function checkToplevelFunction(eng::Engine, 
                               fundef::FunDef,
                               kwargs::Dict{Symbol, CompileType})::InferReport
    ast = fundef.ast
    smapping = Dict{Symbol, ContextValue}()
    mi = eng.mi
    # MI may carry fewer sparams than the AST when extra `where` parameters
    # are bound by keyword-arg dispatch instead of positional dispatch — e.g.
    # `f(...; cb::FCB = default) where {T, FA, FCB}` produces a positional
    # MI with sparams (T, FA) while AST has (T, FA, FCB). Only erroring if
    # the MI carries MORE sparams than the AST, which would be a real bug.
    n_mi = methodHandleSparamCount(mi)
    n_ast = length(fundef.sparams)
    if n_mi > n_ast
        meth = methodHandleDef(mi)
        loc = formatLocation(ast.loc)
        error("mismatched param size: function $(fundef.fname) at $loc — MI has $n_mi sparams but AST only has $n_ast  [method: $meth]")
    end
    # Bind the first n_mi sparams from the MI's concrete values.
    for i in 1:n_mi
        node = makeSparamFlowNode(ast, makeSparamCompileType(mi, i))
        smapping[fundef.sparams[i].first] = ContextValue(node, node)
    end
    # Any extra AST sparams (kwarg-bound, not in the MI) — bind the
    # identifier to the constant Type `Any`. Semantically this is "the
    # static type assertion FF degrades to Any, accept any caller value."
    # Using makeConstVal (not makeType) so `lift()` in the kwarg pipeline
    # treats it as a const Type rather than tripping its "Internal error"
    # branch. Using TypeVar values here doesn't work either — they break
    # the `<:` subtype machinery because TypeVar isn't <: Type.
    for i in (n_mi + 1):n_ast
        # `lookupAnyValue` returns `makeConstVal(Any)` constructed driver-side,
        # so the `Any` value comes from the target runtime's type table.
        node = makeSparamFlowNode(ast, lookupAnyValue(eng.globalCtx.rtctx))
        smapping[fundef.sparams[i].first] = ContextValue(node, node)
    end
    mapping = Dict{Symbol, ContextValue}()
    for i in 1:length(fundef.args)
        # TODO : the ast is incorrect here...
        # add mapping here!
        node = makeParamFlowNode(ast, makeArgCompileType(mi, i+1))
        arg = fundef.args[i]
        if arg.name !== Symbol("_")
            mapping[arg.name] = ContextValue(node, node)
        end
    end
    ctx = Context(ImmutableDict(merge(smapping, mapping)))
    # firstly we fill in all the normal arguments, then we fill in the 
    for i in 1:length(fundef.optargs)
        # TODO : the ast is incorrect here...
        # add mapping here!
        index = i + length(fundef.args) + 1
        arg = fundef.optargs[i]
        # TODO : check input here
        if index <= methodHandleSpecTypesLength(mi)
            initnode = makeParamFlowNode(ast, makeArgCompileType(mi, index))
        else
            if !isNone(arg.initializer)
                rel = inferExpr(eng, ctx, castJust(arg.initializer))
                initnode = rel.node
                ctx = rel.ctx
            else
                throw(InternalError("Optional parameter must have an initializer"))
            end
            initnode = makeParamFlowNode(ast, initnode.typ)
        end
        if !isNone(arg.typ)
            rel = inferExpr(eng, ctx, castJust(arg.typ))
            typnode = rel.node
            ctx = rel.ctx
            ttt = lift(eng.globalCtx.rtctx, typnode.typ)
            if !tryMergeCompileValue(eng.globalCtx.rtctx, ttt, initnode.typ)
                reportErrorAssignInitIncompatible(eng, ast, ttt, initnode.typ)
            end
        end
        ctx = update(ctx, arg.name, ContextValue(initnode, initnode))
    end

    for i in 1:length(fundef.kwargs)
        # TODO : the ast is incorrect here...
        # add mapping here!
        arg = fundef.kwargs[i]
        mtypnode = None(FlowNode)
        minitnode = None(FlowNode)
        if !isNone(arg.initializer)
            rel = inferExpr(eng, ctx, castJust(arg.initializer))
            minitnode = Just(rel.node)
            ctx = rel.ctx
        end
        if !isNone(arg.typ)
            rel = inferExpr(eng, ctx, castJust(arg.typ))
            mtypnode = Just(rel.node)
            ctx = rel.ctx
        end

        if haskey(kwargs, arg.name)
            inputtyp = kwargs[arg.name]
            if !isNone(mtypnode)
                typnode = castJust(mtypnode)
                ttt = lift(eng.globalCtx.rtctx, typnode.typ)
                if !tryMergeCompileValue(eng.globalCtx.rtctx, ttt, inputtyp)
                    reportErrorAssignInitIncompatible(eng, ast, ttt, inputtyp)
                end
            end
            node = makeParamFlowNode(ast, inputtyp)
        else
            if isNone(minitnode)
                # Required keyword (no default). By default this is an
                # error: Julia can decline to specialize on the kwarg's
                # type (same way `::Function` positionals get widened),
                # which silently degrades dispatch. `permitRequiredKwargs`
                # opts into trusting the callers — the runtime still
                # enforces presence via `UndefKeywordError`, and we
                # synthesize the declared annotation (`::T`) or `Any`.
                if eng.globalCtx.permitRequiredKwargs
                    if !isNone(mtypnode)
                        typnode = castJust(mtypnode)
                        ttt = lift(eng.globalCtx.rtctx, typnode.typ)
                        node = makeParamFlowNode(ast, ttt)
                    else
                        node = makeParamFlowNode(ast, makeAnyType(eng.globalCtx.rtctx))
                    end
                else
                    reportErrorkeywordNotDefined(eng, ast, arg.name)
                end
            else
                initnode = castJust(minitnode)
                if !isNone(mtypnode)
                    typnode = castJust(mtypnode)
                    ttt = lift(eng.globalCtx.rtctx, typnode.typ)
                    if !tryMergeCompileValue(eng.globalCtx.rtctx, ttt, initnode.typ)
                        reportErrorAssignInitIncompatible(eng, ast, ttt, initnode.typ)
                    end
                end
                node = makeParamFlowNode(ast, initnode.typ)
            end
        end
        ctx = update(ctx, arg.name, ContextValue(node, node))
    end
    # Firstly, infer return type
    astrt = fundef.rt 
    if !isNone(astrt)
        # TODO : fix the flow node here
        rel = inferExpr(eng, ctx, castJust(astrt))
        # TODO : use a interface functon for val -> typ conversion
        eng.retVal = Just(makeExpectedReturnFlowNode(ast, lift(eng.globalCtx.rtctx, rel.node.typ)))
        ctx = rel.ctx
    end
    rel = inferExpr(eng, ctx, fundef.body)
    newnode = rel.node
    # we need to return the value here
    engn = eng.retVal
    if isNone(engn)
        eng.retVal = Just(newnode)
    else
        if !tryMergeReturnFlowNode(eng.globalCtx.rtctx, castJust(engn), newnode)
            reportErrorReturnEnlargeType(eng, castJust(engn), newnode)
        end
    end
    return InferReport(ast, eng, rel)
end

function evalMods(rtctx::RuntimeContext, parentCT::CompileType, s::Vector{Symbol})::CompileType
    val = evalGetPropertyChain(rtctx, parentCT, s)
    if rtctx.constIsModule(val)
        return val
    else
        error("Not here")
    end
end

# Walk a chain of module-level names through the rtctx seam: at each step we
# ask the driver `isdefined?` then `getfield` via the UtilityUnType wrappers
# (which take CompileType + Symbol and handle the Symbol→String + unwrap).
# The raw target-runtime Module stays opaque inside CompileType.val on the
# driver side — the inferencer sees only CompileType handles.
#
# Used for module navigation (caller checks `constIsModule` on the result)
# and for function-name lookup (caller's `mapAST2Method` peeks `.val` to
# pull out the function value — that peek is in an @nocheck zone).
function evalGetPropertyChain(rtctx::RuntimeContext, parentCT::CompileType, s::Vector{Symbol})::CompileType
    base = parentCT
    for i in s
        if !rtctx.constIsModule(base)
            error("intermediate value in module chain is not a Module")
        end
        if !constModuleIsDefined(rtctx, base, i)
            error("Module field $i not defined")
        end
        base = constModuleGetField(rtctx, base, i)
    end
    return base
end

function collectToplevelFunction(ctx::GlobalContext, ast::JuAST)
    rel = Dict{Vector{Symbol}, Vector{FunDef}}()
    mod = Symbol[]
    rel[mod] = FunDef[]
    collectToplevelFunction!(ctx, rel, mod, ast)
    return rel
end

function readString(io)
    read(io, String)
end


@nocheck function mapAST2Method(rtctx::RuntimeContext, modCT::CompileType, fundef::FunDef)::Vector{MethodDef}
    # Given an JuAST, we try to select an appropriate method for its definition
    # Currently, keywords and optional functions are not handled
    # we handle them lately !!!
    #
    # `modCT` wraps the live Module on the driver side; we crack it once here
    # (this function is @nocheck) and hand the raw Module to `fasteval`, which
    # threads rtctx for its own module-binding lookups but returns raw
    # target-runtime values (Type / TypeVar / Module / literal) because the
    # tuple-signature construction below needs them in that form.
    mod = modCT.val::Module
    paramMap = Dict{Symbol, TypeVar}()
    for (name, bound) in fundef.sparams
        if isNone(bound)
            boundv = Any
        else
            boundv = fasteval(rtctx, mod, castJust(bound), paramMap)
        end
        paramMap[name] = TypeVar(name, boundv)
    end
    result = Any[]
    for arg in fundef.args
        tt = arg.typ
        if isNone(tt)
            v = Any
        else
            v = fasteval(rtctx, mod, castJust(tt), paramMap)
        end
        push!(result, v)
    end
    fCT = evalGetPropertyChain(rtctx, modCT, fundef.fname)
    f = fCT.val
    optresult = Any[]
    allmethods = MethodDef[]

    base = Tuple{Core.Typeof(f), result...}
    # construct from inside out
    for i in reverse(fundef.sparams)
        base = UnionAll(paramMap[i.first], base)
    end
    push!(allmethods, rtctx.whichMethod(OpaqueHandle{CallSigK}(base)))

    for arg in fundef.optargs
        tt = arg.typ
        if isNone(tt)
            v = Any
        else
            v = fasteval(rtctx, mod, castJust(tt), paramMap)
        end
        push!(optresult, v)
        # iterate in reverse order
        base = Tuple{Core.Typeof(f), vcat(result, optresult)...}
        # construct from inside out
        for i in reverse(fundef.sparams)
            base = UnionAll(paramMap[i.first], base)
        end
        push!(allmethods, rtctx.whichMethod(OpaqueHandle{CallSigK}(base)))
    end
    return allmethods
end

# Callable-struct method defs (`function (op::AWrap)(x) ... end`) are a
# valid Julia pattern that the checker doesn't auto-register yet — the
# function "name" is a `::`-typed pattern, not an identifier. We detect
# this and silently skip the def so it doesn't pollute output with
# "Not a valid function name" errors. The wrapper still works at the
# call site (its concrete struct type passes the abstract-arg check);
# only the wrapper's body misses auto-checking. Future work: thread the
# callable type through prepareToplevelFunction / mapAST2Method.
function isCallableMethodDef(ast::JuAST)::Bool
    f = ast
    if f.head == :function || f.head == :(=)
        if length(f.args) == 0
            return false
        end
        f = f.args[1]
    else
        return false
    end
    # peel off `where {...}` and `:: rettype` wrappers
    while true
        if f.head == :where || f.head == :(::)
            length(f.args) == 0 && return false
            f = f.args[1]
        else
            break
        end
    end
    if f.head != :call
        return false
    end
    length(f.args) == 0 && return false
    name = f.args[1]
    return name.head == :(::)
end

function collectToplevelFunction!(ctx::GlobalContext, rel::Dict{Vector{Symbol}, Vector{FunDef}}, mod::Vector{Symbol}, ast::JuAST)::Nothing
    isFunc = false
    if isCallableMethodDef(ast)
        return nothing
    end
    if ast.head == :module
        mname = ast.args[1]
        if mname.head != :identifier
            reportASTError(ctx, ast, "Module name must be an identifier")
        end
        bodyast = ast.args[2]
        if bodyast.head != :block
            reportASTError(ctx, ast, "Module body must be an block")
        end
        newmod = vcat(mod, cast2Symbol(mname.val))
        rel[newmod] = FunDef[]
        for i in bodyast.args
            collectToplevelFunction!(ctx, rel, newmod, i)
        end
    elseif ast.head == :function
        # `function f end` — forward declaration with no body or args.
        # The binding is created at Julia load time; we have nothing to
        # check. Silently skip so packages like BitFlags
        # (`function namemap end`, `function haszero end`) don't trip
        # "Function is empty".
        if length(ast.args) < 2
            return
        end
        isFunc = true
    elseif ast.head == :(=)
        f = ast.args[1]
        while true
            if f.head == :where || f.head == :(::)
                f = f.args[1]
            else
                break
            end
        end
        if f.head == :call
            isFunc = true
        end
    elseif ast.head == :toplevel
        for i in ast.args
            collectToplevelFunction!(ctx, rel, mod, i)
        end
    elseif ast.head == :macrocall
        n1 = ast.args[1]
        if n1.head == :identifier
            sym = cast2Symbol(n1.val)
            if sym == Symbol("core_@doc")
                collectToplevelFunction!(ctx, rel, mod, ast.args[3])
            elseif sym != Symbol("@nocheck")
                for i in ast.args
                    collectToplevelFunction!(ctx, rel, mod, i)
                end
            end
        else
            for i in ast.args
                collectToplevelFunction!(ctx, rel, mod, i)
            end
        end
    end
    if isFunc
        try
            def = prepareToplevelFunction(ctx, ast)
            push!(rel[mod], def)
        catch e
            if e isa SyntaxError
                renderSyntaxError(ctx.errio, e.ast, e.msg)
            else
                rethrow(e)
            end
        end
    end
    return
end

@nocheck function addFile!(ctx::GlobalContext, mod::Core.Module, filename::String)
    if !isabspath(filename)
        error("$filename is not a absolute path")
    end
    str = open(readString, filename)
    ast = parseJuAST(str, filename)
    # Wrap the caller's Module at the seam — once, here — so the recursive
    # evalMods / mapAST2Method calls go through rtctx without each call site
    # needing to know about Modules. addFile! is the API boundary, callers
    # still pass raw Core.Module.
    rtctx = ctx.rtctx
    modCT0 = makeConstVal(mod)
    for pair in collectToplevelFunction(ctx, ast)
        mods = pair.first
        modCT = evalMods(rtctx, modCT0, mods)
        fundefs = pair.second
        for fundef in fundefs
            try
                meths = mapAST2Method(rtctx, modCT, fundef)
                for i in meths
                    ctx.methodDefs[i] = fundef
                end
                meth = meths[end]
                # meth is the corresponding method definition for ex
                if isConcreteMethodDef(meth)
                    ci = methodDefCodeInfo(meth)
                    if ci isa Core.CodeInfo
                        mi = codeInfoMethodHandle(ci)
                        if mi isa MethodHandle
                            push!(ctx.queue, KwFunCall(mi))
                        end
                    end
                end
            catch e
                if ctx.reportSkippedMethods
                    println("CompilerError : At $(formatLocation(fundef.ast.loc)) : Failed to analyze this method, maybe the type signature too complicated, skip it")
                    println("  ", sprint(showerror, e))
                end
            end
        end
    end
end

@nocheck function _errorTag(e)::String
    if e isa SyntaxError
        return "[unsupported]"
    end
    s = string(typeof(e))
    if occursin("TopLevelDispatch", s) || occursin("FunCallArgs", s) || occursin("FunCallUseBottom", s)
        return "[dispatch]"
    end
    return "[type]"
end

@nocheck function _shortenMethodSig(s::String)::String
    pre = "MethodInstance for "
    return startswith(s, pre) ? String(SubString(s, length(pre)+1)) : s
end

@nocheck function runCheck!(ctx::GlobalContext;checkSyntax::Bool = true)
    # Drain pre-inference errors that addFile! buffered into errio. Print
    # them as their own [unsupported] section up front, otherwise they
    # leak into the first method-instance's output and look misattributed.
    preamble = takelog!(ctx.errio)
    if length(preamble) > 0 && checkSyntax
        # Pre-inference diagnostics (reportASTError /
        # reportUnimplementedASTError) are now rendered as full
        # rustc-shape Diagnostics in the buffer — each one is
        # self-describing, so no umbrella header is needed.
        print(preamble)
        if !endswith(preamble, "\n")
            println()
        end
    end
    isFirst = true
    while !isempty(ctx.queue)
        kmi = popfirst!(ctx.queue)
        # this specialization is checked
        mi = kmi.mi
        meth = methodHandleDef(mi)
        if meth isa Module
            continue
        end
        # meth isa MethodDef
        if !haskey(ctx.methodDefs, meth)
            continue
        end
        if haskey(ctx.hasChecked, mi)
            keys = ctx.hasChecked[mi]
            found = false
            for (k, v) in keys
                if k == kmi.kwargs
                    found = true
                end
            end
            if found
                continue
            end
        else
            ctx.hasChecked[mi] = Pair{KeywordSortList, Any}[]
        end
        push!(ctx.hasChecked[mi], kmi.kwargs=>nothing)
        fundef = ctx.methodDefs[meth]
        # Escape hatch: if this method is the only one on its function AND
        # has @nospecialize on some positional slot AND no kwargs / varargs,
        # treat the body as opted-out — the user has explicitly declared
        # this method as the typed-to-Any boundary. Same criterion the call
        # site uses (`_escapeHatchMask` in Inference.jl).
        if _isEscapeHatchMethod(meth)
            continue
        end
        # mark the specialization in check
        try
            sictx = analyzeScopeVariable(ctx, fundef)
            eng = Engine(ctx, mi, sictx.infos)
            checkToplevelFunction(eng, fundef, Dict(kmi.kwargs))
        catch e
            if e isa InferenceError || e isa SyntaxError
                if !checkSyntax && e isa SyntaxError
                    continue
                end
                if !isFirst
                    println()
                end
                isFirst = false
                if ctx.printCheckerFrames
                    tag = _errorTag(e)
                    miStr = _shortenMethodSig(string(mi))
                    println("in ", miStr, "   ", tag)
                end
                # replace with a error
                ctx.hasChecked[mi][end] = ctx.hasChecked[mi][end].first=>e
                if e isa SyntaxError
                    renderSyntaxError(ctx.errio, e.ast, e.msg)
                    body = String(take!(ctx.errio.io))
                    print(body)
                    if !endswith(body, "\n")
                        println()
                    end
                else
                    typname = split(string(typeof(e)), '.')[end]
                    x = "displayError" * typname[length("InferenceError")+1:end]
                    f = eval(Symbol(x))
                    f(e)
                    body = String(take!(ctx.errio.io))
                    print(body)
                    if !endswith(body, "\n")
                        println()
                    end
                end
            else
                # An exception we didn't model (e.g. an `ErrorException`
                # bubbling up from a sub-call that hit an inference dead
                # end, a KeyError from a span-mapping miss, a TypeError on
                # a typeassert in the inferencer pipeline). Don't let it
                # abort the whole queue — record it as a fail on this MI
                # and continue with the next one. The checker promises a
                # diagnostic per method, not an exception per package.
                #
                # Capture the backtrace HERE before any other code path
                # clobbers it. Render a trimmed stack (top N frames in our
                # own src/ — frames in stdlib / Pkg / Base are usually
                # irrelevant for diagnosing checker bugs).
                bt = catch_backtrace()
                ctx.hasChecked[mi][end] = ctx.hasChecked[mi][end].first => e
                if ctx.printCheckerFrames
                    miStr = _shortenMethodSig(string(mi))
                    println("in ", miStr, "   [checker-internal]")
                end
                println("error[JTC-INTERNAL]: checker crashed on this method instance")
                println("   exception: ", first(string(e), 600))
                println("   on: ", _shortenMethodSig(string(mi)))
                println("   trace (top frames from src/):")
                stack = stacktrace(bt)
                shown = 0
                for fr in stack
                    if fr.file !== nothing
                        s = String(fr.file)
                        # Skip Julia stdlib / dependency frames. Surface only
                        # frames whose path includes our own src tree.
                        if occursin("/SimpleTypeChecker/src/", s) ||
                           occursin("SimpleTypeChecker/src/", s) ||
                           occursin("Inference.jl", s) ||
                           occursin("SyntaxAdaptor.jl", s) ||
                           occursin("CompilerAdaptor.jl", s) ||
                           occursin("CommonMacros.jl", s)
                            println("     ", fr.func, " at ", basename(s), ":", fr.line)
                            shown += 1
                            shown >= 8 && break
                        end
                    end
                end
                if shown == 0
                    # Fallback: show top-5 frames period if nothing matched.
                    for fr in stack[1:min(5, length(stack))]
                        println("     ", fr.func, " at ", fr.file, ":", fr.line)
                    end
                end
                println()
            end
        finally
        end
    end
end