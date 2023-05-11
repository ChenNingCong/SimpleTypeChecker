# This file converts a JuAST to a JuExpr and establishes proper mapping between them
import ..SyntaxAdaptor.parseJuAST
import ..SyntaxDefinition.formatLocation
import ..Utility.@nocheck
import ..Utility.Maybe
import ..SyntaxDefinition.JuASTVal

function parseJuExpr(s::String, filename::String)
    ast = parseJuAST(s, filename)
    return constructJuExpr(ast)
end

# TODO
# whether we should use a global mapping for the whole module
# or individual mapping for each function?
# using individual mapping requires an additional indirect in JuAST lookup
# Given a JuAST, we lookup the parent until we see a function/module
# assume it's a toplevel one, we then firstly lookup the function to get the source mapping
# then we lookup the given JuAST in this local mapping
mutable struct ConstructJuExprResult
    ast::JuAST
    # stack of current nested module
    const modules::Vector{Symbol}
    # current mapping for one method
    sourceMapping::SourceMapping
    # whether we encounter error in this walk
    hasError::Bool
    const errio::Any
    const map::Dict{JuExpr, SourceMapping}
    const revMap::Dict{JuAST, JuExpr}
    const modMap::Dict{JuExpr, Vector{Symbol}}
end

@nocheck function makeConstructJuExprResult(ast::JuAST)
    # stdout is a global...
    ConstructJuExprResult(ast, Symbol[], SourceMapping(), false, stdout, Dict{FunDef, SourceMapping}(), Dict{JuAST, FunDef}(), Dict{FunDef, Vector{Symbol}}())
end

@nocheck function forkConstructJuExprResult(r::ConstructJuExprResult)
    ConstructJuExprResult(r.ast, copy(r.modules), SourceMapping(), false, r.errio, r.map, r.revMap, r.modMap)
end

struct InvalidSyntaxError
    msg::String
end

function constructJuExprIfHelper!(result::ConstructJuExprResult,
                                  rel::Vector{Pair{JuExpr, JuExpr}}, 
                                  ast::JuAST)::Maybe{JuExpr}
    
    cond = constructJuExpr!(result, ast.args[1])
    body = constructJuExpr!(result, ast.args[2])
    push!(rel, cond => body)
    if length(ast.args) == 3
        if ast.args[3].head == :elseif
            ex = constructJuExprIfHelper!(result, rel, ast.args[3])
        else
            # else branch
            ex = Just(constructJuExpr!(result, ast.args[3]))
        end
    elseif length(ast.args) == 2
        ex = None(JuExpr)
    else
        ex = None(JuExpr)
        error()
    end
    return ex
end

function addSourceMap!(result::ConstructJuExprResult, ast::JuAST, ex::JuExpr)::Nothing
    map = result.sourceMapping
    if haskey(map.ast2exMapping, ast)
        error("Multiple mapping from a JuAST to a JuExpr")
    end
    map.ast2exMapping[ast] = DerivedExpr(NoDerivedExpr(ex))
    map.ex2astMapping[ex] = ast
    return nothing
end

function addSourceMapDerived!(result::ConstructJuExprResult, ast::JuAST, dast::DerivedExpr)::Nothing
    map = result.sourceMapping
    if haskey(map.ast2exMapping, ast)
        error("Multiple mapping from a JuAST to a JuExpr")
    end
    map.ast2exMapping[ast] = dast
    return nothing
end

function addSourceMapForIf!(result::ConstructJuExprResult,
                            parent::JuExpr, 
                            ast::JuAST)::Nothing
    if ast.head == :if
        addSourceMap!(result, ast, parent)
    elseif ast.head == :elseif
        addSourceMapDerived!(result, ast, DerivedExpr(DerivedExprElseIf(parent)))
    else
        return
    end
    if length(ast.args) == 3
        addSourceMapForIf!(result, parent, ast.args[3])
    end
    return
end

function constructJuExprIf!(result::ConstructJuExprResult, ast::JuAST)::JuExpr
    rel = Vector{Pair{JuExpr, JuExpr}}()
    else_b = constructJuExprIfHelper!(result, rel, ast)
    parent = JuExpr(IfStmt(rel, else_b), ast)
    addSourceMapForIf!(result, parent, ast)
    return parent
end

function prettyInvalidSyntaxError(e::InvalidSyntaxError, ast::JuAST)    
    println('\n', formatLocation(ast.loc))
    println("  ", e.msg)
    println('\n')
end

@nocheck function isaJuASTVal(val::JuASTVal, typ)::Bool
    if !val.isconst
        error("Not a valid literal value")
    end
    return val.val isa typ
end

@nocheck function castJuASTVal(typ::Type{T}, val::JuASTVal)::T where T
    if val.isconst
        valval = val.val
        if valval isa T
            return valval
        else
            error("Failed to cast value : type mismatched")
        end
    else
        error("Failed to cast value : not a valid constant value")
    end
end

function cast2Symbol(val::JuASTVal)::Symbol
    if !val.isconst
        error("Failed to cast value : not a valid constant value")
    end
    valval = val.val
    if valval isa Symbol
        return valval
    else
        error("Failed to cast value : not a symbol")
    end
end

@nocheck function JuASTVal2Literal(val::JuASTVal)::Literal
    Literal(makeConstVal(val.val))
end

function constructLiteral!(result::ConstructJuExprResult, ast::JuAST)::JuExpr
    lval = ast.val
    if isaJuASTVal(lval, Symbol)
        error("Internal error : symbol should be a identifier")
    else
         # an abstract function calling here
        if lval.isconst
            ex = JuExpr(JuASTVal2Literal(lval), ast)
        else
            error("not a constant here")
        end
    end
    return ex
end

function reportError(result::ConstructJuExprResult, err::InvalidSyntaxError, ast::JuAST)
    result.hasError = true
    prettyInvalidSyntaxError(err, ast)
    return nothing
end

function constructJuExprArgs!(result::ConstructJuExprResult, f::JuAST)::JuExpr
    fex = constructJuExpr!(result, f.args[1])
    args = f.args
    fargs = JuExpr[]
    kwargs = Pair{Symbol, JuExpr}[]
    ex = JuExpr()
    if f.head == :curly
        ex = JuExpr(CurlyCall(fex, fargs), f)
    elseif f.head == :call
        ex = JuExpr(FunCall(fex, fargs, kwargs), f)
    elseif f.head == :ref
        ex = JuExpr(ArrayRef(fex, fargs), f)
    else
        error("Internal error : invalid argument to constructJuExprArgs!")
    end

    for i in 2:length(args)
        ast = args[i]
        if ast.head != :(=) && ast.head != :parameters && ast.head != :kw
            push!(fargs, constructJuExpr!(result, ast))
        end
    end
    
    for i in 2:length(args)
        ast = args[i]
        if ast.head == :(=) || ast.head == :(kw)
            err = InvalidSyntaxError("Optional argument appears in function call")
            reportError(result, err, ast)
        end
    end

    for i in 2:length(args)
        ast = args[i]
        if ast.head == :parameters
            hasError = false
            for kw in ast.args
                if kw.head == :(=)
                    # Expr(:kw, ...) 
                    name = kw.args[1]
                    if name.head == :identifier
                        sym = cast2Symbol(name.val)
                        push!(kwargs, sym => constructJuExpr!(result, kw.args[2]))
                        dkw = DerivedExprKwArg(ex, sym)
                        addSourceMapDerived!(result, kw, DerivedExpr(dkw))
                        addSourceMapDerived!(result, name, DerivedExpr(DerivedExprKwArgLHS(dkw)))
                    else
                        err = InvalidSyntaxError("Invalid keyword argument definition")
                        reportError(result, err, name)
                    end
                elseif kw.head == :identifier
                    # keyword syntax
                    # f(x;x) =>f(x;x = x)
                    sym = cast2Symbol(kw.val)
                    rhs = JuExpr(Var(sym), kw)
                    push!(kwargs, sym => rhs)
                    addSourceMapDerived!(result, kw, DerivedExpr(DerivedExprImplicitKwArg(ex, sym)))
                else
                    err = InvalidSyntaxError("Invalid keyword argument definition")
                    reportError(result, err, ast)
                end
            end
            addSourceMapDerived!(result, ast, DerivedExpr(DerivedExprParameter(ex)))
        end
    end
    if (f.head != :call) && (length(kwargs) > 0)
        err = InvalidSyntaxError("Invalid syntax for non-function call") 
        reportError(result, err, f)
    end
    addSourceMap!(result, f, ex)
    return ex
end

function constructJuExprAssign!(result::ConstructJuExprResult, ast::JuAST)::JuExpr
    alhs = ast.args[1]
    ex = JuExpr()
    if alhs.head == :(.)
        dlhs = constructJuExpr!(result, alhs.args[1])
        p = alhs.args[2]
        if p.head == :quote
            x_ = p.args[1]
            if x_.head == :identifier
                sym = cast2Symbol(x_.val)
                rhs = constructJuExpr!(result, ast.args[2])
                ex = JuExpr(SetProperty(dlhs, sym, rhs), ast)
                # TODO : maybe used different derived here?
                addSourceMapDerived!(result, p, DerivedExpr(DerivedExprField(ex)))
                addSourceMapDerived!(result, x_, DerivedExpr(DerivedExprField(ex)))
                addSourceMapDerived!(result, alhs, DerivedExpr(DerivedExprField(ex)))
                addSourceMap!(result, ast, ex)
                return ex
            end
        end
        err = InvalidSyntaxError("Not a valid setproperty AST")
        reportError(result, err, ast)
        return ex
    elseif alhs.head == :ref
        arr = constructJuExpr!(result, alhs.args[1])
        args = Vector{JuExpr}(undef, length(alhs.args)-1)
        for i in 1:length(args)
            args[i] = constructJuExpr!(result, alhs.args[i+1])
        end
        rhs = constructJuExpr!(result, ast.args[2])
        ex = JuExpr(ArraySet(arr, args, rhs), ast)
        addSourceMapDerived!(result, alhs, DerivedExpr(DerivedExprArraySet(ex)))
        addSourceMap!(result, ast, ex)
        return ex
    elseif alhs.head == :(::)
        # add derived here???
        rhs = constructJuExpr!(result, ast.args[2])
        var = alhs.args[1]
        if var.head == :identifier
            sym = cast2Symbol(var.val)
            ttyp = constructJuExpr!(result, alhs.args[2])
            ex = JuExpr(DeclarationList(JuExpr[JuExpr(Declaration(sym, Just(ttyp), Just(rhs)), ast)]), ast)
            addSourceMap!(result, ast, ex)
            return ex
        end
        err = InvalidSyntaxError("Not a valid variable type definition, lhs can only be symbol")
        reportError(result, err, ast)
        return ex
    else
        rhs = constructJuExpr!(result, ast.args[2])
        if alhs.head == :identifier
            sym = cast2Symbol(alhs.val)
            k = Assign(sym, rhs)
            ex = JuExpr(k, ast)
            addSourceMapDerived!(result, alhs, DerivedExpr(DerivedExprAssignLHS(ex)))
            addSourceMap!(result, ast, ex)
            return ex
        elseif alhs.head == :tuple
            syms = Vector{Symbol}(undef, length(alhs.args))
            ex = JuExpr(TupleAssign(syms, rhs), ast)
            for i in eachindex(alhs.args)
                symast = alhs.args[i]
                if !(symast.head == :identifier)
                    err = InvalidSyntaxError("Not a valid tuple destruct")
                    reportError(result, err, ast)
                    return ex
                end
                sym = cast2Symbol(symast.val)
                syms[i] = sym
                addSourceMapDerived!(result, symast, DerivedExpr(DerivedExprAssignLHS(ex)))
            end
            addSourceMap!(result, ast, ex)
            return ex
        end
        err = InvalidSyntaxError("Not a valid assignment AST")
        reportError(result, err, ast)
        return ex
    end
end

function constructJuExpr(ast::JuAST)
    if ast.head == :toplevel
        result = makeConstructJuExprResult(ast)
        for i in ast.args
            if i.head == :function
                constructJuExprFunDef!(result, i)
            elseif i.head == :module
                constructJuExpr!(result, i)
            end
        end
        return result
    else
        error("Input should be a toplevel expression")
    end
end

function constructJuExpr!(result::ConstructJuExprResult, ast::JuAST)::JuExpr
    # Something tricky here, there are two types of character here
    # one is K'char', another is 'Char'
    ex = JuExpr()
    err = InvalidSyntaxError("")
    if ast.head == :char
        charval = ast.args[1].val
        if isaJuASTVal(charval, Char)
            ex = JuExpr(Literal(makeConstVal(castJuASTVal(Char, charval))), ast)
            addSourceMap!(result, ast, ex)
            addSourceMap!(result, ast.args[1], ex)
            return ex
        else
            err = InvalidSyntaxError("Not a valid character literal")
            reportError(result, err, ast)
            return ex
        end
    elseif ast.head == :literal
        lval = ast.val
        ex = constructLiteral!(result, ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :quote
        if length(ast.args) == 1 && ast.args[1].head == :identifier
            ex = JuExpr(Literal(makeConstVal(cast2Symbol(ast.args[1].val))), ast)
            addSourceMap!(result, ast, ex)
        else
           err = InvalidSyntaxError("Quoted expression is disallowed")
           reportError(result, err, ast)
        end
    elseif ast.head == :curly
        ex = constructJuExprArgs!(result, ast)
        return ex
    elseif ast.head == :call
        ex = constructJuExprArgs!(result, ast)
        return ex
    elseif ast.head == :module
        nameast = ast.args[2]
        if ast.args[3].head != :block
            err = InvalidSyntaxError("Invalid module expression")
            reportError(result, err, ast)
            return ex
        end
        if nameast.head == :identifier
            sym = cast2Symbol(nameast.val)
            stmts = Vector{JuExpr}()
            modex = JuExpr(ModDef(sym, stmts), ast)
            addSourceMapDerived!(result, ast.args[1], DerivedExpr(DerivedExprModuleName(modex)))
            addSourceMapDerived!(result, nameast, DerivedExpr(DerivedExprModuleName(modex)))
            addSourceMap!(result, ast, modex)
            # entering module
            push!(result.modules, sym)
            for i in ast.args[3].args
                if i.head == :function
                    ex = constructJuExprFunDef!(result, i)
                elseif i.head == :module
                    ex = constructJuExpr!(result, i)
                else
                    # ignore all other toplevel constructions
                    ex = JuExpr(Literal(makeConstVal(nothing)), i)
                end
                # ignore all invalid expressions here
                if isValidJuExpr(ex)
                    push!(stmts, ex)
                end
            end
            # leaving module
            pop!(result.modules)
            return modex
        else
            err = InvalidSyntaxError("Invalid module definition")
            reportError(result, err, nameast)
        end
        return ex
    elseif ast.head == :macrocall
        # TODO : fix me here
        # @warn "$(formatLocation(ast.span)) : ignore macrocall $(ast.children[1]) here"
        ex = JuExpr(Literal(makeConstVal(nothing)), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :(<:) || ast.head == :(&&) || ast.head == :(||)
        # subtyping lowered as function call
        # this is actually incorrect, because <: can also appear in type application
        # like Array{<:Int, 1}, we need to check this case more carefully, currently we ignore compilicated type definition
        # TODO : please check this 
        f = JuExpr(Var(ast.head), ast)
        # TODO : we map the var to the whole ast, which is incorrect
        # what to do here ???
        if length(ast.args) != 2
            err = InvalidSyntaxError("<: in typing context is unsupported")
            reportError(result, err, ast)
            return ex
        end
        stmts = Vector{JuExpr}(undef, length(ast.args))
        for i in 1:length(ast.args)
            stmts[i] = constructJuExpr!(result, ast.args[i])
        end
        ex = JuExpr(FunCall(f, stmts, Pair{Symbol, JuExpr}[]), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :(::)
        lhs = constructJuExpr!(result, ast.args[1])
        rhs = constructJuExpr!(result, ast.args[2])
        ex = JuExpr(TypedAssert(lhs, rhs), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :toplevel
        err = InvalidSyntaxError("Misplaced toplevel block")
        reportError(result, err, ast)
        return ex
    elseif ast.head == :ref
        ex = constructJuExprArgs!(result, ast)
        return ex
    elseif ast.head == :(.)
        lhs = constructJuExpr!(result, ast.args[1])
        rhs_ = ast.args[2]
        if rhs_.head == :quote
            x_ = rhs_.args[1]
            if x_.head == :identifier
                sym = cast2Symbol(x_.val)
                ex = JuExpr(GetProperty(lhs, sym), ast)
                addSourceMapDerived!(result, rhs_, DerivedExpr(DerivedExprField(ex)))
                addSourceMapDerived!(result, x_, DerivedExpr(DerivedExprField(ex)))
                addSourceMap!(result, ast, ex)
                return ex
            end
        else
            err = InvalidSyntaxError("Not a valid getproperty AST")
            reportError(result, err, ast)
            return ex
        end
    elseif ast.head == :if
        return constructJuExprIf!(result, ast)
    elseif ast.head == :block
        stmts = Vector{JuExpr}(undef, length(ast.args))
        for i in 1:length(ast.args)
            stmts[i] = constructJuExpr!(result, ast.args[i])
        end
        ex = JuExpr(Block(stmts), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :return
        if length(ast.args) == 1
            ex = JuExpr(Return(Just(constructJuExpr!(result, ast.args[1]))), ast)
        else
            ex = JuExpr(Return(None(JuExpr)), ast)
        end
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :while
        cond = ast.args[1]
        body = ast.args[2]
        ex = JuExpr(WhileStmt(constructJuExpr!(result, cond), constructJuExpr!(result, body)), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :for
        cond = ast.args[1]
        body = ast.args[2]
        if cond.head != :(=)
            err = InvalidSyntaxError("Invalid for expression")
            reportError(result, err, cond)
            return ex
        end
        var = cond.args[1]
        iter = cond.args[2]
        if var.head == :identifier
            sym = cast2Symbol(var.val)
            iterex = constructJuExpr!(result, iter)
            bodyex = constructJuExpr!(result, body)
            ex = JuExpr(ForStmt(sym, iterex, bodyex), ast)
            addSourceMapDerived!(result, cond, DerivedExpr(DerivedExprForAssign(ex)))
            addSourceMapDerived!(result, var, DerivedExpr(DerivedExprForVar(ex)))
            addSourceMap!(result, ast, ex)
            return ex
        else
            err = InvalidSyntaxError("Iterate variable is not a symbol")
            reportError(result, err, var)
            return ex
        end
    elseif ast.head == :string
        # TODO : this is incorrect, we should lower it to a qualified variable instead of a local one
        fff = JuExpr(Var(:string), ast)
        # string splatting, lower to function call if not a literal
        stringval = ast.args[1].val
        strargs = Vector{JuExpr}(undef, length(ast.args)) 
        for i in 1:length(ast.args)
            strargs[i] = constructJuExpr!(result, ast.args[i])
        end
        ex = JuExpr(FunCall(fff, strargs, Pair{Symbol, JuExpr}[]), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :(=)
        ex = constructJuExprAssign!(result, ast)
        return ex 
    elseif ast.head == :function || ast.head == :(->)
        err = InvalidSyntaxError("Nested function definition is not supported")
        reportError(result, err, ast)
        return ex
    elseif ast.head in Symbol[:const, :struct, :import, :using, :export]
        ex = JuExpr(Literal(makeConstVal(nothing)), ast)
        return ex
    elseif ast.head == :vect
        err = InvalidSyntaxError("Don't use untyped array construction. Use Type[...] instead of [...]")
        reportError(result, err, ast)
        return ex
    elseif ast.head == :local
        exs = JuExpr[]
        for i in ast.args
            whole = i
            typ = None(JuExpr)
            rhs = None(JuExpr)
            # the mapping needs to establish somewhere
            # some mapping is lacked due to x::Int = 1...
            if i.head == :(=)
                rhs = Just(constructJuExpr!(result, i.args[2]))
                i = i.args[1]
            end
            if i.head == :(::)
                typ = Just(constructJuExpr!(result, i.args[2]))
                i = i.args[1]
            end
            if i.head == :identifier
                sym = cast2Symbol(i.val)
                ex = JuExpr(Declaration(sym, typ, rhs), whole)
                addSourceMapDerived!(result, i, DerivedExpr(DerivedExprAssignLHS(ex)))
                push!(exs, ex)
            else
                err = InvalidSyntaxError("local only support that lhs isa symbol")
                reportError(result, err, ast)
            end
        end
        ex = JuExpr(DeclarationList(exs), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :continue
        ex = JuExpr(ContinueStmt(), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :break
        ex = JuExpr(BreakStmt(), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :tuple
        params = Vector{JuExpr}(undef, length(ast.args))
        for i in eachindex(ast.args)
            params[i] = constructJuExpr!(result, ast.args[i])
        end
        ex = JuExpr(TupleLiteral(params), ast)
        addSourceMap!(result, ast, ex)
        return ex
    elseif ast.head == :identifier
        ex = JuExpr(Var(cast2Symbol(ast.val)), ast)
        addSourceMap!(result, ast, ex)
        return ex
    else
        x = string(ast.head)
        if length(x) >= 1 && x[end] == '='
            x_ = ast.args[1]
            if x_.head == :identifier
                sym = cast2Symbol(x_.val)
                ex = JuExpr(UpdateAssign(Symbol(x[1:end-1]), sym, constructJuExpr!(result, ast.args[2])), ast)
                addSourceMapDerived!(result, x_, DerivedExpr(DerivedExprAssignLHS(ex)))
                addSourceMap!(result, ast, ex)
                return ex
            else
                err = InvalidSyntaxError("lhs is not a symbol")
                reportError(result, err, ast)
            end
            return ex
        end
        err = InvalidSyntaxError("Unsupported AST kind")
        reportError(result, err, ast)
        return ex
    end
    return ex
end

function constructJuExprFunDef!(result::ConstructJuExprResult, ast::JuAST)::JuExpr
    ex = JuExpr()
    err = InvalidSyntaxError("")
    if length(ast.args) < 2
        err = InvalidSyntaxError("Unsupported empty function body")
        reportError(result, err, ast)
        return ex
    end
    oldresult = result
    # we open a result here, so if we encounter error in the function, we can safely discard the function body
    result = forkConstructJuExprResult(result)

    params = Symbol[]
    body = constructJuExpr!(result, ast.args[2])
    args = Pair{Symbol, Maybe{JuExpr}}[]
    optargs = Pair{Symbol, Pair{Maybe{JuExpr}, JuExpr}}[]
    kwargs = Pair{Symbol, Pair{Maybe{JuExpr}, JuExpr}}[]
    hasrt = false
    rt = None(JuExpr)
    # circumvent recursive dependency here

    fast = ast.args[1]
    if fast.head == :where
        fast = fast.args[1]
    end
    if fast.head == :(::)
        rt = Just(constructJuExpr!(result, fast.args[2]))
        hasrt = true
    end
    fnameSym = Base.RefValue{Symbol}(:none)
    exf = FunDef(fnameSym, args, optargs, kwargs, params, rt, body)
    ex = JuExpr(exf, ast)
    if hasrt
        addSourceMapDerived!(result, fast, DerivedExpr(DerivedExprReturnType(ex)))
    end

    fast = ast.args[1]
    if fast.head == :where
        # TODO : support subtyping contraint here...
        for i in 1:length(fast.args)-1
            child = fast.args[i + 1]
            if child.head == :identifier
                sym = cast2Symbol(child.val)
                addSourceMapDerived!(result, child, DerivedExpr(DerivedExprFunParam(ex, sym)))
                push!(params, sym)
            else
                err = InvalidSyntaxError("Unsupported paramter variable")
                reportError(result, err, child)
            end
        end
        addSourceMapDerived!(result, fast, DerivedExpr(DerivedExprFunParamList(ex)))
        fast = fast.args[1]
    end

    if fast.head == :(::)
        fast = fast.args[1]
    end
    
    if fast.head == :call
        fname = fast.args[1]
        if fname.head == :identifier
            sym = cast2Symbol(fname.val)
            exf.f[] = sym
            addSourceMapDerived!(result, fname, DerivedExpr(DerivedExprFunName(ex)))
            for i in 2:length(fast.args)
                param = fast.args[i]
                atyp = None(JuExpr)
                if param.head == :(::)
                    atyp = Just(constructJuExpr!(result, param.args[2]))
                    # TODO : fix the derivation here, it's incorrect
                    addSourceMapDerived!(result, param, DerivedExpr(DerivedExprFunDefArgPos(ex, i)))
                    param = param.args[1]
                end
                if param.head == :identifier
                    ss = cast2Symbol(param.val)
                    push!(args, ss => atyp)
                    addSourceMapDerived!(result, param, DerivedExpr(DerivedExprFunDefArg(ex, ss)))
                else
                    err = InvalidSyntaxError("Not a valid function argument type, we don't support keyword or optional arguments")
                    reportError(result, err, ast)
                end
            end
            addSourceMapDerived!(result, fast, DerivedExpr(DerivedExprFunSignature(ex)))
            addSourceMap!(result, ast, ex)
        else
            err = InvalidSyntaxError("Only support named function definition")
            reportError(result, err, ast)
        end
    else
        err = InvalidSyntaxError("Only support named function definition")
        reportError(result, err, ast)
    end
    # we need to join definition here
    if !result.hasError
        exval = ex.val
        if exval isa FunDef
            oldresult.revMap[ast] = ex
            oldresult.map[ex] = result.sourceMapping
            oldresult.modMap[ex] = copy(oldresult.modules)
        else
            error("Internal error")
        end
    end
    return ex
end
