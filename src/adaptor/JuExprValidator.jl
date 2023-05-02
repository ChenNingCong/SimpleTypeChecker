# This file is used for debugging
# It valids whether a JuExpr and its mapping faithfully represents a JuAST with no information loss

function lookup(map::SourceMapping, ast::JuAST)
    if !haskey(map.ast2exMapping, ast)
        println(ast)
    end
    if ast.head == :quote
        return 
    end
    if ast.head == :macrocall
        return
    end
    for i in ast.args
        lookup(map, i)
    end
end

function lookupToplevel(result)
    ast = result.ast
    if ast.head == :toplevel
        for i in ast.args
            lookupFunctionOrModule(result, i)
        end
    elseif ast.head == :module
        lookupFunctionOrModule(result, ast)
    else
        error("Not a valid ast")
    end
    return
end

function lookupFunctionOrModule(result::ConstructJuExprResult, ast::JuAST)
    if ast.head == :module
        for i in ast.args
            lookupFunctionOrModule(result, i)
        end
    elseif ast.head == :function
        lookup(result.map[result.revMap[ast]], ast)
    else
    end
    return
end

