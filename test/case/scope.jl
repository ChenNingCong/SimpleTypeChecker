module ScopeTest
function f()
    local x
    local y::Int
    local z::Int = 1

    y = 1
    x = 1
    println(x, y, z)
end

function inorder_test()
    x = 1
    local x
    return 
end

function inner_inorder_test()
    for i in 1:10
        t = 1
    end
    local t
    print(t)
end

function double_declare_test()
    local x
    local x
end

end