function f()
    x = 1
    if x > 1
        println(x)
    else
        println(x + 1.0)
    end
    return x
end

function f1()
    x = 1
    if x > 1
        x = 2
    else
        x = 3
    end
    return x
end

function f2()
    x = 1
    for i in 1:10
        println(i)
        x = x + i
    end
    return x
end

function f3()
    x = 1
    for i in 1:10
        println(i)
        x = 3
    end
    return x
end

function f4()
    x = 1
    for i in 1:10
        println(i)
        x = 1
    end 
    return x
end

function f5()
    x = 1
    for i in 1:10
        println(i)
        if i > 5
            x = 2
        end
    end 
    return x
end