function testRun()
    try
        error()
    catch e
        println(stacktrace())
    end
end