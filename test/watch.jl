import Dates
if !isdefined(Main, :modtime)
    modtime = nothing
    global function watchDirectory(path::String)
        maxtime::Float64 = 0.0 
        for (root, dirs, files) in walkdir(path)
            for file in files
                maxtime = max(mtime(joinpath(root, file)), maxtime)
            end
        end
        return Dates.unix2datetime(maxtime)
    end
end
newtime = watchDirectory(joinpath(@__DIR__, "../src/adaptor"))
if modtime isa Nothing || modtime != newtime
    println("Update code $modtime $newtime")
    modtime = newtime
    include("eval.jl")
end
