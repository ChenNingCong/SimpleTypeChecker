try
    Core.eval(Main,:(import PianoFingering))
catch e
end
import PianoFingering
import SimpleTypeChecker
const path = dirname(abspath(pathof(PianoFingering)))
files = String[]
for i in collect(walkdir(path))[1][3]
    println(joinpath(path, i))
    push!(files, i)
end
ctx = SimpleTypeChecker.Inference.GlobalContext()
for i in files
    full = joinpath(path, i)
    SimpleTypeChecker.API.addFile!(ctx, PianoFingering, full)
end
SimpleTypeChecker.API.runCheck!(ctx;checkSyntax=true)