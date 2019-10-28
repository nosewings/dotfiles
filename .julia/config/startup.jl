atreplinit() do repl
    try
        @eval using Revise
        @async Revise.wait_steal_repl_backend()
    catch
        println("Unable to load Revise.")
    end
end

try
    using OhMyREPL
catch
    println("Unable to load OhMyREPL.")
end

using Pkg
Pkg.activate(".")

try
    include(joinpath(pwd(), "startup.jl"))
    println("Loaded local startup.jl.")
catch
end
