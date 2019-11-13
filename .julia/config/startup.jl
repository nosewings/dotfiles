using Pkg

atreplinit() do repl
    try
        @eval using Revise
        @async Revise.wait_steal_repl_backend()
    catch
        warn("Revise not installed")
    end
end

try
    @eval using OhMyREPL
catch
    warn("OhMyREPL not installed")
end
