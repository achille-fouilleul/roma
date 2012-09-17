module Roma.Compiler.Program

open Roma.Cli.ModuleLoading

[<EntryPoint>]
let main (args : string[]) =
    System.Diagnostics.Debugger.Break()
    let mods = Array.map loadModule args
    // TODO
    0
