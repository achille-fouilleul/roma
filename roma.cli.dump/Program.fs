module Roma.Cli.Dump.Program

open Roma.Cli

[<EntryPoint>]
let main (args : string[]) =
    let m = ModuleLoading.loadModule args.[0]
    RawTree.dump m
    0
