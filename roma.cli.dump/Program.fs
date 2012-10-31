module Roma.Cli.Dump.Program

open Roma.Cli

let private parseArgs (args : string[]) =
    let mutable path = null
    let mutable syntax = None
    let refs = System.Collections.Generic.List<_>()
    for arg in args do
        if arg.StartsWith("-") then
            let p = arg.IndexOf(':')
            let name, value =
                if p < 0 then
                    arg.[1 .. ], null
                else
                    arg.[1 .. p - 1], arg.[p + 1 .. ]
            match name with
            | "syntax" ->
                if Option.isSome syntax then
                    failwithf "Option 'syntax' specified multiple times."
                syntax <-
                    match value with
                    | "rawtree" -> RawTree.dump
                    | "csharp" -> CSharp.dump
                    | _ -> failwithf "Unknown syntax '%s'." value
                    |> Some
            | "r" ->
                refs.Add(value)
            | _ -> failwithf "Unknown option '%s'." name
        else
            if path <> null then
                failwith "Multiple paths specified."
            path <- arg
    if path = null then
        failwith "No path specified."
    path, (Option.fold (fun _ s -> s) RawTree.dump syntax), List.ofSeq refs

[<EntryPoint>]
let main (args : string[]) =
    try
        let path, syntax, refPaths = parseArgs args
        let m = ModuleLoading.loadModule path
        let refs =
            [
                yield (path, m)
                for refPath in refPaths ->
                    (refPath, ModuleLoading.loadModule refPath)
            ]
        syntax refs m
        0
    with e ->
        System.Console.Error.WriteLine(e.Message)
        1
