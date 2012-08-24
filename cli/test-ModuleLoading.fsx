#r "bin/Debug/cli.dll"
#load "Platform.fsx"

open System
open System.IO
open System.Text
open Roma.Cli
open Roma.Cli.ModuleLoading
open Roma.Platform

let fsDir = selectPath @"C:\Program Files\Microsoft F#\v4.0" "/usr/lib/fsharp"

let rootPath = Path.Combine(fsDir, "fsc.exe")
// let rootPath = Path.Combine(fsDir, "fsi.exe")

let asmRefs = System.Collections.Generic.List<_>()
let asms = System.Collections.Generic.List<_>()

let loadFromPath path =
    printfn "%s" path
    let m = loadModule path
    asms.Add(m)
    asmRefs.AddRange(m.assemblyRefs)
    m

let rootModule = loadFromPath rootPath

let isLoaded(asmRef : AssemblyRef) =
    asms |> Seq.exists(
        fun asm ->
            let name = Option.get asm.assembly
            name.name = asmRef.name && name.version = asmRef.version)

let toStr(xs : byte[]) =
    let buf = StringBuilder()
    for x in xs do
        buf.AppendFormat("{0:x2}", int x) |> ignore
    buf.ToString()

let resolve(asmRef : AssemblyRef) =
    let name = asmRef.name + ".dll"
    let cliPath = Path.Combine(cliRoot, name)
    let fsPath = Path.Combine(fsDir, name)
    match name with
    | _ when File.Exists(cliPath) -> cliPath
    | _ when File.Exists(fsPath) -> fsPath
    | _ ->
        let dir1 = Path.Combine(@"C:\Windows\Microsoft.NET\assembly\GAC_MSIL", asmRef.name)
        let v0, v1, v2, v3 = asmRef.version
        let dir2 = Path.Combine(dir1, sprintf "v%d.%d_%d.%d.%d.%d__%s" v0 v1 v0 v1 v2 v3 (toStr asmRef.publicKeyOrToken))
        Path.Combine(dir2, name)

while asmRefs.Count <> 0 do
    let asmRef = asmRefs.[0]
    if not(isLoaded asmRef || asmRef.name = "ISymWrapper") then
        let path = resolve asmRef
        loadFromPath path |> ignore
    asmRefs.RemoveAt(0)

