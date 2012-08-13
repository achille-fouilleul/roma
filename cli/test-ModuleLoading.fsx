#r "bin\Debug\cli.dll"

open System
open System.IO
open System.Text
open Roma.Cli
open Roma.Cli.ModuleLoading

// let rootPath = @"C:\Program Files\Microsoft F#\v4.0\Fsc.exe"
let rootPath = @"C:\Program Files\Microsoft F#\v4.0\Fsi.exe"

printfn "%s" rootPath
let rootModule = loadModule rootPath

let asmRefs = System.Collections.Generic.List<_>()
let asms = System.Collections.Generic.List<_>()

asms.Add(rootModule)

for asmRef in rootModule.assemblyRefs do
    asmRefs.Add(asmRef)

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
    let path = Path.Combine(@"C:\Windows\Microsoft.NET\Framework\v4.0.30319", asmRef.name + ".dll")
    if File.Exists(path) then
        path
    else
        let dir1 = Path.Combine(@"C:\Windows\Microsoft.NET\assembly\GAC_MSIL", asmRef.name)
        let v0, v1, v2, v3 = asmRef.version
        let dir2 = Path.Combine(dir1, sprintf "v%d.%d_%d.%d.%d.%d__%s" v0 v1 v0 v1 v2 v3 (toStr asmRef.publicKeyOrToken))
        Path.Combine(dir2, asmRef.name + ".dll")

let load asmRef =
    if not(isLoaded asmRef) then
        let path = resolve asmRef
        printfn "%s" path
        let m = loadModule path
        asms.Add(m)
        asmRefs.AddRange(m.assemblyRefs)

while asmRefs.Count <> 0 do
    let asmRef = asmRefs.[0]
    load asmRef
    asmRefs.RemoveAt(0)

