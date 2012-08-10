#r "bin\Debug\cli.dll"

open System.IO
open Roma.Cli.ModuleLoading

let names = [
    "mscorlib.dll"
    "System.dll"
    @"C:\Program Files\Microsoft F#\v4.0\Fsc.exe"
]

for name in names do
    printfn "%s" name
    let path =
        if name.Contains(":") then
            name
        else
            Path.Combine(@"C:\Windows\Microsoft.NET\Framework\v4.0.30319", name)
    let m = loadModule path
    ()
