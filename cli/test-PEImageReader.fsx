#r "bin\Debug\cli.dll"

open System
open System.IO
open Roma.Cli

let hex(data : byte[]) =
    let buffer = System.Text.StringBuilder()
    for x in data do
        buffer.AppendFormat("{0:x2}", x) |> ignore
    buffer.ToString()

let names = [ "mscorlib.dll"; "System.dll" ]
for name in names do
    printfn "%s" name
    let path = Path.Combine(@"C:\Windows\Microsoft.NET\Framework\v4.0.30319", name)
    let img = PEImageReader(path)
    printfn " StrongNameHash: %s" (hex img.StrongNameHash)

    printfn " CLI header:"
    match img.CliHeader with
    | None -> printfn " (none)"
    | Some cliHeader ->
        if not(cliHeader.metaData.IsZero) then
            let metadata = img.Read(cliHeader.metaData)
            printfn "  MetaData size: %d" metadata.Length
        if not(cliHeader.strongNameSig.IsZero) then
            printfn "  StrongNameSig: %s" (hex(img.Read(cliHeader.strongNameSig)))

    printfn ""