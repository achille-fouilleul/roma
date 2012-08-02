#r "bin\Debug\cli.dll"

open System
open System.IO
open Roma.Cli

let hex(data : byte[]) =
    let buffer = System.Text.StringBuilder(2 * data.Length)
    for x in data do
        buffer.AppendFormat("{0:x2}", x) |> ignore
    buffer.ToString()

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
    let img = PEImageReader(path)
    printfn " StrongNameHash: %s" (hex img.StrongNameHash)

    printfn " CLI header:"
    match img.CliHeader with
    | None -> printfn " (none)"
    | Some cliHeader ->
        printfn "  EntryPointToken: 0x%x" cliHeader.entryPointToken
        if not(cliHeader.strongNameSig.IsZero) then
            printfn "  StrongNameSig: %s" (hex(img.Read(cliHeader.strongNameSig)))
        if not(cliHeader.metaData.IsZero) then
            let md = MetadataReader(img)
            let il = CILReader(img)
            let row = md.ModuleTable.[0]
            printfn "  Name: %s" row.Name
            printfn "  Id: %A" row.Mvid
            for row in md.MethodDefTable do
                if row.Rva <> 0u then
                    printfn "  %s %A" row.Name (il.ReadMethodBody(row.Rva))

    printfn ""