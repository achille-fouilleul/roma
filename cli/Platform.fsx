module Roma.Platform

open System

let selectPath winPath unixPath : string =
    match Environment.OSVersion.Platform with
    | PlatformID.Win32NT -> winPath
    | PlatformID.Unix -> unixPath
    | _ -> raise(NotImplementedException())

let cliRoot = selectPath @"C:\Windows\Microsoft.NET\Framework\v4.0.30319" "/usr/lib/mono/2.0"
