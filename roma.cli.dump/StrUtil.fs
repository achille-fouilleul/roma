module internal Internal.StrUtil

open System
open System.Globalization

let ic = CultureInfo.InvariantCulture

let intToStr (n : int) =
    n.ToString(ic)

type Writer() =
    let mutable level = 0
    let output = Console.Out
    let err = Console.Error

    member this.Enter() =
        level <- level + 1

    member this.Leave() =
        assert(level > 0)
        level <- level - 1

    member this.Print() =
        output.WriteLine()

    member this.Print(s : string) = 
        output.WriteLine((String.replicate level " ") + s)

    member this.Warn(s : string) =
        err.WriteLine(s)

// C#-specific

let private escapeChar c =
    match c with
    | '\u0000' -> "\\0" 
    | '\t' -> "\\t"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | c when c >= '\u0020' && c < '\u007f' -> string c
    | c -> "\\x" + (uint16 c).ToString("x04", ic)

let charToCSharpStr c =
    let c' = if c = '\'' then "\\'" else escapeChar c
    "'" + c' + "'"

let strToCSharpStr s =
    let s' =
        if String.forall (fun c -> c >= '\u0020' && c < '\u007f' && c <> '"') s then
            s
        else
            String.collect (fun c -> if c = '"' then "\\\"" else escapeChar c) s
    "\"" + s' + "\""
