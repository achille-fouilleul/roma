module internal Internal.StrUtil

open System
open System.Globalization
open Roma.Cli

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

type CharReader(s : string) =
    let mutable i = 0

    member this.Offset = i

    member this.Peek() =
        if i < s.Length then
            Some s.[i]
        else
            None

    member this.Read() =
        match this.Peek() with
        | None -> None
        | x ->
            this.Advance()
            x

    member this.Advance() =
        i <- i + 1

let utor32 (value : uint32) =
    BitConverter.ToSingle(BitConverter.GetBytes(value), 0)

let utor64 (value : uint64) =
    BitConverter.ToDouble(BitConverter.GetBytes(value), 0)

// C#-specific

let private escapeChar c =
    match c with
    | '\u0000' -> "\\0" 
    | '\t' -> "\\t"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\\' -> "\\\\"
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

let float32ToCSharpStr (value : uint32) =
    match value with
    | 0xffc00000u -> "0.0f / 0.0f" // NaN
    | 0x7f800000u -> "1.0f / 0.0f" // +inf
    | 0xff800000u -> "-1.0f / 0.0f" // -inf
    | _ -> (utor32 value).ToString("r", ic) + "f"

let float64ToCSharpStr (value : uint64) =
    match value with
    | 0xfff8000000000000UL -> "0.0 / 0.0" // NaN
    | 0x7ff0000000000000UL -> "1.0 / 0.0" // +inf
    | 0xfff0000000000000UL -> "-1.0 / 0.0" // -inf
    | _ -> (utor64 value).ToString("r", ic)

let constantToCSharpStr constant =
    match constant with
    | ConstantBool false -> "false"
    | ConstantBool true -> "true"
    | ConstantBytearray bytes -> raise(NotImplementedException()) // TODO
    | ConstantChar value -> char value |> charToCSharpStr
    | ConstantR4 value -> float32ToCSharpStr value
    | ConstantR8 value -> float64ToCSharpStr value
    | ConstantI1 value -> value.ToString(ic)
    | ConstantU1 value -> value.ToString(ic)
    | ConstantI2 value -> value.ToString(ic)
    | ConstantU2 value -> value.ToString(ic)
    | ConstantI4 value -> value.ToString(ic)
    | ConstantU4 value -> value.ToString(ic) + "U"
    | ConstantI8 value -> value.ToString(ic) + "L"
    | ConstantU8 value -> value.ToString(ic) + "UL"
    | ConstantString s -> strToCSharpStr s
    | ConstantNullRef -> "null"

