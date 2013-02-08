module Roma.Compiler.Asm

open System

type AsmLine =
    | Label of string
    | Comment of string
    | U8 of uint8
    | S8 of int8
    | U16 of uint16
    | S16 of int16
    | U32 of uint32
    | S32 of int32
    | U64 of uint64
    | S64 of int64
    | Uleb128 of UInt128
    | Sleb128 of Int128
    | Ref32 of string
    | Ref64 of string
    | String of string
    | Expr32 of string
    | Expr64 of string

    member this.ByteSize =
        match this with
        | Label _ | Comment _ -> 0
        | U8 _ | S8 _ -> 1
        | U16 _ | S16 _ -> 2
        | U32 _ | S32 _ | Ref32 _ | Expr32 _ -> 4
        | U64 _ | S64 _ | Ref64 _ | Expr64 _ -> 8
        | Uleb128 x -> Leb128.length x
        | Sleb128 x -> Leb128.length x
        | String s -> System.Text.Encoding.UTF8.GetByteCount(s) + 1

let private quoteString (s : string) =
    let buffer = System.Text.StringBuilder()
    if Seq.forall (fun c -> c <> '"' && (c >= '\x20') && (c < '\x7f')) s then
        "\"" + s + "\""
    else
        let bytes = System.Text.Encoding.UTF8.GetBytes(s)
        let inline bprintf s = Printf.bprintf buffer s
        bprintf "\""
        for x in bytes do
            match char x with
            | '\t' -> bprintf "\\t"
            | '\n' -> bprintf "\\n"
            | '\r' -> bprintf "\\r"
            | '"' ->  bprintf "\\\""
            | c when c >= '\x20' && c < '\x7f' -> bprintf "%c" c
            | _ -> bprintf "\\x%02x" x
        bprintf "\""
        buffer.ToString()

let private ii = System.Globalization.NumberFormatInfo.InvariantInfo

let toStrings lines =
    let m = System.Collections.Generic.Dictionary<_, _>()
    let mutable offset = 0
    for line in lines do
        match line with
        | Label label -> m.Add(label, offset)
        | _ -> offset <- Checked.(+) offset line.ByteSize
    seq {
        for line in lines ->
            match line with
            | Label label -> sprintf "%s:" label
            | Comment comment -> sprintf "// %s" comment
            | U8 x -> sprintf "\t.byte 0x%x" x
            | S8 x -> sprintf "\t.byte %d" x
            | U16 x -> sprintf "\t.value 0x%x" x
            | S16 x -> sprintf "\t.value %d" x
            | U32 x -> sprintf "\t.long 0x%x" x
            | S32 x -> sprintf "\t.long %d" x
            | U64 x -> sprintf "\t.quad 0x%x" x
            | S64 x -> sprintf "\t.quad %d" x
            | Uleb128 x ->
                sprintf "\t.uleb128 0x%s" (x.ToString("x", ii))
            | Sleb128 x ->
                if x >= Int128.Zero then
                    sprintf "\t.sleb128 0x%s" (x.ToString("x", ii))
                else
                    sprintf "\t.sleb128 %s" (x.ToString(ii))
            | Ref32 label -> sprintf "\t.long %d" (Checked.uint32(m.[label]))
            | Ref64 label -> sprintf "\t.quad %d" (Checked.uint64(m.[label]))
            | String s -> sprintf "\t.string %s" (quoteString s)
            | Expr32 s -> sprintf "\t.long %s" s
            | Expr64 s -> sprintf "\t.quad %s" s
    }

let mutable private labelIndex = 0

let createLabel() =
    let index = System.Threading.Interlocked.Increment(&labelIndex)
    sprintf ".L%d" index
