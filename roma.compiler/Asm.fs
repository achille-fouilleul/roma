﻿module Roma.Compiler.Asm

open System

type AsmLabel =
    {
        name : string
        comment : string
    }

type AsmLine =
    | Label of AsmLabel 
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
    | Ref32 of AsmLabel
    | Ref64 of AsmLabel
    | String of string
    | Expr32 of string
    | Expr64 of string

    member this.ByteSize =
        match this with
        | Label _ -> 0
        | U8 _ | S8 _ -> 1
        | U16 _ | S16 _ -> 2
        | U32 _ | S32 _ | Ref32 _ | Expr32 _ -> 4
        | U64 _ | S64 _ | Ref64 _ | Expr64 _ -> 8
        | Uleb128 x -> Leb128.length x
        | Sleb128 x -> Leb128.length x
        | String s -> System.Text.Encoding.UTF8.GetByteCount(s) + 1

let private quoteString (s : string) =
    let buffer = System.Text.StringBuilder()
    let inline bprintf s = Printf.bprintf buffer s
    bprintf "\""
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    for x in bytes do
        match char x with
        | '\t' -> bprintf "\\t"
        | '\n' -> bprintf "\\n"
        | '\r' -> bprintf "\\r"
        | '"' ->  bprintf "\\\""
        | c when c >= '\x20' && c <= '\x7f' -> bprintf "%c" c
        | _ -> bprintf "\\x%02x" x
    bprintf "\""
    buffer.ToString()

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
            | Label label ->
                if String.IsNullOrEmpty(label.comment) then
                    sprintf "%s:" label.name
                else
                    sprintf "%s: // %s" label.name label.comment
            | U8 x -> sprintf "\t.byte 0x%x" x
            | S8 x -> sprintf "\t.byte %d" x
            | U16 x -> sprintf "\t.value 0x%x" x
            | S16 x -> sprintf "\t.value %d" x
            | U32 x -> sprintf "\t.long 0x%x" x
            | S32 x -> sprintf "\t.long %d" x
            | U64 x -> sprintf "\t.quad 0x%x" x
            | S64 x -> sprintf "\t.quad %d" x
            | Uleb128 x ->
                sprintf "\t.uleb128 0x%s" (x.ToString("x", System.Globalization.NumberFormatInfo.InvariantInfo))
            | Sleb128 x ->
                if x >= Int128.Zero then
                    sprintf "\t.sleb128 0x%s" (x.ToString("x", System.Globalization.NumberFormatInfo.InvariantInfo))
                else
                    sprintf "\t.sleb128 %s" (x.ToString(System.Globalization.NumberFormatInfo.InvariantInfo))
            | Ref32 label -> sprintf "\t.long %d" (Checked.uint32(m.[label]))
            | Ref64 label -> sprintf "\t.quad %d" (Checked.uint64(m.[label]))
            | String s -> sprintf "\t.string %s" (quoteString s)
            | Expr32 s -> sprintf "\t.long %s" s
            | Expr64 s -> sprintf "\t.quad %s" s
    }

let mutable private labelIndex = 0

let createLabel comment =
    let index = System.Threading.Interlocked.Increment(&labelIndex)
    let label : AsmLabel =
        {
            name = sprintf ".L%d" index
            comment = comment
        }
    label
