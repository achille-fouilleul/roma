namespace Roma.Cli

open LittleEndian

type ByteReader(array, index) =
    let mutable offset = index

    member this.Offset = offset

    member this.U8() =
        let x = Array.get array offset
        offset <- offset + 1
        x

    member this.U16() =
        let x = u16 array offset
        offset <- offset + 2
        x

    member this.U32() =
        let x = u32 array offset
        offset <- offset + 4
        x

    member this.S32() =
        this.U32() |> int32

    member this.U64() =
        let x = u64 array offset
        offset <- offset + 8
        x

    member this.S64() =
        this.U64() |> int64
