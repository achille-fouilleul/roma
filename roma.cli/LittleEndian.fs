namespace Roma.Cli

module LittleEndian =
    let u16 (arr : byte[]) i =
        let x0 = uint16(arr.[i])
        let x1 = uint16(arr.[i + 1]) <<< 8
        x0 ||| x1

    let s16 arr i =
        int16(u16 arr i)

    let u32 (arr : byte[]) i =
        let x0 = uint32(arr.[i])
        let x1 = uint32(arr.[i + 1]) <<< 8
        let x2 = uint32(arr.[i + 2]) <<< 16
        let x3 = uint32(arr.[i + 3]) <<< 24
        x0 ||| x1 ||| x2 ||| x3

    let s32 arr i =
        int32(u32 arr i)

    let u64 (arr : byte[]) i =
        let x0 = uint64(arr.[i])
        let x1 = uint64(arr.[i + 1]) <<< 8
        let x2 = uint64(arr.[i + 2]) <<< 16
        let x3 = uint64(arr.[i + 3]) <<< 24
        let x4 = uint64(arr.[i + 4]) <<< 32
        let x5 = uint64(arr.[i + 5]) <<< 40
        let x6 = uint64(arr.[i + 6]) <<< 48
        let x7 = uint64(arr.[i + 7]) <<< 56
        x0 ||| x1 ||| x2 ||| x3 ||| x4 ||| x5 ||| x6 ||| x7

    let s64 arr i =
        int64(u64 arr i)
