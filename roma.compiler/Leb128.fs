module Roma.Compiler.Leb128

let inline private encode_n (x : ^t) n =
    let arr = Array.create n 0uy
    for i = 0 to n - 1 do
        let msb = if i <> n - 1 then 0x80uy else 0uy
        arr.[i] <- (byte(x >>> (7 * i)) &&& 0x7fuy) ||| msb
    arr

let inline private lengthU (x : ^t) =
    let mutable tmp = x
    let mutable n = 0
    while tmp <> LanguagePrimitives.GenericZero do
        tmp <- tmp >>> 7
        n <- n + 1
    max 1 n

let inline private encodeU (x : ^t) =
    encode_n x (lengthU x)

let inline private lengthS size (x : ^t) =
    let zero = LanguagePrimitives.GenericZero
    let one = LanguagePrimitives.GenericOne
    let mutable n = 0
    let mutable value = x
    let mutable more = true
    let negative = x < zero
    while more do
        let x = (byte value) &&& 0x7fuy
        value <- value >>> 7
        if negative then
            value <- value ||| -(one <<< (size - 7))
        if ((value = zero && (x &&& 0x40uy) = 0uy) ||
            (value = -one && (x &&& 0x40uy) <> 0uy)) then
            more <- false
        n <- n + 1
    n

let inline private encodeS size (x : ^t) =
    encode_n x (lengthS size x)

let length (x : 't) =
    match box x with
    | :? uint8 as u -> lengthU u
    | :? uint16 as u -> lengthU u
    | :? uint32 as u -> lengthU u
    | :? uint64 as u -> lengthU u
    | :? UInt128 as u -> lengthU u
    | :? int8 as i -> lengthS 8 i
    | :? int16 as i -> lengthS 16 i
    | :? int32 as i -> lengthS 32 i
    | :? int64 as i -> lengthS 64 i
    | :? Int128 as i -> lengthS 128 i
    | _ -> raise(System.NotSupportedException())

let encode (x : 't) =
    match box x with
    | :? uint8 as u -> encodeU u
    | :? uint16 as u -> encodeU u
    | :? uint32 as u -> encodeU u
    | :? uint64 as u -> encodeU u
    | :? UInt128 as u -> encodeU u
    | :? int8 as i -> encodeS 8 i
    | :? int16 as i -> encodeS 16 i
    | :? int32 as i -> encodeS 32 i
    | :? int64 as i -> encodeS 64 i
    | :? Int128 as i -> encodeS 128 i
    | _ -> raise(System.NotSupportedException())

