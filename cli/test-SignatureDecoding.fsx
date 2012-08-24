# r "bin/Debug/cli.dll"

open Roma.Cli.SignatureDecoding

let test cond =
    if not cond then
        failwith "test failed"

[
    [| 0x03uy |], 3u
    [| 0x7fuy |], 0x7fu
    [| 0x80uy; 0x80uy |], 0x80u
    [| 0xaeuy; 0x57uy |], 0x2e57u
    [| 0xbfuy; 0xffuy |], 0x3fffu
    [| 0xc0uy; 0x00uy; 0x40uy; 0x00uy |], 0x4000u
    [| 0xdfuy; 0xffuy; 0xffuy; 0xffuy |], 0x1fffffffu
]
|> List.iter(
    fun (xs, v) ->
        let pos = ref 0
        let r = decodeCompressedUInt xs pos
        test(r = v && !pos = xs.Length)
)

[
    [| 0x06uy |], 3
    [| 0x7buy |], -3
    [| 0x80uy; 0x80uy |], 64
    [| 0x01uy |], -64
    [| 0xc0uy; 0x00uy; 0x40uy; 0x00uy |], 8192
    [| 0x80uy; 0x01uy |], -8192
    [| 0xdfuy; 0xffuy; 0xffuy; 0xfeuy |], 268435455
    [| 0xc0uy; 0x00uy; 0x00uy; 0x01uy |], -268435456
]
|> List.iter(
    fun (xs, v) ->
        let pos = ref 0
        let r = decodeCompressedSInt xs pos
        test(r = v && !pos = xs.Length)
)
