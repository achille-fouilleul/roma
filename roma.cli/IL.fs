module Roma.Cli.Disassembling

open Tables
open SignatureDecoding

let private map0 =
    [
        0x00uy, Nop
        0x01uy, Break
        0x02uy, Ldarg_0
        0x03uy, Ldarg_1
        0x04uy, Ldarg_2
        0x05uy, Ldarg_3
        0x06uy, Ldloc_0
        0x07uy, Ldloc_1
        0x08uy, Ldloc_2
        0x09uy, Ldloc_3
        0x0auy, Stloc_0
        0x0buy, Stloc_1
        0x0cuy, Stloc_2
        0x0duy, Stloc_3
        0x14uy, Ldnull
        0x15uy, Ldc_i4_m1
        0x16uy, Ldc_i4_0
        0x17uy, Ldc_i4_1
        0x18uy, Ldc_i4_2
        0x19uy, Ldc_i4_3
        0x1auy, Ldc_i4_4
        0x1buy, Ldc_i4_5
        0x1cuy, Ldc_i4_6
        0x1duy, Ldc_i4_7
        0x1euy, Ldc_i4_8
        0x25uy, Dup
        0x26uy, Pop
        0x2auy, Ret
        0x46uy, Ldind_i1
        0x47uy, Ldind_u1
        0x48uy, Ldind_i2
        0x49uy, Ldind_u2
        0x4auy, Ldind_i4
        0x4buy, Ldind_u4
        0x4cuy, Ldind_i8
        0x4duy, Ldind_i
        0x4euy, Ldind_r4
        0x4fuy, Ldind_r8
        0x50uy, Ldind_ref
        0x51uy, Stind_ref
        0x52uy, Stind_i1
        0x53uy, Stind_i2
        0x54uy, Stind_i4
        0x55uy, Stind_i8
        0x56uy, Stind_r4
        0x57uy, Stind_r8
        0x58uy, Add
        0x59uy, Sub
        0x5auy, Mul
        0x5buy, Div
        0x5cuy, Div_un
        0x5duy, Rem
        0x5euy, Rem_un
        0x5fuy, And
        0x60uy, Or
        0x61uy, Xor
        0x62uy, Shl
        0x63uy, Shr
        0x64uy, Shr_un
        0x65uy, Neg
        0x66uy, Not
        0x67uy, Conv_i1
        0x68uy, Conv_i2
        0x69uy, Conv_i4
        0x6auy, Conv_i8
        0x6buy, Conv_r4
        0x6cuy, Conv_r8
        0x6duy, Conv_u4
        0x6euy, Conv_u8
        0x76uy, Conv_r_un
        0x7auy, Throw
        0x82uy, Conv_ovf_i1_un
        0x83uy, Conv_ovf_i2_un
        0x84uy, Conv_ovf_i4_un
        0x85uy, Conv_ovf_i8_un
        0x86uy, Conv_ovf_u1_un
        0x87uy, Conv_ovf_u2_un
        0x88uy, Conv_ovf_u4_un
        0x89uy, Conv_ovf_u8_un
        0x8auy, Conv_ovf_i_un
        0x8buy, Conv_ovf_u_un
        0x8euy, Ldlen
        0x90uy, Ldelem_i1
        0x91uy, Ldelem_u1
        0x92uy, Ldelem_i2
        0x93uy, Ldelem_u2
        0x94uy, Ldelem_i4
        0x95uy, Ldelem_u4
        0x96uy, Ldelem_i8
        0x97uy, Ldelem_i
        0x98uy, Ldelem_r4
        0x99uy, Ldelem_r8
        0x9auy, Ldelem_ref
        0x9buy, Stelem_i
        0x9cuy, Stelem_i1
        0x9duy, Stelem_i2
        0x9euy, Stelem_i4
        0x9fuy, Stelem_i8
        0xa0uy, Stelem_r4
        0xa1uy, Stelem_r8
        0xa2uy, Stelem_ref
        0xb3uy, Conv_ovf_i1
        0xb4uy, Conv_ovf_u1
        0xb5uy, Conv_ovf_i2
        0xb6uy, Conv_ovf_u2
        0xb7uy, Conv_ovf_i4
        0xb8uy, Conv_ovf_u4
        0xb9uy, Conv_ovf_i8
        0xbauy, Conv_ovf_u8
        0xc3uy, Ckfinite
        0xd1uy, Conv_u2
        0xd2uy, Conv_u1
        0xd3uy, Conv_i
        0xd4uy, Conv_ovf_i
        0xd5uy, Conv_ovf_u
        0xd6uy, Add_ovf
        0xd7uy, Add_ovf_un
        0xd8uy, Mul_ovf
        0xd9uy, Mul_ovf_un
        0xdauy, Sub_ovf
        0xdbuy, Sub_ovf_un
        0xdcuy, Endfinally // also endfault
        0xdfuy, Stind_i
        0xe0uy, Conv_u
    ]
    |> Map.ofList

let private mapbrs =
    [
        0x2buy, Br_s
        0x2cuy, Brfalse_s
        0x2duy, Brtrue_s
        0x2euy, Beq_s
        0x2fuy, Bge_s
        0x30uy, Bgt_s
        0x31uy, Ble_s
        0x32uy, Blt_s
        0x33uy, Bne_un_s
        0x34uy, Bge_un_s
        0x35uy, Bgt_un_s
        0x36uy, Ble_un_s
        0x37uy, Blt_un_s
        0xdeuy, Leave_s
    ]
    |> Map.ofList

let private mapbr =
    [
        0x38uy, Br
        0x39uy, Brfalse
        0x3auy, Brtrue
        0x3buy, Beq
        0x3cuy, Bge
        0x3duy, Bgt
        0x3euy, Ble
        0x3fuy, Blt
        0x40uy, Bne_un
        0x41uy, Bge_un
        0x42uy, Bgt_un
        0x43uy, Ble_un
        0x44uy, Blt_un
        0xdduy, Leave
    ]
    |> Map.ofList

let private maptype =
    [
        0x70uy, Cpobj
        0x71uy, Ldobj
        0x74uy, Castclass
        0x75uy, Isinst
        0x79uy, Unbox
        0x81uy, Stobj
        0x8cuy, Box
        0x8duy, Newarr
        0x8fuy, Ldelema
        0xa3uy, Ldelem
        0xa4uy, Stelem
        0xa5uy, Unbox_any
        0xc2uy, Refanyval
        0xc6uy, Mkrefany
    ]
    |> Map.ofList

let private mapfld =
    [
        0x7buy, Ldfld
        0x7cuy, Ldflda
        0x7duy, Stfld
        0x7euy, Ldsfld
        0x7fuy, Ldsflda
        0x80uy, Stsfld
    ]
    |> Map.ofList

let private (|OfMap|_|) map opcode =
    Map.tryFind opcode map

let decodeInstructions (mr : IModuleLoader) (codeBytes : byte[]) =
    [
        let reader = ByteReader(codeBytes, 0)

        let readToken() =
            tokenOfValue(reader.U32())

        while reader.Offset < codeBytes.Length do
            let offset = reader.Offset
            let opcode = reader.U8()
            let instr =
                match opcode with
                | OfMap map0 instr -> instr
                | OfMap mapbrs ctor ->
                    let delta = reader.U8() |> int8 |> int
                    ctor(reader.Offset + delta)
                | OfMap mapbr ctor ->
                    let delta = reader.U32() |> int
                    ctor(reader.Offset + delta)
                | OfMap maptype ctor -> ctor(mr.GetTypeRef(readToken()))
                | OfMap mapfld ctor -> ctor(mr.GetFieldRef(readToken()))
                | 0x0euy -> Ldarg_s(reader.U8() |> int)
                | 0x0fuy -> Ldarga_s(reader.U8() |> int)
                | 0x10uy -> Starg_s(reader.U8() |> int)
                | 0x11uy -> Ldloc_s(reader.U8() |> int)
                | 0x12uy -> Ldloca_s(reader.U8() |> int)
                | 0x13uy -> Stloc_s(reader.U8() |> int)
                | 0x1fuy -> Ldc_i4_s(reader.U8() |> int8 |> int32)
                | 0x20uy -> Ldc_i4(reader.S32())
                | 0x21uy -> Ldc_i8(reader.S64())
                | 0x22uy -> Ldc_r4(reader.U32())
                | 0x23uy -> Ldc_r8(reader.U64())
                | 0x27uy -> Jmp(mr.GetMethodSpec(readToken()))
                | 0x28uy -> Call(mr.GetMethodSpec(readToken()))
                | 0x29uy -> Calli(mr.GetMethodSig(readToken()))
                | 0x45uy ->
                    let n = reader.S32()
                    let deltas = [| for i in 1 .. n -> reader.S32() |]
                    Switch [| for delta in deltas -> reader.Offset + delta |]
                | 0x6fuy -> Callvirt(mr.GetMethodSpec(readToken()))
                | 0x72uy ->
                    let token = reader.U32()
                    if (token >>> 24) <> 0x70u then
                        failwith "invalid string literal token"
                    let s = mr.GetUserString(token &&& 0xffffffu)
                    Ldstr s
                | 0x73uy -> Newobj(mr.GetMethodSpec(readToken()))
                | 0xd0uy ->
                    let token = readToken ()
                    match token with
                    | (TableNumber.MethodDef, _)
                    | (TableNumber.MethodSpec, _) ->
                        Ldtoken_method(mr.GetMethodSpec(token))
                    | (TableNumber.TypeDef, _)
                    | (TableNumber.TypeRef, _)
                    | (TableNumber.TypeSpec, _) ->
                        Ldtoken_type(mr.GetTypeRef(token))
                    | (TableNumber.Field, _) ->
                        Ldtoken_field(mr.GetFieldRef(token))
                    | (TableNumber.MemberRef, _) ->
                        match mr.GetMemberRef(token) with
                        | MemberRef.Choice1Of2 methodRef ->
                            let methodSpec : MethodSpec = {
                                methodRef = methodRef
                                args = []
                            }
                            Ldtoken_method methodSpec
                        | MemberRef.Choice2Of2 fieldRef ->
                            Ldtoken_field fieldRef
                    | _ -> failwith "invalid token type for ldtoken"
                | 0xfeuy ->
                    let ext = reader.U8()
                    match ext with
                    | 0x00uy -> Arglist
                    | 0x01uy -> Ceq
                    | 0x02uy -> Cgt
                    | 0x03uy -> Cgt_un
                    | 0x04uy -> Clt
                    | 0x05uy -> Clt_un
                    | 0x06uy -> Ldftn(mr.GetMethodSpec(readToken()))
                    | 0x07uy -> Ldvirtftn(mr.GetMethodSpec(readToken()))
                    | 0x09uy -> Ldarg(reader.U16() |> int)
                    | 0x0auy -> Ldarga(reader.U16() |> int)
                    | 0x0buy -> Starg(reader.U16() |> int)
                    | 0x0cuy -> Ldloc(reader.U16() |> int)
                    | 0x0duy -> Ldloca(reader.U16() |> int)
                    | 0x0euy -> Stloc(reader.U16() |> int)
                    | 0x0fuy -> Localloc
                    | 0x11uy -> Endfilter
                    | 0x12uy -> Unaligned
                    | 0x13uy -> Volatile
                    | 0x14uy -> Tail
                    | 0x15uy -> Initobj(mr.GetTypeRef(readToken()))
                    | 0x16uy -> Constrained(mr.GetTypeRef(readToken()))
                    | 0x17uy -> Cpblk
                    | 0x18uy -> Initblk
                    | 0x1auy -> Rethrow
                    | 0x1cuy -> Sizeof(mr.GetTypeRef(readToken()))
                    | 0x1duy -> Refanytype
                    | 0x1euy -> Readonly
                    | _ -> failwith "invalid extended opcode"
                | _ -> failwith "invalid opcode"
            yield (offset, instr)
    ]
