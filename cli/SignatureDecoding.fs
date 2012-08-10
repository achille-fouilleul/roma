module Roma.Cli.SignatureDecoding

open System

type IModuleReader =
    abstract member GetTypeRef : Token -> TypeSpec

let private getTypeRefFromCodedIndex (this : IModuleReader) codedIndex =
    this.GetTypeRef(CodedIndexes.typeDefOrRef.Decode(codedIndex))

[<Flags>]
type private Bits =
    | Zero = 0x00uy
    | Default = 0x00uy
    | C = 0x01uy
    | Stdcall = 0x02uy
    | Thiscall = 0x03uy
    | Fastcall = 0x04uy
    | Vararg = 0x05uy
    | Generic = 0x10uy
    | HasThis = 0x20uy
    | ExplicitThis = 0x40uy

let private decodeCompressedInt (blob : byte[]) off =
    let x0 = blob.[!off] |> uint32
    off := !off + 1
    if x0 < 0x80u then
        x0, 1
    else
        let x1 = blob.[!off] |> uint32
        off := !off + 1
        if x0 < 0xc0u then
            ((x0 <<< 8) ||| x1) &&& 0x3fffu, 2
        else
            if x0 < 0xe0u then
                let x2, x3 = (blob.[!off] |> uint32), (blob.[!off + 1] |> uint32)
                off := !off + 2
                ((x0 <<< 24) ||| (x1 <<< 16) ||| (x2 <<< 8) ||| x3) &&& 0x1fffffffu, 4
            else
                // invalid or special 0xff value
                raise (new NotImplementedException())

let decodeCompressedUInt (blob : byte[]) off : uint32 =
    let x, _ = decodeCompressedInt blob off
    x

let private rotSex n x =
    let s = (x &&& 1u) <> 0u
    let r =
        if s then
            (x >>> 1) ||| (0xffffffffu <<< (n - 1))
        else
            (x >>> 1)
    int32 r

let decodeCompressedSInt (blob : byte[]) off : int32 =
    let x, n = decodeCompressedInt blob off
    match n with
    | 1 -> rotSex 7 x
    | 2 -> rotSex 14 x
    | 4 -> rotSex 29 x
    | _ ->
        // invalid or special 0xff value
        raise(new System.NotImplementedException())

let private decodeArrayShape blob off =
    let rank = decodeCompressedUInt blob off |> int
    let numSizes = decodeCompressedUInt blob off |> int
    let sizes =
        [
            for i in 1 .. rank ->
                if i <= numSizes then
                    Some (decodeCompressedUInt blob off |> int)
                else
                    None
        ]
    let numLoBounds = decodeCompressedUInt blob off |> int
    let loBounds =
        [
            for i in 1 .. rank ->
                if i <= numLoBounds then
                    decodeCompressedSInt blob off
                else
                    0
        ]
    List.zip loBounds sizes

let rec decodeTypeSig (mr : IModuleReader) (blob : byte[]) off : TypeSig =
    let hd = blob.[!off]
    off := !off + 1
    match hd with
    | 0x01uy -> TypeSig.Void
    | 0x02uy -> TypeSig.Boolean
    | 0x03uy -> TypeSig.Char
    | 0x04uy -> TypeSig.I1
    | 0x05uy -> TypeSig.U1
    | 0x06uy -> TypeSig.I2
    | 0x07uy -> TypeSig.U2
    | 0x08uy -> TypeSig.I4
    | 0x09uy -> TypeSig.U4
    | 0x0auy -> TypeSig.I8
    | 0x0buy -> TypeSig.U8
    | 0x0cuy -> TypeSig.R4
    | 0x0duy -> TypeSig.R8
    | 0x0euy -> TypeSig.String
    | 0x0fuy -> TypeSig.Ptr (decodeTypeSig mr blob off)
    | 0x10uy -> TypeSig.ByRef (decodeTypeSig mr blob off)

    | 0x11uy ->
        decodeCompressedUInt blob off
        |> getTypeRefFromCodedIndex mr
        |> TypeSig.ValueType

    | 0x12uy ->
        decodeCompressedUInt blob off
        |> getTypeRefFromCodedIndex mr
        |> TypeSig.Class

    | 0x13uy -> TypeSig.Var (decodeCompressedUInt blob off |> int)

    | 0x14uy ->
        let typeSig = decodeTypeSig mr blob off
        let shape = decodeArrayShape blob off
        TypeSig.Array(typeSig, shape)

    | 0x15uy ->
        let genType = decodeTypeSig mr blob off
        let genArgCount = decodeCompressedUInt blob off |> int
        let args =
            [
                for i in 1 .. genArgCount ->
                    decodeTypeSig mr blob off
            ]
        TypeSig.GenericInst(genType, args)

    | 0x16uy -> TypeSig.TypedByRef
    | 0x18uy -> TypeSig.I
    | 0x19uy -> TypeSig.U
    | 0x1buy -> TypeSig.Fnptr (decodeMethodSig mr blob off)
    | 0x1cuy -> TypeSig.Object
    | 0x1duy -> TypeSig.SZArray (decodeTypeSig mr blob off)
    | 0x1euy -> TypeSig.MVar (decodeCompressedUInt blob off |> int)

    | 0x1fuy ->
        let typeRef = decodeCompressedUInt blob off |> getTypeRefFromCodedIndex mr
        let typeSig = decodeTypeSig mr blob off
        TypeSig.ModReq(typeRef, typeSig)

    | 0x20uy ->
        let typeRef = decodeCompressedUInt blob off |> getTypeRefFromCodedIndex mr
        let typeSig = decodeTypeSig mr blob off
        TypeSig.ModOpt(typeRef, typeSig)

    | 0x45uy ->
        let typeSig = decodeTypeSig mr blob off
        TypeSig.Pinned typeSig

    // TODO
    | _-> raise(NotImplementedException()) // TODO

and decodeMethodSig (mr : IModuleReader) (blob : byte[]) off =
    let hd = LanguagePrimitives.EnumOfValue blob.[!off]
    off := !off + 1
    let hasThis = (hd &&& Bits.HasThis) <> Bits.Zero
    let explicitThis = (hd &&& Bits.ExplicitThis) <> Bits.Zero
    let genParamCount =
        if (hd &&& Bits.Generic) <> Bits.Zero then
            decodeCompressedUInt blob off |> int
        else
            0
    let callKind =
        match hd &&& ~~~(Bits.HasThis ||| Bits.ExplicitThis ||| Bits.Generic) with
        | Bits.Default -> Default
        | Bits.C -> Cdecl
        | Bits.Stdcall -> Stdcall
        | Bits.Thiscall -> Thiscall
        | Bits.Fastcall -> Fastcall
        | Bits.Vararg -> Vararg
        | _ -> raise (new System.NotImplementedException())
    let callConv : CallConv = {
        hasThis = hasThis
        explicitThis = explicitThis
        callKind = callKind
    }
    let paramCount = decodeCompressedUInt blob off
    let retType = decodeTypeSig mr blob off
    let types, varTypes =
        let rec loop types varTypes i =
            if i < paramCount then
                match varTypes with
                | None ->
                    match blob.[!off] with
                    | 0x41uy ->
                        off := !off + 1
                        loop types (Some List.empty) i
                    | _ ->
                        let newTypes = (decodeTypeSig mr blob off) :: types
                        loop newTypes varTypes (i + 1u)
                | Some vts ->
                    let newVarTypes = (decodeTypeSig mr blob off) :: vts
                    loop types (Some newVarTypes) (i + 1u)
            else
                types, varTypes
        loop List.empty None 0u
    let methodSig : MethodSig = {
        callConv = callConv
        genParamCount = genParamCount
        retType = retType
        paramTypes = types |> List.rev |> List.toArray
        varargParamTypes = Option.map (fun xs -> xs |> List.rev |> List.toArray) varTypes
    }
    methodSig

let decodeFieldSig (mr : IModuleReader) (blob : byte[]) =
    if blob.[0] <> 0x06uy then
        failwith "Invalid field signature."
    let off = ref 1
    decodeTypeSig mr blob off

let decodeMethodSpec (mr : IModuleReader) (blob : byte[]) =
    if blob.[0] <> 0x0auy then
        failwith "Invalid MethodSpec blob signature."
    let off = ref 1
    let n = decodeCompressedUInt blob off |> Checked.int
    [|
        for i in 1 .. n ->
            decodeTypeSig mr blob off
    |]

let decodePropertySig (mr : IModuleReader) (blob : byte[]) =
    let hasThis =
        match blob.[0] with
        | 0x08uy -> false
        | 0x28uy -> true
        | _ -> failwith "Invalid PropertySig blob signature."
    let off = ref 1
    let n = decodeCompressedUInt blob off |> Checked.int
    let retType = decodeTypeSig mr blob off
    let paramTypes =
        [|
            for i in 1 .. n ->
                decodeTypeSig mr blob off
        |]
    (hasThis, retType, paramTypes)

let decodeIntConstant (blob : byte[]) t0 conv len =
    if Array.length blob <> len then
        failwith "Invalid integer constant."
    let mutable t = t0
    for i = 0 to Array.length blob - 1 do
        t <- conv t blob.[i] (i * 8)
    t

let decodeU1 blob =
    match blob with
    | [| x |] -> x
    | [| x; 0uy |] -> x
    | _ -> failwith "Invalid integer constant."

let decodeI1 blob =
    decodeU1 blob |> Operators.sbyte

let decodeU2 blob =
    decodeIntConstant blob 0us (fun t x s -> t ||| ((Operators.uint16 x) <<< s)) 2

let decodeI2 blob =
    decodeU2 blob |> Operators.int16

let decodeU4 blob =
    decodeIntConstant blob 0u (fun t x s -> t ||| ((Operators.uint32 x) <<< s)) 4

let decodeI4 blob =
    decodeU4 blob |> Operators.int32

let decodeU8 blob =
    decodeIntConstant blob 0UL (fun t x s -> t ||| ((Operators.uint64 x) <<< s)) 8

let decodeI8 blob =
    decodeU8 blob |> Operators.int64

let decodeConstant elemType blob =
    match elemType with
    | 0x02uy ->
        match blob with
        | [| 0uy |] | [| 0uy; 0uy |] -> ConstantBool false
        | [| 1uy |] | [| 1uy; 0uy |] -> ConstantBool true
        | _ -> failwith "Bad bool constant."
    | 0x03uy -> ConstantChar (decodeU2 blob)
    | 0x04uy -> ConstantI1 (decodeI1 blob)
    | 0x05uy -> ConstantU1 (decodeU1 blob)
    | 0x06uy -> ConstantI2 (decodeI2 blob)
    | 0x07uy -> ConstantU2 (decodeU2 blob)
    | 0x08uy -> ConstantI4 (decodeI4 blob)
    | 0x09uy -> ConstantU4 (decodeU4 blob)
    | 0x0auy -> ConstantI8 (decodeI8 blob)
    | 0x0buy -> ConstantU8 (decodeU8 blob)
    | 0x0cuy -> ConstantR4 (decodeU4 blob)
    | 0x0duy -> ConstantR8 (decodeU8 blob)
    | 0x0euy ->
        let s =
            if blob <> null then
                System.Text.Encoding.Unicode.GetString(blob)
            else
                ""
        ConstantString s
    | 0x12uy ->
        if blob <> "\x00\x00\x00\x00"B then
            failwith "Bad nullref constant."
        ConstantNullRef
    | _ -> raise (new System.NotImplementedException())

let private decodeNativeTypeSigStr blob off =
    let len = decodeCompressedUInt blob off |> int
    if len <> 0 then
        let s = System.Text.Encoding.UTF8.GetString(blob, !off, len)
        off := !off + len
        s
    else
        null

let private decodeNativeTypeSigIntrinsic (blob : byte[]) off =
    let code = blob.[!off]
    off := 1 + !off
    match code with
    | 0x02uy -> NativeBool
    | 0x03uy -> NativeI1
    | 0x04uy -> NativeU1
    | 0x05uy -> NativeI2
    | 0x06uy -> NativeU2
    | 0x07uy -> NativeI4
    | 0x08uy -> NativeU4
    | 0x09uy -> NativeI8
    | 0x0auy -> NativeU8
    | 0x0buy -> NativeR4
    | 0x0cuy -> NativeR8
    | 0x13uy -> NativeBstr
    | 0x14uy -> NativeLpstr
    | 0x15uy -> NativeLpwstr
    | 0x16uy -> NativeLptstr
    | 0x17uy -> NativeFixedSysString (decodeCompressedUInt blob off |> int)
    | 0x19uy -> NativeIUnknown
    | 0x1auy -> NativeIDispatch
    | 0x1buy -> NativeStruct
    | 0x1cuy -> NativeInterface
    | 0x1duy -> NativeSafeArray
    | 0x1euy -> NativeFixedArray (decodeCompressedUInt blob off |> int)
    | 0x1fuy -> NativeI
    | 0x20uy -> NativeU
    | 0x26uy -> NativeFunc
    | 0x28uy -> NativeAsAny
    | 0x2buy -> NativeLpstruct
    | 0x2cuy ->
        decodeNativeTypeSigStr blob off |> ignore
        decodeNativeTypeSigStr blob off |> ignore
        let s1 = decodeNativeTypeSigStr blob off
        let s2 = decodeNativeTypeSigStr blob off
        NativeCustom (s1, s2)
    | 0x2duy -> NativeError
    | _ -> failwith "Invalid native intrinsic."

let private decodeNativeTypeSigInt blob off =
    if !off < Array.length blob then
        Some (decodeCompressedUInt blob off |> int)
    else
        None

let decodeMarshal (blob : byte[]) =
    let off = ref 0
    let hd = blob.[0]
    if hd = 0x2auy then
        off := 1 + !off
        let elemType =
            match blob.[!off] with
            | 0x50uy ->
                off := 1 + !off
                None
            | _ -> Some (decodeNativeTypeSigIntrinsic blob off)
        let m = decodeNativeTypeSigInt blob off
        let n = decodeNativeTypeSigInt blob off
        NativeArray (elemType, m, n)
    else
        decodeNativeTypeSigIntrinsic blob off

let decodeLocalVarSig (mr : IModuleReader) (blob : byte[]) =
    if blob.[0] <> 0x07uy then
        failwith "Invalid local var signature."
    let off = ref 1
    let count = decodeCompressedUInt blob off
    [|
        for i in 1u .. count ->
            decodeTypeSig mr blob off
    |]
