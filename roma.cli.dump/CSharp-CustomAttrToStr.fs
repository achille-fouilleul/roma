module internal Internal.CSharp.CustomAttrToStr

open System
open Roma.Cli
open Internal.StrUtil
open Internal.CSharp.TypeToStr

let private isValueType (typeDef : TypeDef) =
    match typeDef.baseType with
    | None -> false
    | Some(TypeSpec.Choice1Of2 typeRef) ->
        // TODO: check scope = mscorlib
        match typeRef.typeName, typeRef.typeNamespace with
        | "System", "Enum" -> true
        | "System", "ValueType" when (typeDef.typeNamespace, typeDef.typeName) <> ("System", "Enum") -> true
        | _ -> false
    | Some(TypeSpec.Choice2Of2 _) -> false

let private typeRefToTypeSig resolveTypeRef typeRef =
    let typeDef = resolveTypeRef typeRef
    if isValueType typeDef then
        TypeSig.ValueType typeRef
    else
        TypeSig.Class typeRef

type private CAType =
    | Boolean
    | Char
    | I1
    | U1
    | I2
    | U2
    | I4
    | U4
    | I8
    | U8
    | R4
    | R8
    | String
    | SZArray of CAType
    | TypeRef
    | Boxed
    | Enum of TypeRef

let private nullGenVarMap : GenVarMap = { vars = null; mvars = null }

let rec private caTypeToStr resolveTypeRef caType =
    match caType with
    | Boolean -> "bool"
    | Char -> "char"
    | I1 -> "sbyte"
    | U1 -> "byte"
    | I2 -> "short"
    | U2 -> "ushort"
    | I4 -> "int"
    | U4 -> "uint"
    | I8 -> "long"
    | U8 -> "ulong"
    | R4 -> "float"
    | R8 -> "double"
    | String -> "string"
    | SZArray elemType -> caTypeToStr resolveTypeRef elemType + "[]"
    | TypeRef -> "System.Type"
    | Boxed -> "object"
    | Enum typeRef -> typeRefToStr resolveTypeRef nullGenVarMap None typeRef

let private decodePackedLen (r : ByteReader) =
    let x0 = uint32(r.U8())
    match x0 with
    | 0xffu -> -1
    | 0x00u -> 0
    | _ when x0 < 0x80u -> int x0
    | _ when x0 < 0xc0u ->
        let x1 = uint32(r.U8())
        int((x0 <<< 8) ||| x1) &&& 0x3fff
    | _ when x0 < 0xe0u ->
        let x1 = uint32(r.U8())
        let x2 = uint32(r.U8())
        let x3 = uint32(r.U8())
        int((x0 <<< 24) ||| (x1 <<< 16) ||| (x2 <<< 8) ||| x3) &&& 0x1fffffff
    | _ -> failwith "Invalid PackedLen."

let private decodeSerString (r : ByteReader) =
    let l = decodePackedLen r
    match l with
    | -1 -> null
    | 0 -> ""
    | _ ->
        // TODO: speed optimization
        Text.Encoding.UTF8.GetString([| for i = 1 to l do yield r.U8() |])

let private hexDigit c =
    match byte c with
    | c when c >= '0'B && c <= '9'B -> c - '0'B
    | c when c >= 'A'B && c <= 'F'B -> c - 'A'B + 10uy
    | c when c >= 'a'B && c <= 'f'B -> c - 'a'B + 10uy
    | _ -> failwith "Invalid hex digit."

let private parseBlob (s : string) =
    [|
        for i in 0 .. 2 .. (s.Length - 1) ->
            let c0 = hexDigit s.[i]
            let c1 = hexDigit s.[i + 1]
            (c0 <<< 4) ||| c1
    |]

let private parseFqn (r : CharReader) =
    let buf = Text.StringBuilder()
    let rec loop() =
        match r.Peek() with
        | Some c
            // TODO: handle escaping
            when (c >= '0' && c <= '9') ||
                (c >= 'A' && c <= 'Z') ||
                (c >= 'a' && c <= 'z') ||
                c = '.' ||
                c = '_' ||
                c = '<' || c = '>' ||
                c = '`' ->
            r.Advance()
            buf.Append(c) |> ignore
            loop()
        | _ -> buf.ToString()
    let fqn = loop()
    let p = fqn.LastIndexOf('.')
    if p < 0 then
        "", fqn
    else
        fqn.[.. p - 1], fqn.[p + 1 ..]

let private error r =
    failwith "Invalid type name."

let rec private skipWhitespace (r : CharReader) =
    match r.Peek() with
    | Some ' ' ->
        r.Advance()
        skipWhitespace r
    | _ -> ()

let parseTypeName (r : CharReader) =
    let buf = Text.StringBuilder()
    let rec loop() =
        match r.Peek() with
        // TODO: extensive check, escaping
        | Some c
            when c >= '0' && c <= '9' ||
                c >= 'A' && c <= 'Z' ||
                c >= 'a' && c <= 'z' ||
                c = '_' ||
                c = '<' || c = '>' ||
                c = '`' ->
            r.Advance()
            buf.Append(c) |> ignore
            loop()
        | _ -> buf.ToString()
    loop()

let rec parseKV (r : CharReader) =

    let nameBuf = Text.StringBuilder()

    let rec loop() =
        match r.Peek() with
        | Some c when c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' ->
            r.Advance()
            nameBuf.Append(c) |> ignore
            loop()
        | _ -> nameBuf.ToString()

    let name = loop()

    match r.Peek() with
    | Some '=' ->
        r.Advance()

        let valueBuf = Text.StringBuilder()

        let rec loop() =
            match r.Peek() with
            | Some ',' ->
                r.Advance()
                skipWhitespace r
                valueBuf.ToString()
            | Some ']'
            | None ->
                valueBuf.ToString()
            | Some c ->
                r.Advance()
                valueBuf.Append(c) |> ignore
                loop()

        let value = loop()

        Some(name, value)

    | _ ->
        if name.Length <> 0 then
            error r
        None
        
let private parseAssemblyRef (r : CharReader) =

    let buf = Text.StringBuilder()

    // TODO: handle escaping
    let rec loop() =
        match r.Peek() with
        | None -> ()
        | Some ',' ->
            r.Advance()
            skipWhitespace r
        | Some c ->
            r.Advance()
            buf.Append(c) |> ignore
            loop()
    loop()

    let name = buf.ToString()

    let mutable assemblyRef : AssemblyRefRow =
        {
            MajorVersion = 0us
            MinorVersion = 0us
            BuildNumber = 0us
            RevisionNumber = 0us
            Flags = LanguagePrimitives.EnumOfValue 0u
            PublicKeyOrToken = null
            Name = name
            Culture = null
            HashValue = null
        }

    let kvs =
        Seq.unfold (
            fun() ->
                Option.map (fun kv -> kv, ()) (parseKV r)
        ) ()

    for name, value in kvs do
        match name with
        | "Version" ->
            let vs = value.Split('.') |> Array.map uint16
            if vs.Length <> 4 then
                error r
            assemblyRef <- { assemblyRef with MajorVersion = vs.[0]; MinorVersion = vs.[1]; BuildNumber = vs.[2]; RevisionNumber = vs.[3] }
        | "Culture" ->
            assemblyRef <- { assemblyRef with Culture = if value = "neutral" then null else value }
        | "PublicKeyToken" ->
            assemblyRef <- { assemblyRef with Flags = assemblyRef.Flags &&& ~~~AssemblyFlags.PublicKey; PublicKeyOrToken = parseBlob value }
        | _ ->
            Diagnostics.Debugger.Break()
            raise(NotImplementedException()) // TODO

    assemblyRef

let private parseTypeSpec resolveTypeRef (s : string) =
    let r = CharReader(s)

    let rec parseArg() =
        match r.Peek() with
        | Some '[' ->
            r.Advance()
            let arg = parse true
            match r.Read() with
            | Some ']' -> arg
            | _ -> error r
        | _ ->
            parse false

    // TODO: improve type parsing
    and parse withScope =
        let ns, name = parseFqn r

        let typeNesting =
            let rec loop f =
                match r.Peek() with
                | Some '+' ->
                    r.Advance()
                    let name = parseTypeName r
                    loop (fun typeRef ->
                        let typeSpec = TypeSpec.Choice1Of2(f typeRef)
                        let typeRef' : TypeRef =
                            {
                                scope = Some(TypeRefScope typeSpec)
                                typeNamespace = null
                                typeName = name
                            }
                        typeRef'
                    )
                | _ -> f
            loop id

        let args =
            match r.Peek() with
            | Some '[' ->
                r.Advance()

                let args = Collections.Generic.List<_>()

                // parse first argument
                args.Add(parseArg())

                // parse other arguments
                let rec loop() =
                    match r.Peek() with
                    | Some ',' ->
                        r.Advance()
                        skipWhitespace r
                        args.Add(parseArg())
                        loop()
                    | Some ']' ->
                        r.Advance()
                    | _ -> error r

                loop()

                List.ofSeq args

            | _ -> []

        let scope =
            if withScope then
                match r.Peek() with
                | Some ',' ->
                    r.Advance()
                    skipWhitespace r
                    let assemblyRef = parseAssemblyRef r
                    Some(AssemblyRefScope assemblyRef)
                | _ -> None
            else
                None

        let typeRef : TypeRef =
            {
                scope = scope
                typeNamespace = ns
                typeName = name
            }

        let typeRef' = typeNesting typeRef

        match args with
        | [] -> TypeSpec.Choice1Of2 typeRef'
        | _ ->
            let genTypeSig = typeRefToTypeSig resolveTypeRef typeRef'
            let genTypeArgs =
                [
                    for arg in args ->
                        match arg with
                        | TypeSpec.Choice1Of2 typeRef -> typeRefToTypeSig resolveTypeRef typeRef
                        | TypeSpec.Choice2Of2 typeSig -> typeSig
                ]
            TypeSpec.Choice2Of2(GenericInst(genTypeSig, genTypeArgs))

    let typeSpec = parse true
    if r.Offset <> s.Length then
        error r
    typeSpec

let invalidFieldOrPropType() =
    failwith "Invalid FieldOrPropType."

let rec private decodeFieldOrPropType resolveTypeRef (r : ByteReader) =
    let x = r.U8()
    match x with
    | 0x02uy -> Boolean
    | 0x03uy -> Char
    | 0x04uy -> I1
    | 0x05uy -> U1
    | 0x06uy -> I2
    | 0x07uy -> U2
    | 0x08uy -> I4
    | 0x09uy -> U4
    | 0x0auy -> I8
    | 0x0buy -> U8
    | 0x0cuy -> R4
    | 0x0duy -> R8
    | 0x0euy -> String
    | 0x1duy -> SZArray(decodeFieldOrPropType resolveTypeRef r)
    | 0x50uy -> TypeRef
    | 0x51uy -> Boxed
    | 0x55uy ->
        let typeSpec =
            decodeSerString r
            |> parseTypeSpec resolveTypeRef
        match typeSpec with
        | TypeSpec.Choice1Of2 typeRef -> Enum typeRef
        | _ -> invalidFieldOrPropType()
    | _ -> invalidFieldOrPropType()

let private typeSigToCAType typeSig =
    match typeSig with
    | TypeSig.Boolean -> Some Boolean
    | TypeSig.Char -> Some Char
    | TypeSig.I1 -> Some I1
    | TypeSig.U1 -> Some U1
    | TypeSig.I2 -> Some I2
    | TypeSig.U2 -> Some U2
    | TypeSig.I4 -> Some I4
    | TypeSig.U4 -> Some U4
    | TypeSig.I8 -> Some I8
    | TypeSig.U8 -> Some U8
    | TypeSig.R4 -> Some R4
    | TypeSig.R8 -> Some R8
    | TypeSig.String -> Some String
    | _ -> None

let rec private decodeElement resolveTypeRef caType (r : ByteReader) =
    match caType with
    | Boolean ->
        match r.U8() with
        | 0uy -> "false"
        | 1uy -> "true"
        | _ -> failwith "Invalid boolean value."
    | Char -> r.U16() |> char |> charToCSharpStr
    | I1 -> r.S8().ToString(ic)
    | U1 -> r.U8().ToString(ic)
    | I2 -> r.S16().ToString(ic)
    | U2 -> r.U16().ToString(ic)
    | I4 -> r.S32().ToString(ic)
    | U4 -> r.U32().ToString(ic) + "U"
    | I8 -> r.S64().ToString(ic) + "L"
    | U8 -> r.U64().ToString(ic) + "UL"
    | R4 -> r.U32() |> float32ToCSharpStr
    | R8 -> r.U64() |> float64ToCSharpStr
    | String -> decodeSerString r |> strToCSharpStr
    | SZArray elemType ->
        let n = r.U32()
        if n = 0xffffffffu then
            "null"
        else
            let args =
                seq {
                    for i in 1u .. n ->
                        decodeElement resolveTypeRef elemType r
                }
                |> String.concat ", "
            "new " + caTypeToStr resolveTypeRef elemType + "[] { " + args + " }"
    | TypeRef ->
        let s =
            decodeSerString r
            |> parseTypeSpec resolveTypeRef
            |> typeSpecToStr resolveTypeRef nullGenVarMap None
        "typeof(" + s + ")"
    | Boxed ->
        let caType = decodeFieldOrPropType resolveTypeRef r
        decodeElement resolveTypeRef caType r
    | Enum typeRef ->
        decodeEnumElement resolveTypeRef typeRef r

and private decodeEnumElement resolveTypeRef typeRef r =
    let typeDef = resolveTypeRef typeRef
    let name = typeRefToStr resolveTypeRef nullGenVarMap None typeRef
    let constant =
        match enumUnderlyingType typeDef with
        | TypeSig.I1 -> ConstantI1(r.S8())
        | TypeSig.U1 -> ConstantU1(r.U8())
        | TypeSig.I2 -> ConstantI2(r.S16())
        | TypeSig.U2 -> ConstantU2(r.U16())
        | TypeSig.I4 -> ConstantI4(r.S32())
        | TypeSig.U4 -> ConstantU4(r.U32())
        | TypeSig.I8 -> ConstantI8(r.S64())
        | TypeSig.U8 -> ConstantU8(r.U64())
        | _ -> failwith "Invalid underlying enum type."
    
    let fldNames =
        typeDef.fields
        |> List.choose (
            fun fld ->
                match fld.constant with
                Some c when c = constant -> Some(fld.name)
                | _ -> None
        )
    match fldNames with
    | [ fldName ] -> name + "." + fldName
    | _ -> "(" + name + ")" + constantToCSharpStr constant

let rec private decodeFixedArg resolveTypeRef (typeSig : TypeSig) (r : ByteReader) =
    match typeSigToCAType typeSig with
    | Some caType -> decodeElement resolveTypeRef caType r
    | None ->
        match typeSig with
        | TypeSig.Class typeRef
            // TODO: check that scope is system library
            when (typeRef.typeNamespace, typeRef.typeName) = ("System", "Type") ->
            let s =
                decodeSerString r
                |> parseTypeSpec resolveTypeRef
                |> typeSpecToStr resolveTypeRef nullGenVarMap None
            "typeof(" + s + ")"
        | TypeSig.ValueType typeRef ->
            decodeEnumElement resolveTypeRef typeRef r
        | TypeSig.Object ->
            let caType = decodeFieldOrPropType resolveTypeRef r
            decodeElement resolveTypeRef caType r
        | TypeSig.SZArray typeSig ->
            let n = r.U32()
            if n = 0xffffffffu then
                "null"
            else
                let args =
                    seq {
                        for i in 1u .. n ->
                            decodeFixedArg resolveTypeRef typeSig r
                    }
                    |> String.concat ", "
                "new " + typeSigToStr resolveTypeRef nullGenVarMap None typeSig + "[] { " + args + " }"
        | _ ->
            Diagnostics.Debugger.Break()
            raise(NotImplementedException()) // TODO

let attrToStr resolveTypeRef varMap (attr : CustomAttribute) =
    if attr.methodRef.methodName <> ".ctor" then
        raise(NotSupportedException())
    let typeSpec =
        match attr.methodRef.typeRef with
        | None -> raise(NotSupportedException())
        | Some typeSpec -> typeSpec
    let name = typeSpecToStr resolveTypeRef varMap None typeSpec
    let suffix = "Attribute"
    let shortName =
        if name.EndsWith(suffix) then
            name.[0 .. name.Length - suffix.Length - 1]
        else
            name

    let r = ByteReader(attr.value, 0)
    if r.U16() <> 0x0001us then
        failwith "Bad custom attribute prolog."
    // TODO: check method sig is not vararg
    let args =
        [
            for typeSig in attr.methodRef.signature.paramTypes do
                yield decodeFixedArg resolveTypeRef typeSig r
            let numNamed = int(r.U16())
            for i = 1 to numNamed do
                match r.U8() with
                | 0x53uy (* FIELD *)
                | 0x54uy (* PROPERTY *) ->
                    let caType = decodeFieldOrPropType resolveTypeRef r
                    let name = decodeSerString r
                    let arg = decodeElement resolveTypeRef caType r
                    yield name + "=" + arg
                | _ -> failwith "Invalid custom attribute named argument."
        ]
    match args with
    | [] -> shortName
    | _ -> shortName + "(" + String.concat ", " args + ")"

