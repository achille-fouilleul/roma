module internal Internal.CSharp.CustomAttrToStr

open System
open Roma.Cli
open Internal.StrUtil
open Internal.CSharp.TypeToStr

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

let private parseTypeRef (typeStr : string) =
    // TODO: improve type parsing
    let xs = typeStr.Split([| ", " |], StringSplitOptions.None)
    let fqn = xs.[0]
    let p = fqn.LastIndexOf('.')
    let ns, name =
        if p < 0 then
            null, fqn
        else
            fqn.[.. p - 1], fqn.[p + 1 ..]

    let scope =
        if xs.Length = 1 then
            None
        else
            let mutable assemblyRef : AssemblyRefRow =
                {
                    MajorVersion = 0us
                    MinorVersion = 0us
                    BuildNumber = 0us
                    RevisionNumber = 0us
                    Flags = LanguagePrimitives.EnumOfValue 0u
                    PublicKeyOrToken = null
                    Name = xs.[1]
                    Culture = null
                    HashValue = null
                }
            for x in xs.[2.. ] do
                let p = x.IndexOf('=')
                if p < 0 then
                    failwith "Invalid type name."
                let name, value = x.[.. p - 1], x.[p + 1 ..]
                match name with
                | "Version" ->
                    let vs = value.Split('.') |> Array.map uint16
                    // TODO: check number of elements
                    assemblyRef <- { assemblyRef with MajorVersion = vs.[0]; MinorVersion = vs.[1]; BuildNumber = vs.[2]; RevisionNumber = vs.[3] }
                | "Culture" ->
                    assemblyRef <- { assemblyRef with Culture = if value = "neutral" then null else value }
                | "PublicKeyToken" ->
                    assemblyRef <- { assemblyRef with Flags = assemblyRef.Flags &&& ~~~AssemblyFlags.PublicKey; PublicKeyOrToken = parseBlob value }
                | _ ->
                    Diagnostics.Debugger.Break()
                    raise(NotImplementedException()) // TODO
            Some(AssemblyRefScope assemblyRef)

    { scope = scope; typeNamespace = ns; typeName = name }

let rec private decodeFieldOrPropType (r : ByteReader) =
    let x = r.U8()
    match x with
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
    | 0x1duy -> TypeSig.SZArray(decodeFieldOrPropType r)
    | 0x55uy ->
        let typeStr = decodeSerString r
        TypeSig.ValueType(parseTypeRef typeStr)
    | _ -> failwith "Invalid FieldOrPropType."

let private decodeFixedArg resolveTypeRef typeSig (r : ByteReader) =
    let decodeSimple typeSig =
        match typeSig with
        | Boolean ->
            match r.U8() with
            | 0uy -> "false"
            | 1uy -> "true"
            | _ -> failwith "Invalid boolean value."
        | I1 -> r.S8().ToString(ic)
        | U1 -> r.U8().ToString(ic)
        | I2 -> r.S16().ToString(ic)
        | U2 -> r.U16().ToString(ic)
        | I4 -> r.S32().ToString(ic)
        | U4 -> r.U32().ToString(ic) + "U"
        | I8 -> r.S64().ToString(ic) + "L"
        | U8 -> r.U64().ToString(ic) + "UL"
        | String -> decodeSerString r |> strToCSharpStr
        | _ ->
            Diagnostics.Debugger.Break()
            raise(NotImplementedException()) // TODO: float, char

    match typeSig with
    | Boolean | I1 | U1 | I2 | U2 | I4 | U4 | I8 | U8 | String -> decodeSimple typeSig // TODO: float, char
    | Class typeRef
        // TODO: check that scope is system library
        when (typeRef.typeNamespace, typeRef.typeName) = ("System", "Type") ->
        "typeof(" + decodeSerString r + ")" // TODO: remove cruft
    | ValueType typeRef ->
        let name = typeRefToStr { vars = null; mvars = null} None typeRef
        let typeDef = resolveTypeRef typeRef
        let typeSig = enumUnderlyingType typeDef
        let s = decodeSimple typeSig
        "(" + name + ")" + s
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
    let name = typeSpecToStr varMap None typeSpec
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
                    let typeSig = decodeFieldOrPropType r
                    let name = decodeSerString r
                    let arg = decodeFixedArg resolveTypeRef typeSig r
                    yield name + "=" + arg
                | _ -> failwith "Invalid custom attribute named argument."
        ]
    match args with
    | [] -> shortName
    | _ -> shortName + "(" + String.concat ", " args + ")"

