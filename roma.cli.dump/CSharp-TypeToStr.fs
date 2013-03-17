module internal Internal.CSharp.TypeToStr

open System
open Roma.Cli
open Internal.StrUtil

type GenVarMap =
    {
        vars : string[]
        mvars : string[]
    }

    member this.Var(n) = this.vars.[n]

    member this.MVar(n) = this.mvars.[n]

type ByRefDir =
    | OutOnly
    | InOut

let rec typeSpecToStr resolveTypeRef varMap byRefDir typeSpec =
    match typeSpec with
    | TypeSpec.Choice1Of2 typeRef -> typeRefToStr resolveTypeRef varMap byRefDir typeRef
    | TypeSpec.Choice2Of2 typeSig -> typeSigToStr resolveTypeRef varMap byRefDir typeSig

// TODO: factor code (cf. GenericInst case in typeSigToStr)
and typeRefToStr resolveTypeRef varMap byRefDir typeRef =
    let makeName (typeDef : TypeDef) i =
        let name = typeDef.typeName
        let n = typeDef.genericParams.Length - i
        if n > 0 then
            let expectedSuffix = "`" + intToStr n
            let shortName =
                if name.EndsWith(expectedSuffix) then
                    name.[.. name.Length - expectedSuffix.Length - 1]
                else
                    // TODO: warn about missing suffix
                    Diagnostics.Debugger.Break()
                    name
            shortName + "<" + String.replicate (n - 1) "," + ">", i + n
        else
            name, i

    let rec loop typeRef =
        match typeRef.scope with
        | Some(TypeRefScope enclosingTypeRef) ->
            let typeDef, s, i = loop enclosingTypeRef
            let typeDef' =
                typeDef.nestedTypes
                |> Seq.find (fun typeDef -> typeDef.typeName = typeRef.typeName)
            let name, i' = makeName typeDef' i
            typeDef', s + "." + name, i'
        | _ ->
            let typeDef = resolveTypeRef typeRef
            let name, i' = makeName typeDef 0
            let s =
                match typeRef.typeNamespace with
                | "" -> name
                | ns -> ns + "." + name
            typeDef, s, i'
    let _, s, _ = loop typeRef
    s

and typeSigToStr resolveTypeRef (varMap : GenVarMap) byRefDir typeSig =
    match typeSig with
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
    | I -> "System.IntPtr"
    | U -> "System.UIntPtr"
    | Array(typeSig, dims) ->
        if not(List.forall (fun dim -> dim = (0, None)) dims) then
            failwith "Unsupported array shape."
        typeSigToStr resolveTypeRef varMap byRefDir typeSig + "[" + (String.replicate (List.length dims - 1) ",") + "]"
    | ByRef typeSig ->
        let typeStr = typeSigToStr resolveTypeRef varMap None typeSig
        match byRefDir with
        | None -> "ref " + typeStr // TODO: warn
        | Some OutOnly -> "out " + typeStr
        | Some InOut -> "ref " + typeStr
    | Fnptr methodSig ->
        let retTypeStr = typeSigToStr resolveTypeRef varMap None methodSig.retType
        let paramTypes = Seq.map (typeSigToStr resolveTypeRef varMap None) methodSig.paramTypes
        "__fnptr(" + retTypeStr + "(" + String.concat ", " paramTypes + ")" // TODO: calling convention
    | GenericInst(typeSig, args) ->
        match typeSig with
        | TypeSig.Class typeRef
        | TypeSig.ValueType typeRef ->
            let args' =
                [|
                    for arg in args ->
                        typeSigToStr resolveTypeRef varMap byRefDir arg
                |]

            let makeName (typeDef : TypeDef) i =
                let name = typeDef.typeName
                let n = typeDef.genericParams.Length - i
                if n > 0 then
                    let expectedSuffix = "`" + intToStr n
                    let shortName =
                        if name.EndsWith(expectedSuffix) then
                            name.[.. name.Length - expectedSuffix.Length - 1]
                        else
                            // TODO: warn about missing suffix
                            Diagnostics.Debugger.Break()
                            name
                    shortName + "<" + String.concat ", " args'.[i .. i + n - 1] + ">", i + n
                else
                    name, i

            let rec loop typeRef =
                match typeRef.scope with
                | Some(TypeRefScope enclosingTypeRef) ->
                    let typeDef, s, i = loop enclosingTypeRef
                    let typeDef' =
                        typeDef.nestedTypes
                        |> Seq.find (fun typeDef -> typeDef.typeName = typeRef.typeName)
                    let name, i' = makeName typeDef' i
                    typeDef', s + "." + name, i'
                | _ ->
                    let typeDef = resolveTypeRef typeRef
                    let name, i' = makeName typeDef 0
                    let s =
                        match typeDef.typeNamespace with
                        | "" -> name
                        | ns -> ns + "." + name
                    typeDef, s, i'

            let _, s, _ = loop typeRef
            s
        | _ -> failwith "Invalid GenericInst."
    | MVar n -> varMap.MVar(n)
    | Object -> "object"
    | Ptr typeSig -> typeSigToStr resolveTypeRef varMap byRefDir typeSig + "*"
    | String -> "string"
    | SZArray typeSig -> typeSigToStr resolveTypeRef varMap byRefDir typeSig + "[]"
    | TypedByRef -> "System.TypedReference"
    | Var n -> varMap.Var(n)
    | Void -> "void"
    | ModReq(custMod, typeSig) ->
        let custModStr = typeRefToStr resolveTypeRef varMap None custMod
        let typeStr = typeSigToStr resolveTypeRef varMap byRefDir typeSig
        match custModStr with
        | "System.Runtime.CompilerServices.IsVolatile" ->
            // TODO: check type is from system library
            "volatile " + typeStr
        | _ -> "__modreq(" + custModStr + ", " + typeStr + ")"
    | ModOpt(custMod, typeSig) ->
        let custModStr = typeRefToStr resolveTypeRef varMap None custMod
        let typeStr = typeSigToStr resolveTypeRef varMap byRefDir typeSig
        "__modopt(" + custModStr + ", " + typeStr + ")"
    | Pinned typeSig ->
        Diagnostics.Debugger.Break()
        raise(NotImplementedException()) // TODO
    | Class typeRef -> typeRefToStr resolveTypeRef varMap byRefDir typeRef
    | ValueType typeRef -> typeRefToStr resolveTypeRef varMap byRefDir typeRef

let enumUnderlyingType (typeDef : TypeDef) =
    match typeDef.baseType with
    | Some(TypeSpec.Choice1Of2 typeRef)
        // TODO: check that base type belongs to system library
        when (typeRef.typeNamespace, typeRef.typeName) = ("System", "Enum") -> ()
    | _ -> failwith "Not an enum type."
    let fld =
        typeDef.fields
        |> Seq.find (
            fun fld ->
                fld.name = "value__" && (fld.flags &&& FieldAttributes.SpecialName) = FieldAttributes.SpecialName
        )
    fld.typeSig
