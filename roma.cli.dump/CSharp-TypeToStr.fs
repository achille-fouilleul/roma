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

let rec typeSpecToStr varMap byRefDir typeSpec =
    match typeSpec with
    | TypeSpec.Choice1Of2 typeRef -> typeRefToStr varMap byRefDir typeRef
    | TypeSpec.Choice2Of2 typeSig -> typeSigToStr varMap byRefDir typeSig

and typeRefToStr varMap byRefDir typeRef =
    match typeRef.scope with
    | Some(TypeRefScope enclosingTypeRef) ->
        (typeSpecToStr varMap byRefDir enclosingTypeRef) + "." + typeRef.typeName
    | _ ->
        if typeRef.typeNamespace.Length <> 0 then
            typeRef.typeNamespace + "." + typeRef.typeName
        else
            typeRef.typeName

and typeSigToStr (varMap : GenVarMap) byRefDir typeSig =
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
            failwith "unsupported array shape"
        typeSigToStr varMap byRefDir typeSig + "[" + (String.replicate (List.length dims - 1) ",") + "]"
    | ByRef typeSig ->
        let typeStr = typeSigToStr varMap None typeSig
        match byRefDir with
        | None -> "ref " + typeStr // TODO: warn
        | Some OutOnly -> "out " + typeStr
        | Some InOut -> "ref " + typeStr
    | Fnptr methodSig ->
        let retTypeStr = typeSigToStr varMap None methodSig.retType
        let paramTypes = Seq.map (typeSigToStr varMap None) methodSig.paramTypes
        "__fnptr(" + retTypeStr + "(" + String.concat ", " paramTypes + ")" // TODO: calling convention
    | GenericInst(typeSig, args) ->
        let name = typeSigToStr varMap byRefDir typeSig
        let expectedSuffix = "`" + intToStr(List.length args)
        let shortName =
            if name.EndsWith(expectedSuffix) then
                name.[0 .. name.Length - expectedSuffix.Length - 1]
            else
                // TODO: warn about missing suffix
                name
        shortName + "<" + (args |> Seq.map (typeSigToStr varMap byRefDir) |> String.concat ", ") + ">"
    | MVar n -> varMap.MVar(n)
    | Object -> "object"
    | Ptr typeSig -> typeSigToStr varMap byRefDir typeSig + "*"
    | String -> "string"
    | SZArray typeSig -> typeSigToStr varMap byRefDir typeSig + "[]"
    | TypedByRef -> "System.TypedReference"
    | Var n -> varMap.Var(n)
    | Void -> "void"
    | ModReq(custMod, typeSig) ->
        let custModStr = typeRefToStr varMap None custMod
        let typeStr = typeSigToStr varMap byRefDir typeSig
        match custModStr with
        | "System.Runtime.CompilerServices.IsVolatile" -> "volatile " + typeStr
        | _ -> "__modreq(" + custModStr + ", " + typeStr + ")"
    | ModOpt(custMod, typeSig) ->
        let custModStr = typeRefToStr varMap None custMod
        let typeStr = typeSigToStr varMap byRefDir typeSig
        "__modopt(" + custModStr + ", " + typeStr + ")"
    // | Pinned of TypeSig -> TODO
    | Class typeRef -> typeRefToStr varMap byRefDir typeRef
    | ValueType typeRef -> typeRefToStr varMap byRefDir typeRef
    | _ ->
        Diagnostics.Debugger.Break()
        raise(NotImplementedException()) // TODO

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
