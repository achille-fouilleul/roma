﻿open System

open Roma.Cli

[<Literal>]
let private Indent = " "

type private Writer() =
    let mutable stack = [ "" ]

    member this.Print(s) =
        Console.WriteLine(List.head stack + s)

    member this.Print(name, value) =
        this.Print(name + ": " + value)

    member this.Enter(name : string) =
        let prefix = List.head stack
        this.Print(name + " {")
        stack <- (prefix + Indent) :: stack

    member this.Leave() =
        stack <- List.tail stack
        this.Print("}")

let private intToStr (n : int) =
    n.ToString(System.Globalization.CultureInfo.InvariantCulture)

let private arrayDimsToStr dims =
    dims
    |> Seq.map (
        function
        | 0, None -> ""
        | n, None -> intToStr n + "..."
        | 0, Some m -> intToStr m
        | n, Some m -> intToStr n + "..." + intToStr (n + m - 1)
    )
    |> String.concat ","

let rec private typeSpecToStr typeSpec =
    match typeSpec with
    | TypeSpec.Choice1Of2 typeRef ->
        typeRefToStr typeRef
    | TypeSpec.Choice2Of2 typeSig ->
        match typeSig with
        | GenericInst(typeSig, args) ->
            genericInstToStr(typeSig, args)
        | _ -> failwith "Invalid TypeSpec."

and typeRefToStr typeRef =
    let name =
        match typeRef.typeNamespace with
        | "" -> typeRef.typeName
        | _ -> typeRef.typeNamespace + "." + typeRef.typeName
    match typeRef.scope with
    | Some scope -> "[" + resolutionScopeToStr scope + "]" + name
    | None -> name

and resolutionScopeToStr scope =
    match scope with
    | TypeRefScope typeSpec -> typeSpecToStr typeSpec
    | ModuleRefScope moduleRef -> "." + moduleRef
    | AssemblyRefScope assemblyRef -> assemblyRef.Name // TODO: include details

and typeSigToStr typeSig =
    match typeSig with
    | Boolean -> "bool"
    | Char -> "char"
    | I1 -> "int8"
    | U1 -> "uint8"
    | I2 -> "int16"
    | U2 -> "uint16"
    | I4 -> "int32"
    | U4 -> "uint32"
    | I8 -> "int64"
    | U8 -> "uint64"
    | R4 -> "float32"
    | R8 -> "float64"
    | I -> "intptr"
    | U -> "uintptr"
    | Array(typeSig, dims) -> "[" + arrayDimsToStr dims + "]" + typeSigToStr typeSig
    | ByRef typeSig -> "&" + typeSigToStr typeSig
    | Fnptr methodSig -> methodSigToStr methodSig
    | GenericInst(typeSig, args) -> genericInstToStr(typeSig, args)
    | MVar n -> "!!" + intToStr n
    | Object -> "object"
    | Ptr typeSig -> "*" + typeSigToStr typeSig
    | String -> "string"
    | SZArray typeSig -> "[]" + typeSigToStr typeSig
    | TypedByRef -> "typedref"
    | Var n -> "!" + intToStr n
    | Void -> "void"
    | ModReq(typeRef, typeSig) -> "modreq(" + typeRefToStr typeRef + ") " + typeSigToStr typeSig
    | ModOpt(typeRef, typeSig) -> "modopt(" + typeRefToStr typeRef + ") " + typeSigToStr typeSig
    | Pinned typeSig -> "pinned " + typeSigToStr typeSig
    | Class typeRef
    | ValueType typeRef -> typeRefToStr typeRef

and methodSigToStr methodSig =
    // TODO
    typeSigToStr methodSig.retType + "(" + ( methodSig.paramTypes |> Seq.map typeSigToStr |> String.concat ", ") + ")"

and genericInstToStr(typeSig, args) =
    match typeSig with
    | Class(typeRef)
    | ValueType(typeRef) ->
        typeRefToStr typeRef + "<" + (args |> Seq.map typeSigToStr |> String.concat ", ") + ">"
    | _ -> failwith "Invalid GenericInst."

let private dumpCustomAttr (w : Writer) (ca : CustomAttribute) =
    w.Enter("CustomAttribute")
    match ca.methodRef.typeRef with
    | None -> ()
    | Some typeRef ->
        w.Print("type", typeSpecToStr typeRef)
    // TODO
    w.Print("value", BitConverter.ToString(ca.value))
    w.Leave()

let private dumpCustomAttrs (w : Writer) (cas : seq<CustomAttribute>) =
    for ca in cas do
        dumpCustomAttr w ca

let private dumpField (w : Writer) (fld : FieldDef) =
    w.Enter("Field")
    w.Print("name", fld.name)
    w.Print("type", typeSigToStr fld.typeSig)
    dumpCustomAttrs w fld.customAttrs
    // TODO
    w.Leave()

let private dumpMethod (w : Writer) (mth : MethodDef) =
    w.Enter("Method")
    w.Print("name", mth.name)
    w.Print("signature", methodSigToStr mth.signature)
    dumpCustomAttrs w mth.customAttrs
    // TODO
    w.Leave()

let private dumpProperty (w : Writer) (prop : Property) =
    w.Enter("Property")
    w.Print("name", prop.name)
    dumpCustomAttrs w prop.customAttrs
    // TODO
    w.Leave()

let private dumpEvent (w : Writer) (evt : Event) =
    w.Enter("Event")
    w.Print("name", evt.name)
    dumpCustomAttrs w evt.customAttrs
    // TODO
    w.Leave()

let rec private dumpTypeDef (w : Writer) (typeDef : TypeDef) =
    w.Enter("TypeDef")
    w.Print("name", typeDef.typeNamespace + "." + typeDef.typeName)
    dumpCustomAttrs w typeDef.customAttrs
    match typeDef.baseType with
    | None -> ()
    | Some baseType -> w.Print("baseType", typeSpecToStr baseType)
    for gp in typeDef.genericParams do
        w.Enter("GenericParam")
        w.Print("name", gp.name)
        // TODO
        w.Leave()
    for nestedType in typeDef.nestedTypes do
        dumpTypeDef w nestedType
    for fld in typeDef.fields do
        dumpField w fld
    for mth in typeDef.methods do
        dumpMethod w mth
    for prop in typeDef.properties do
        dumpProperty w prop
    for evt in typeDef.events do
        dumpEvent w evt
    // TODO
    w.Leave()

[<EntryPoint>]
let main (args : string[]) =
    let w = Writer()
    let m = ModuleLoading.loadModule args.[0]
    w.Enter("Module")
    w.Print("mvid", m.moduleGuid.ToString("B"))
    w.Print("name", m.moduleName)
    match m.assembly with
    | None -> ()
    | Some assembly ->
        w.Enter("Assembly")
        w.Print("name", assembly.name)
        w.Print("version", assembly.version.ToString())
        // TODO
        w.Leave()
    for assemblyRef in m.assemblyRefs do
        w.Enter("AssemblyRef")
        w.Print("name", assemblyRef.name)
        w.Print("version", assemblyRef.version.ToString())
        // TODO
        w.Leave()
    dumpCustomAttrs w m.customAttrs
    for typeDef in m.typeDefs do
        dumpTypeDef w typeDef
    for fld in m.fields do
        dumpField w fld
    for mth in m.methods do
        dumpMethod w mth
    // TODO
    w.Leave()
    0