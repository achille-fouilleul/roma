[<AutoOpen>]
module Roma.Cli.Compiler.TypeSupport

open System

open Roma.Cli
open Roma.Cli.Compiler

let rec private getModule typeRef =
    match typeRef with
    | TopLevelTypeRef(m, _, _) -> m
    | NestedTypeRef(typeRef', _) -> getModule typeRef'

let private getSysLib (m : Module) =
    m.Assembly.AssemblyResolver.SystemLibrary.ManifestModule

let private primNameKindMap =
    [
        "Boolean", Roma.Compiler.PrimitiveTypeKind.Bool
        "Char", Roma.Compiler.PrimitiveTypeKind.Char16
        "SByte", Roma.Compiler.PrimitiveTypeKind.SInt8
        "Int16", Roma.Compiler.PrimitiveTypeKind.SInt16
        "Int32", Roma.Compiler.PrimitiveTypeKind.SInt32
        "Int64",  Roma.Compiler.PrimitiveTypeKind.SInt64
        "IntPtr", Roma.Compiler.PrimitiveTypeKind.SIntPtr
        "Byte", Roma.Compiler.PrimitiveTypeKind.UInt8
        "UInt16", Roma.Compiler.PrimitiveTypeKind.UInt16
        "UInt32", Roma.Compiler.PrimitiveTypeKind.UInt32
        "UInt64", Roma.Compiler.PrimitiveTypeKind.UInt64
        "UIntPtr", Roma.Compiler.PrimitiveTypeKind.UIntPtr
        "Single", Roma.Compiler.PrimitiveTypeKind.Float32
        "Double", Roma.Compiler.PrimitiveTypeKind.Float64
    ]
    |> Map.ofSeq

let rec private toTypeDef (typeRef : TypeRef) : TypeDef =
    match typeRef with
    | TopLevelTypeRef(m, ns, name) ->
        Map.find (ns, name) m.TypeMap
    | NestedTypeRef(tr, name) ->
        (toTypeDef tr).nestedTypes
        |> Seq.find (fun td -> td.typeName = name)

let private isSysLib (m : Module) =
    m = getSysLib m

let isSysType fqn typeRef =
    match typeRef with
    | TopLevelTypeRef(m, ns, name) when (ns, name) = fqn && isSysLib m -> true
    | _ -> false

let (|SysTypeRef|_|) typeRef =
    match typeRef with
    | TopLevelTypeRef(m, ns, name) when isSysLib m -> Some(ns, name)
    | _ -> None

let private (|IsPrimitive|_|) typeRef =
    match typeRef with
    | TopLevelTypeRef(m, "System", name) when isSysLib m ->
        Map.tryFind name primNameKindMap
    | _ -> None

let rec private translateToTypeRef (m : Module) (typeRef : Roma.Cli.TypeRef) : TypeRef =
    let typeRef' =
        let ns, name = typeRef.typeNamespace, typeRef.typeName
        match typeRef.scope with
        | None ->
            TopLevelTypeRef(m, ns, name)
        | Some(TypeRefScope enclosingTypeRef) ->
            NestedTypeRef(translateToTypeRef m enclosingTypeRef, name)
        | Some(ModuleRefScope modName) ->
            TopLevelTypeRef(m.Assembly.GetModule(modName), ns, name)
        | Some(AssemblyRefScope asmName) ->
            TopLevelTypeRef(m.Assembly.AssemblyResolver.Load(asmName).ManifestModule, ns, name)
    canonicalize m typeRef'

and private canonicalize (m : Module) (typeRef : TypeRef) =
    match typeRef with
    | TopLevelTypeRef(m, ns, name) ->
        if Map.containsKey (ns, name) m.TypeMap then
            typeRef
        else
            let rec translate et =
                let ns, name = et.typeNamespace, et.typeName
                match et.implementation with
                | Implementation_File fileName ->
                    TopLevelTypeRef(m.Assembly.GetModule(fileName), ns, name)
                | Implementation_AssemblyRef asmRef ->
                    TopLevelTypeRef(m.Assembly.AssemblyResolver.Load(asmRef).ManifestModule, ns, name)
                | Implementation_ExportedType expType ->
                    NestedTypeRef(translate expType, name)
                |> canonicalize m
            m.ExportedTypeMap
            |> Map.find (ns, name)
            |> translate
    | NestedTypeRef(tr, name) ->
        let enclosingTypeRef = canonicalize m tr
        if obj.ReferenceEquals(enclosingTypeRef, tr) then
            typeRef
        else
            NestedTypeRef(enclosingTypeRef, name)

// underlying types of enum types
let private primSigKindMap =
    [
        TypeSig.Boolean, Roma.Compiler.PrimitiveTypeKind.Bool
        TypeSig.Char, Roma.Compiler.PrimitiveTypeKind.Char16
        TypeSig.I1, Roma.Compiler.PrimitiveTypeKind.SInt8
        TypeSig.U1, Roma.Compiler.PrimitiveTypeKind.UInt8
        TypeSig.I2, Roma.Compiler.PrimitiveTypeKind.SInt16
        TypeSig.U2, Roma.Compiler.PrimitiveTypeKind.UInt16
        TypeSig.I4, Roma.Compiler.PrimitiveTypeKind.SInt32
        TypeSig.U4, Roma.Compiler.PrimitiveTypeKind.UInt32
        TypeSig.I8,  Roma.Compiler.PrimitiveTypeKind.SInt64
        TypeSig.U8, Roma.Compiler.PrimitiveTypeKind.UInt64
        TypeSig.I, Roma.Compiler.PrimitiveTypeKind.SIntPtr
        TypeSig.U, Roma.Compiler.PrimitiveTypeKind.UIntPtr
    ]
    |> Map.ofSeq

let private isEnumTypeRef (typeRef : TypeRef) =
    let typeDef = toTypeDef typeRef
    match typeDef.baseType with
    | Some(Choice1Of2 typeRef') when (typeRef'.typeNamespace, typeRef'.typeName) = ("System", "Enum") ->
        translateToTypeRef (getModule typeRef) typeRef'
        |> isSysType("System", "Enum")
    | _ -> false

let private isValueTypeRef (typeRef : TypeRef) =
    match typeRef with
    | SysTypeRef("System", "Enum") -> false
    | _ ->
        let typeDef = toTypeDef typeRef
        match typeDef.baseType with
        | Some(Choice1Of2 baseTypeRef) ->
            translateToTypeRef (getModule typeRef) baseTypeRef
            |> isSysType("System", "ValueType")
        | _ -> false

let private (|IsEnum|_|) typeRef =
    if isEnumTypeRef typeRef then
        let typeDef = toTypeDef typeRef
        let fld =
            typeDef.fields
            |> Seq.find (fun fld -> (fld.flags &&& FieldAttributes.Static) <> FieldAttributes.Static)
        let kindOpt = Map.tryFind fld.typeSig primSigKindMap
        if Option.isNone kindOpt then
            failwith "Invalid underlying type."
        kindOpt
    else
        None

type CompositeTypeInfo with
    member this.Module = getModule this.typeRef
    member this.TypeDef = toTypeDef this.typeRef

    member this.Namespace = this.TypeDef.typeNamespace
    member this.Name = this.TypeDef.typeName

    member this.Methods =
        seq {
            for mthDef in this.TypeDef.methods ->
                {
                    Method.ownerType = this
                    Method.methodDef = mthDef
                }
        }

    member this.Fields =
        seq {
            for fldDef in this.TypeDef.fields ->
                {
                    Field.ownerType = this
                    Field.fieldDef = fldDef
                }
        }

    member private this.SysType(ns, name) : CompositeTypeInfo =
        {
            typeRef = TopLevelTypeRef(getSysLib this.Module, ns, name)
            genArgs = []
        }

    member this.BaseType : CompositeTypeInfo option =
        this.TypeDef.baseType
        |> Option.map (
            function
            | Choice1Of2 typeRef ->
                let info : CompositeTypeInfo =
                    {
                        typeRef = translateToTypeRef this.Module typeRef
                        genArgs = []
                    }
                info
            | Choice2Of2(GenericInst(TypeSig.Class typeRef, genArgs))
            | Choice2Of2(GenericInst(TypeSig.ValueType typeRef, genArgs)) ->
                let info : CompositeTypeInfo =
                    {
                        typeRef = translateToTypeRef this.Module typeRef
                        genArgs = [ for t in genArgs -> this.Module.TranslateToType(t, (this.genArgs, [])) ]
                    }
                info
            | _ -> failwith "Invalid TypeSpec."
        )

and TypeRef with
    member this.TypeDef =
        toTypeDef this

    member this.NestedTypes =
        seq {
            for nestedTypeDef in this.TypeDef.nestedTypes ->
                NestedTypeRef(this, nestedTypeDef.typeName)
        }

and TypeDef with
    member this.IsGeneric =
        not(Array.isEmpty this.genericParams)

    member this.IsExplicitLayout =
        (this.flags &&& TypeAttributes.LayoutMask) = TypeAttributes.ExplicitLayout

    member this.IsSequentialLayout =
        (this.flags &&& TypeAttributes.LayoutMask) = TypeAttributes.SequentialLayout

    member this.IsInterface =
        (this.flags &&& TypeAttributes.ClassSemanticsMask) = TypeAttributes.Interface

and Module with
    member this.Types =
        seq {
            for _, t in Map.toSeq this.TypeMap ->
                TopLevelTypeRef(this, t.typeNamespace, t.typeName)
        }

    member this.TranslateToType(typeSig : TypeSig, genArgs) =
        match typeSig with
        | Boolean -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.Bool
        | Char -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.Char16
        | I1 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.SInt8
        | U1 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.UInt8
        | I2 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.SInt16
        | U2 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.UInt16
        | I4 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.SInt32
        | U4 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.UInt32
        | I8 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.SInt64
        | U8 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.UInt64
        | R4 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.Float32
        | R8 -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.Float64
        | I -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.SIntPtr
        | U -> PrimitiveType Roma.Compiler.PrimitiveTypeKind.UIntPtr
        | Array(elemType, shape) -> ArrayType(this.TranslateToType(elemType, genArgs), Some shape)
        | ByRef typeSig -> ByRefType(this.TranslateToType(typeSig, genArgs))
        | Fnptr methodSig -> raise(NotImplementedException()) // TODO
        | GenericInst(genType, args) ->
            let args' =
                [
                    for arg in args ->
                        this.TranslateToType(arg, genArgs)
                ]
            let k (typeRef : Roma.Cli.TypeRef) =
                let info : CompositeTypeInfo =
                    {
                        typeRef = translateToTypeRef this typeRef
                        genArgs = args'
                    }
                CompositeType info
            match genType with
            | TypeSig.Class typeRef -> GCRefType(k typeRef) // TODO: reference type check
            | TypeSig.ValueType typeRef -> k typeRef // TODO: value type check
            | _ -> failwith "Bad GenericInst."
        | MVar n ->
            let _, methodArgs = genArgs
            List.nth methodArgs n
        | Object -> GCRefType(this.SysType("System", "String"))
        | Ptr typeSig -> PointerType(this.TranslateToType(typeSig, genArgs))
        | String -> GCRefType(this.SysType("System", "Object"))
        | SZArray elemType -> ArrayType(this.TranslateToType(elemType, genArgs), None)
        | TypedByRef -> this.SysType("System", "TypedReference")
        | Var n ->
            let typeArgs, _ = genArgs
            List.nth typeArgs n
        | TypeSig.Void -> VoidType
        | ModReq(modType, typeSig) ->
            let modRef = translateToTypeRef this modType
            match modRef with
            | SysTypeRef("System.Runtime.CompilerServices", "IsVolatile") ->
                VolatileModifier(this.TranslateToType(typeSig, genArgs))
            | _ -> failwith "Invalid modreq."
        | ModOpt _ -> raise(NotImplementedException()) // TODO
        | Pinned _ -> raise(NotImplementedException()) // TODO
        | TypeSig.Class typeRef ->
            // TODO: reference type check
            let info : CompositeTypeInfo =
                {
                    typeRef = translateToTypeRef this typeRef
                    genArgs = []
                }
            GCRefType(CompositeType info)
        | TypeSig.ValueType typeRef ->
            // TODO: value type check
            match translateToTypeRef this typeRef with
            | IsPrimitive kind -> PrimitiveType kind
            | IsEnum kind as typeRef -> EnumType(kind, typeRef)
            | typeRef' ->
                let info : CompositeTypeInfo =
                    {
                        typeRef = typeRef'
                        genArgs = []
                    }
                CompositeType info

    member private this.SysType(ns, name) =
        let info : CompositeTypeInfo =
            {
                typeRef = TopLevelTypeRef(getSysLib this, ns, name)
                genArgs = []
            }
        CompositeType(info)

    member this.IsSysLib = isSysLib this

type Method with
    member private this.TranslateType(typeSig) =
        let m = this.ownerType.Module
        m.TranslateToType(typeSig, (this.ownerType.genArgs, []))

    member this.IsGeneric =
        not(Array.isEmpty this.methodDef.genericParams)

    member this.ReturnType =
        this.TranslateType(this.methodDef.signature.retType)

    member this.ParamTypes =
        [|
            for typeSig in this.methodDef.signature.paramTypes ->
                this.TranslateType(typeSig)
        |]

type Field with
    member private this.TranslateType(typeSig) =
        let m = this.ownerType.Module
        m.TranslateToType(typeSig, (this.ownerType.genArgs, []))

    member this.IsStatic =
        (this.fieldDef.flags &&& FieldAttributes.Static) = FieldAttributes.Static

    member this.FieldType =
        this.TranslateType(this.fieldDef.typeSig)

