module Roma.Cli.Compiler.Main

open Roma.Cli
open Roma.Compiler
open Roma.Compiler.Dwarf

type private MutableMap<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type private MutableList<'t> = System.Collections.Generic.List<'t>

type TypeScope =
    {
        typeMap : MutableMap<string * string, AnyTypeNode>
        closedGenericTypeMap : MutableMap<string * string * (TypeEntry list), TypeNode>
    }

// non-generic types and closed generic types
and TypeNode =
    {
        scope : TypeScope // nested types
        typeDef : TypeDef
        genArgs : TypeEntry list
        typeEntry : TypeEntry
        fields : MutableList<FieldDef * Member>
        methods : MutableList<MethodDef * Subprogram>
        genMethods : MutableList<MethodDef>
    }

and OpenGenericTypeNode =
    {
        typeDef : TypeDef
        scope : TypeScope
    }

and EnumTypeNode =
    {
        typeDef : TypeDef
    }

and AnyTypeNode = Choice<TypeNode, OpenGenericTypeNode, EnumTypeNode>

let getNamespace fqn (ns : INamespace) =
    if System.String.IsNullOrEmpty(fqn) then
        ns
    else
        ns.GetNamespace(fqn)

let mkScope() : TypeScope =
    {
        typeMap = MutableMap<_, _>()
        closedGenericTypeMap = MutableMap<_, _>()
    }

let rec typeEntryDescription (entry : TypeEntry) =
    match box entry with
    | :? EnumTypeEntry as entry -> entry.Name
    | :? PrimitiveTypeEntry as entry -> entry.Name
    | :? ComplexTypeBase as entry -> entry.Name
    | :? ManagedPointerTypeEntry as entry ->
        typeEntryDescription entry.ReferencedType
    | :? ManagedArrayTypeEntry as entry ->
        typeEntryDescription entry.ElementType + "[]"
    | _ -> raise(System.NotImplementedException()) // TODO: arrays, etc.

let private createTypeEntry (scope : ITypeContainer) (typeDef : TypeDef) name (initType : ComplexTypeBase -> 't) addValues =
    if (typeDef.flags &&& TypeAttributes.ClassSemanticsMask) = TypeAttributes.Interface then
        scope.CreateInterfaceType(name) |> initType
    else
        // FIXME: check of base type is name-based
        if (typeDef.typeNamespace, typeDef.typeName) = ("System", "Enum") then
            scope.CreateClassType(name) |> initType
        else
            match typeDef.baseType with
            | Some(Choice1Of2 { typeNamespace = "System"; typeName = "Enum" }) ->
                scope.CreateEnumType(name) |> addValues
            | Some(Choice1Of2 { typeNamespace = "System"; typeName = "ValueType" }) ->
                scope.CreateStructType(name) |> initType
            | _ -> scope.CreateClassType(name) |> initType

let private addFields (fieldDefs : FieldDef list) addField (fields : MutableList<_>) =
    for fld in fieldDefs do
        fields.Add((fld, addField fld.name))

let private addMethods (methodDefs : MethodDef list) addMethod (methods : MutableList<_>, genMethods : MutableList<_>) =
    for mth in methodDefs do
        if mth.genericParams.Length <> 0 then
            genMethods.Add(mth)
        else
            let sub : Subprogram = addMethod mth.name
            methods.Add((mth, sub))
            if (mth.flags &&& MethodAttributes.Virtual) = MethodAttributes.Virtual then
                sub.SetVirtual((mth.flags &&& MethodAttributes.Abstract) = MethodAttributes.Abstract)

let compile (m : Module) (compileUnit : CompileUnit) =

    let rootTypeScope = mkScope()
    let rootVariables = MutableList<_>()
    let rootMethods = MutableList<_>()
    let rootGenMethods = MutableList<_>()

    for typeDef in m.typeDefs do
        let rec addType typeContainer (scope : TypeScope) (typeDef : TypeDef) =
            let initType (entry : ComplexTypeBase) =
                let scope' = mkScope()
                let node : TypeNode =
                    {
                        scope = scope'
                        typeDef = typeDef
                        genArgs = []
                        typeEntry = entry
                        fields = MutableList<_>()
                        methods = MutableList<_>()
                        genMethods = MutableList<_>()
                    }
                scope.typeMap.Add((typeDef.typeNamespace, typeDef.typeName), Choice1Of3 node)
                addFields typeDef.fields (fun name -> entry.AddMember(name)) node.fields
                addMethods typeDef.methods (fun name -> entry.AddSubprogram(name)) (node.methods, node.genMethods)
                for nestedType in typeDef.nestedTypes do
                    addType (entry :> ITypeContainer) scope' nestedType

            let addValues (entry : EnumTypeEntry) =
                let node : EnumTypeNode = { typeDef = typeDef }
                scope.typeMap.Add((typeDef.typeNamespace, typeDef.typeName), Choice3Of3 node)
                for fld in typeDef.fields do
                    if (fld.flags &&& FieldAttributes.Static) = FieldAttributes.Static then
                        match fld.constant with
                        | Some c ->
                            let value =
                                match c with
                                | ConstantBool false -> Int128.Zero
                                | ConstantBool true -> Int128.One
                                | ConstantChar x -> Int128(uint32 x)
                                | ConstantI1 x -> Int128(int32 x)
                                | ConstantU1 x -> Int128(uint32 x)
                                | ConstantI2 x -> Int128(int32 x)
                                | ConstantU2 x -> Int128(uint32 x)
                                | ConstantI4 x -> Int128(x)
                                | ConstantU4 x -> Int128(x)
                                | ConstantI8 x -> Int128(x)
                                | ConstantU8 x -> Int128(x)
                                | _ -> failwith "Invalid constant type for enum."
                            entry.AddValue(fld.name, value)
                        | None -> () // TODO: emit diagnostic
                    else
                        // TODO: check that there is exactly one such field
                        let kind =
                            match fld.typeSig with
                            | Boolean -> PrimitiveTypeKind.Bool
                            | Char -> PrimitiveTypeKind.Char16
                            | I1 -> PrimitiveTypeKind.SInt8
                            | U1 -> PrimitiveTypeKind.UInt8
                            | I2 -> PrimitiveTypeKind.SInt16
                            | U2 -> PrimitiveTypeKind.UInt16
                            | I4 -> PrimitiveTypeKind.SInt32
                            | U4 -> PrimitiveTypeKind.UInt32
                            | I8 -> PrimitiveTypeKind.SInt64
                            | U8 -> PrimitiveTypeKind.UInt64
                            | I -> PrimitiveTypeKind.SIntPtr
                            | U -> PrimitiveTypeKind.UIntPtr
                            | _ -> failwith "Invalid underlying type for enum."
                        entry.SetUnderlyingType(compileUnit.GetPrimitiveType(kind))

            if typeDef.genericParams.Length <> 0 then
                let rec addGenType scope typeDef =
                    let scope' = mkScope()
                    let node : OpenGenericTypeNode =
                        {
                            scope = scope'
                            typeDef = typeDef
                        }
                    scope.typeMap.Add((typeDef.typeNamespace, typeDef.typeName), Choice2Of3 node)
                    for nestedType in typeDef.nestedTypes do
                        addGenType scope' nestedType
                addGenType scope typeDef
            else
                createTypeEntry typeContainer typeDef typeDef.typeName initType addValues

        let ns = getNamespace typeDef.typeNamespace compileUnit
        addType ns rootTypeScope typeDef

    addFields m.fields (fun name -> compileUnit.AddVariable(name)) rootVariables
    addMethods m.methods (fun name -> compileUnit.AddSubprogram(name)) (rootMethods, rootGenMethods)

    let prim kind =
        Some(compileUnit.GetPrimitiveType(kind) :> TypeEntry)

    let getSystemType ns name =
        // TODO: handle cases where current module is not the system library
        (compileUnit :> INamespace).GetNamespace(ns).FindType(name)

    let primTypes =
        [
            "Boolean", PrimitiveTypeKind.Bool
            "Char", PrimitiveTypeKind.Char16
            "Single", PrimitiveTypeKind.Float32
            "Double", PrimitiveTypeKind.Float64
            "SByte", PrimitiveTypeKind.SInt8
            "Int16", PrimitiveTypeKind.SInt16
            "Int32", PrimitiveTypeKind.SInt32
            "Int64", PrimitiveTypeKind.SInt64
            "IntPtr", PrimitiveTypeKind.SIntPtr
            "Byte", PrimitiveTypeKind.UInt8
            "UInt16", PrimitiveTypeKind.UInt16
            "UInt32", PrimitiveTypeKind.UInt32
            "UInt64", PrimitiveTypeKind.UInt64
            "UIntPtr", PrimitiveTypeKind.UIntPtr
        ]
        |> List.map (fun (name, kind) -> (getSystemType "System" name, compileUnit.GetPrimitiveType(kind)))

    let rec resolveTypeRef (typeRef : TypeRef) =
        match typeRef.scope with
        | None ->
            let ns = getNamespace typeRef.typeNamespace compileUnit
            ns.FindType(typeRef.typeName)
        | Some(TypeRefScope(Choice1Of2 enclosingType)) ->
            let typeEntry = resolveTypeRef enclosingType
            (box typeEntry :?> ITypeContainer).FindType(typeRef.typeName)
        | Some _ -> raise(System.NotImplementedException()) // TODO

    let rec canonicalize =
        function
        | None -> failwith "Invalid generic argument."
        | Some(entry : TypeEntry) ->
            match box entry with
            | :? PrimitiveTypeEntry
            | :? ClassTypeEntry
            | :? InterfaceTypeEntry
            | :? EnumTypeEntry -> entry
            | :? StructTypeEntry as structType ->
                match List.tryPick (fun (e, primEntry) -> if e = entry then Some primEntry else None) primTypes with
                | Some primEntry -> primEntry :> TypeEntry
                | None -> entry
            | :? ManagedPointerTypeEntry as ptrType ->
                compileUnit.GetManagedPointerType(canonicalize(Some ptrType.ReferencedType)) :> TypeEntry
            | :? ManagedArrayTypeEntry as arrayType ->
                compileUnit.GetManagedArrayType(canonicalize(Some arrayType.ElementType)) :> TypeEntry
            | _ ->
                // TODO: remove modopts, modreqs, etc.
                raise(System.NotImplementedException()) // TODO

    let newClosedGenericTypes = MutableList<_>()

    let resolveTypeSig typeSig (vars, mvars) =
        let rec visitTypeSig =
            function
            | TypeSig.Boolean -> prim PrimitiveTypeKind.Bool
            | TypeSig.Char -> prim PrimitiveTypeKind.Char16
            | TypeSig.I1 -> prim PrimitiveTypeKind.SInt8
            | TypeSig.U1 -> prim PrimitiveTypeKind.UInt8
            | TypeSig.I2 -> prim PrimitiveTypeKind.SInt16
            | TypeSig.U2 -> prim PrimitiveTypeKind.UInt16
            | TypeSig.I4 -> prim PrimitiveTypeKind.SInt32
            | TypeSig.U4 -> prim PrimitiveTypeKind.UInt32
            | TypeSig.I8 -> prim PrimitiveTypeKind.SInt64
            | TypeSig.U8 -> prim PrimitiveTypeKind.UInt64
            | TypeSig.R4 -> prim PrimitiveTypeKind.Float32
            | TypeSig.R8 -> prim PrimitiveTypeKind.Float64
            | TypeSig.I -> prim PrimitiveTypeKind.SIntPtr
            | TypeSig.U -> prim PrimitiveTypeKind.UIntPtr
            | TypeSig.Array(elemType, shape) ->
                match visitTypeSig elemType with
                | None -> failwith "Element type of managed array cannot be void."
                | Some t -> Some(compileUnit.GetManagedArrayType(t, shape) :> TypeEntry)
            | TypeSig.ByRef typeSig -> Some(compileUnit.GetReferenceType(visitTypeSig typeSig) :> TypeEntry)
            | TypeSig.Fnptr methodSig ->
                let retType = visitTypeSig methodSig.retType
                let paramTypes = Array.map visitTypeSig methodSig.paramTypes
                let subType = compileUnit.GetSubroutineType(retType, paramTypes) :> TypeEntry
                Some(compileUnit.GetPointerType(Some subType) :> TypeEntry)
            | TypeSig.GenericInst(genType, args) ->
                match genType with
                | TypeSig.Class typeRef ->
                    // TODO: check that typeRef references a reference type
                    let tmp = genericTypeInstance typeRef args
                    Some(compileUnit.GetManagedPointerType(tmp) :> TypeEntry)
                | TypeSig.ValueType typeRef ->
                    // TODO: check that typeRef references a value type
                    Some(genericTypeInstance typeRef args)
                | _ -> failwith "Invalid generic inst."
            | TypeSig.MVar n -> Some(List.nth mvars n)
            | TypeSig.Object -> Some(getSystemType "System" "Object")
            | TypeSig.Ptr typeSig ->
                Some(compileUnit.GetPointerType(visitTypeSig typeSig) :> TypeEntry)
            | TypeSig.String -> Some(getSystemType "System" "String")
            | TypeSig.SZArray elemType ->
                match visitTypeSig elemType with
                | None -> failwith "Element type of managed array cannot be void."
                | Some t -> Some(compileUnit.GetManagedArrayType(t) :> TypeEntry)
            | TypeSig.TypedByRef -> Some(getSystemType "System" "TypedReference")
            | TypeSig.Var n -> Some(List.nth vars n)
            | TypeSig.Void -> None
            | TypeSig.ModReq(modType, typeSig) ->
                // FIXME: name-based type check
                match (modType.typeNamespace, modType.typeName) with
                | ("System.Runtime.CompilerServices", "IsVolatile") ->
                    match visitTypeSig typeSig with
                    | None -> failwith "Volatile modifier cannot be applied to void type."
                    | Some t -> Some(compileUnit.GetVolatileType(t) :> TypeEntry)
                | _ -> raise(System.NotImplementedException()) // TODO
            | TypeSig.ModOpt(modType, typeSig) -> raise(System.NotImplementedException()) // TODO
            | TypeSig.Pinned typeSig -> raise(System.NotImplementedException()) // TODO
            | TypeSig.Class typeRef ->
                // TODO: check that typeRef references a reference type
                let tmp = resolveTypeRef typeRef
                Some(compileUnit.GetManagedPointerType(tmp) :> TypeEntry)
            | TypeSig.ValueType typeRef ->
                // TODO: check that typeRef references a value type
                Some(resolveTypeRef typeRef)

        and genericTypeInstance typeRef genArgSigs =
            let genArgs = List.map (visitTypeSig >> canonicalize) genArgSigs
            let inst (scope : TypeScope) (typeContainer : ITypeContainer) fqn =
                match scope.typeMap.[fqn] with
                | Choice1Of3 node -> node
                | Choice2Of3 node ->
                    let typeDef = node.typeDef
                    let args =
                        genArgs
                        |> Seq.take typeDef.genericParams.Length
                        |> Seq.toList
                    let key = (typeDef.typeNamespace, typeDef.typeName, args)
                    match scope.closedGenericTypeMap.TryGetValue(key) with
                    | true, node -> node
                    | _ ->
                        let entry =
                            let argNames = Seq.map typeEntryDescription args
                            let name = typeRef.typeName + "<" + (String.concat ", " argNames) + ">"
                            createTypeEntry typeContainer typeDef name id (fun _ -> failwith "Enums cannot be generic.")
                        for p, a in Seq.zip typeDef.genericParams args do
                            entry.AddTypeParameter(p.name, a)
                        let node : TypeNode =
                            {
                                scope = node.scope
                                typeDef = typeDef
                                genArgs = args
                                typeEntry = entry
                                fields = MutableList<_>()
                                methods = MutableList<_>()
                                genMethods = MutableList<_>()
                            }
                        scope.closedGenericTypeMap.Add(key, node)
                        newClosedGenericTypes.Add((key, node))
                        node
                | Choice3Of3 _ -> failwith "Enum type cannot contain other types."
            let rec visitTypeRef (typeRef : TypeRef) =
                let fqn = (typeRef.typeNamespace, typeRef.typeName)
                match typeRef.scope with
                | None ->
                    let typeContainer = getNamespace typeRef.typeNamespace compileUnit
                    inst rootTypeScope typeContainer fqn
                | Some(TypeRefScope(Choice1Of2 enclosingTypeRef)) ->
                    let enclosingNode = visitTypeRef enclosingTypeRef
                    let typeContainer = box enclosingNode.typeEntry :?> ITypeContainer
                    inst enclosingNode.scope typeContainer fqn
                | _ -> raise(System.NotImplementedException()) // TODO
            let node = visitTypeRef typeRef
            node.typeEntry

        visitTypeSig typeSig

    // set types of members
    let rec visitScope (scope : TypeScope) =
        for kvp in scope.typeMap do
            match kvp.Value with
            | Choice1Of3 node -> visitNode node
            | Choice2Of3 _ -> () // skip open generic type
            | Choice3Of3 _ -> () // skip enum types

    and visitNode node =
        let genArgs = (node.genArgs, [])

        for size in Option.toArray node.typeDef.classSize do
            if size <> 0 then
                node.typeEntry.SetByteSize(Checked.uint32 size)

        node.typeDef.packingSize // TODO
        |> ignore

        // resolve base type
        match box node.typeEntry with
        | :? ComplexTypeBase as t ->
            let inheritFrom typeSpec =
                let entry =
                    match typeSpec with
                    | Choice1Of2 typeRef -> resolveTypeRef typeRef
                    | Choice2Of2 typeSig ->
                        match resolveTypeSig typeSig genArgs with
                        | None ->
                            // cannot derive from void
                            failwith "Invalid base type."
                        | Some typeEntry ->
                            match box typeEntry with
                            | :? ManagedPointerTypeEntry as ptrType -> ptrType :> TypeEntry
                            | _ -> failwith "Invalid base type."
                t.Inherit(entry)
            for typeSpec in Option.toArray node.typeDef.baseType do
                inheritFrom typeSpec
            for typeSpec in node.typeDef.interfaces do
                inheritFrom typeSpec
        | _ -> ()

        // fields
        for fld, mem in node.fields do
            match resolveTypeSig fld.typeSig genArgs with
            | None -> failwith "Field type cannot be void."
            | Some typeEntry -> mem.SetType(typeEntry)

        // methods
        for mth, sub in node.methods do
            sub.SetReturnType(resolveTypeSig mth.signature.retType genArgs)
            if mth.signature.callConv.hasThis then
                let paramType =
                    match box node.typeEntry with
                    | :? StructTypeEntry ->
                        compileUnit.GetReferenceType(Some node.typeEntry) :> TypeEntry
                    | _ ->
                        compileUnit.GetManagedPointerType(node.typeEntry) :> TypeEntry
                let param = sub.AddParameter(paramType, None)
                param.SetArtificial()
                sub.SetObjectPointer(param)
            for paramDefOpt, typeSig in Seq.zip mth.parameters mth.signature.paramTypes do
                match resolveTypeSig typeSig genArgs with
                | None -> failwith "Parameter type cannot be void."
                | Some paramType ->
                    let nameOpt = paramDefOpt |> Option.map (fun paramDef -> paramDef.name)
                    sub.AddParameter(paramType, nameOpt)
                    |> ignore
            if mth.signature.callConv.callKind = CallKind.Vararg then
                sub.AddUnspecifiedParameters()

        visitScope node.scope

    visitScope rootTypeScope

    while newClosedGenericTypes.Count <> 0 do
        let kvs = newClosedGenericTypes.ToArray()
        eprintfn "%d" kvs.Length
        newClosedGenericTypes.Clear()
        for _, node in kvs do
            visitNode node

    // TODO

