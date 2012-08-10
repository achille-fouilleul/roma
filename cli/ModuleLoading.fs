module Roma.Cli.ModuleLoading

open System
open SignatureDecoding

let getListBounds (table1 : 'a[]) idx (table2 : 'b[]) getIndex =
    let row = table1.[int idx - 1]
    let listId0 = getIndex row
    let listId1 =
        if int idx < table1.Length then
            getIndex table1.[int idx]
        else
            uint32(table2.Length + 1)
    (row, listId0, listId1)

let private getChildIndex (childTable : 'a[]) getParentIndex parentIndex =
    let rec search lo hi =
        if hi < lo then
            None
        else
            let pos = lo + (hi - lo) / 2u
            let row = childTable.[int pos]
            let idx = getParentIndex row
            if parentIndex < idx then
                if pos > lo then
                    search lo (pos - 1u)
                else
                    None
            elif parentIndex > idx then
                if pos < hi then
                    search (pos + 1u) hi
                else
                    None
            else
                Some pos
    if childTable.Length >= 1 then
        search 0u (uint32 childTable.Length - 1u)
    else
        None

let private getChild childTable getParentIndex parentIndex =
    getChildIndex childTable getParentIndex parentIndex
    |> Option.map (fun i -> childTable.[int i])

let private getChildrenIndices childTable getParentIndex parentIndex =
    getChildIndex childTable getParentIndex parentIndex
    |> Option.map (
        fun i ->
            let mutable b = i
            let mutable e = i
            while b >= 1u && getParentIndex childTable.[int b - 1] = parentIndex do
                b <- b - 1u
            while e + 1u <= uint32(childTable.Length - 1) && getParentIndex childTable.[int e + 1] = parentIndex do
                e <- e + 1u
            (b, e)
    )

let private getChildren childTable getParentIndex parentIndex =
    match getChildrenIndices childTable getParentIndex parentIndex with
    | None -> List.empty
    | Some(b, e) -> [ for i in b .. e -> childTable.[int i] ]

let private getParent (parentTable : 'a[]) (childTable : 'b[]) getChildIndex childIndex =
    let rec search lo hi =
        if hi < lo then
            failwith "Parent not found."
        else
            let pos = lo + (hi - lo) / 2u
            let idx0 = getChildIndex parentTable.[int pos]
            let idx1 =
                if pos + 1u < uint32 parentTable.Length then
                    getChildIndex parentTable.[int pos + 1]
                else
                    uint32 childTable.Length + 1u // TODO: check correctness
            if childIndex < idx0 then
                search lo (pos - 1u)
            elif childIndex >= idx1 then
                search (pos + 1u) hi
            else
                1u + pos

    search 0u (uint32 parentTable.Length - 1u)

let private isNested (row : TypeDefRow) =
    match row.Flags &&& TypeAttributes.VisibilityMask with
    | TypeAttributes.NestedPublic
    | TypeAttributes.NestedPrivate
    | TypeAttributes.NestedFamily
    | TypeAttributes.NestedAssembly
    | TypeAttributes.NestedFamANDAssem
    | TypeAttributes.NestedFamORAssem -> true
    | _ -> false

let private enumerate i0 xs =
    seq {
        let iRef = ref i0
        for x in xs do
            let i = !iRef
            yield (i, x)
            iRef := i + 1u
    }

type private ModuleLoader(pe : PEImageReader) =
    let entryPointToken = Tables.tokenOptOfValue (Option.get pe.CliHeader).entryPointToken
    let md = MetadataReader(pe)
    let il = CILReader(pe)

    let moduleTable = md.ModuleTable
    let typeRefTable = md.TypeRefTable
    let typeDefTable = md.TypeDefTable
    let fieldTable = md.FieldTable
    let methodDefTable = md.MethodDefTable
    let paramTable = md.ParamTable
    let interfaceImplTable = md.InterfaceImplTable
    let memberRefTable = md.MemberRefTable
    let constantTable = md.ConstantTable
    let customAttributeTable = md.CustomAttributeTable
    let fieldMarshalTable = md.FieldMarshalTable
    let declSecurityTable = md.DeclSecurityTable
    let classLayoutTable = md.ClassLayoutTable
    let fieldLayoutTable = md.FieldLayoutTable
    let standAloneSigTable = md.StandAloneSigTable
    let eventMapTable = md.EventMapTable
    let eventTable = md.EventTable
    let propertyMapTable = md.PropertyMapTable
    let propertyTable = md.PropertyTable
    let methodSemanticsTable = md.MethodSemanticsTable
    let methodImplTable = md.MethodImplTable
    let moduleRefTable = md.ModuleRefTable
    let typeSpecTable = md.TypeSpecTable
    let implMapTable = md.ImplMapTable
    let fieldRVATable = md.FieldRVATable
    let assemblyTable = md.AssemblyTable
    let assemblyProcessorTable = md.AssemblyProcessorTable
    let assemblyOSTable = md.AssemblyOSTable
    let assemblyRefTable = md.AssemblyRefTable
    let assemblyRefProcessorTable = md.AssemblyRefProcessorTable
    let assemblyRefOSTable = md.AssemblyRefOSTable
    let fileTable = md.FileTable
    let exportedTypeTable = md.ExportedTypeTable
    let manifestResourceTable = md.ManifestResourceTable
    let nestedClassTable = md.NestedClassTable
    let genericParamTable = md.GenericParamTable
    let methodSpecTable = md.MethodSpecTable
    let genericParamConstraintTable = md.GenericParamConstraintTable

    let eventMap =
        seq {
            for eventMapId in 1u .. uint32 eventMapTable.Length do
                let row, eventId0, eventId1 = getListBounds eventMapTable eventMapId eventTable (fun row -> row.EventList)
                if eventId0 <> eventId1 then
                    yield (row.Parent, (eventId0, eventId1))
        }
        |> Map.ofSeq

    let propertyMap =
        seq {
            for propertyMapId in 1u .. uint32 propertyMapTable.Length do
                let row, propertyId0, propertyId1 = getListBounds propertyMapTable propertyMapId propertyTable (fun row -> row.PropertyList)
                if propertyId0 <> propertyId1 then
                    yield (row.Parent, (propertyId0, propertyId1))
        }
        |> Map.ofSeq

    let mutable methodRefMap = Map.empty

    member this.LoadModuleTable() =
        match moduleTable with
        | [| row |] -> row.Name, row.Mvid
        | _ -> failwith "invalid Module table"

    member this.LoadModuleRefTable() =
        [ for row in moduleRefTable -> row.Name ]

    member this.LoadAssemblyTable() =
        match assemblyTable with
        | [| |] -> None
        | [| row |] ->
            let token = (TableNumber.Assembly, 1u)
            let asm : Assembly = {
                hashAlgId = row.HashAlgId
                version = row.MajorVersion, row.MinorVersion, row.BuildNumber, row.RevisionNumber
                flags = row.Flags
                publicKey = row.PublicKey
                name = row.Name
                culture = row.Culture
                declSec = this.LoadDeclSecurity(token)
                customAttrs = this.LoadCustomAttributes(token)
            }
            Some asm
        | _ -> failwith "invalid Assembly table"

    member this.LoadDeclSecurity(token) =
        let codedIndex = CodedIndexes.hasDeclSecurity.Encode(token)
        let rows = getChildren declSecurityTable (fun row -> row.Parent) codedIndex
        [
            for row in rows ->
                {
                    action = row.Action
                    permissionSet = row.PermissionSet
                }
        ]

    member this.LoadCustomAttributes(token) =
        let codedIndex = CodedIndexes.hasCustomAttribute.Encode(token)
        let rows = getChildren customAttributeTable (fun row -> row.Parent) codedIndex
        [
            for row in rows ->
                let token = CodedIndexes.customAttributeType.Decode(row.Type)
                {
                    methodRef = this.LoadMethodRef(token)
                    value = row.Value
                }
        ]

    member this.LoadTypeRefOptFromTypeDefIndex(index : uint32) =
        if index <> 1u then
            let row = typeDefTable.[int index - 1]
            let scope =
                if isNested row then
                    match getChild nestedClassTable (fun row -> row.NestedClass) index with
                    | None -> failwith "Enclosing type not found."
                    | Some row ->
                        Some(TypeResolutionScope_TypeRef(this.LoadTypeRefFromTypeDefIndex(row.EnclosingClass)))
                else
                    None
            let typeRef : TypeRef = {
                    scope = scope
                    typeNamespace = row.TypeNamespace
                    typeName = row.TypeName
                }
            Some(TypeSpec_Plain typeRef)
        else
            None

    member this.LoadMethodRefFromMethodDefIndex(index) =
        let ownerIdx = getParent typeDefTable methodDefTable (fun row -> row.MethodList) index
        let owner = this.LoadTypeRefOptFromTypeDefIndex(ownerIdx)
        let methodDefRow = methodDefTable.[int index - 1]
        (owner, methodDefRow.Name, methodDefRow.Signature)

    member this.LoadMethodRefFromMemberRefIndex(index) =
        raise(NotImplementedException()) // TODO

    member this.LoadTypeRefFromTypeDefIndex(index) =
        match this.LoadTypeRefOptFromTypeDefIndex(index) with
        | None -> failwith "invalid TypeRef"
        | Some typeRef -> typeRef

    member this.LoadTypeRefFromIndex(idx) =
        let row = typeRefTable.[int idx - 1]
        let scope =
            match CodedIndexes.resolutionScope.DecodeOpt(row.ResolutionScope) with
            | None ->
                // TODO: look up this type in ExportedType table;
                // Implementation must be (File _) or (AssemblyRef _)
                raise (new System.NotImplementedException())
            | Some(TableNumber.TypeRef, idx) ->
                Some(TypeResolutionScope_TypeRef(this.LoadTypeRefFromIndex(idx)))
            | Some(TableNumber.ModuleRef, idx) ->
                Some(TypeResolutionScope_ModuleRef(moduleRefTable.[int idx - 1].Name))
            | Some(TableNumber.Module, idx) ->
                if idx <> 1u then
                    failwith "Invalid module index."
                None
            | Some(TableNumber.AssemblyRef, idx) ->
                Some(TypeResolutionScope_AssemblyRef(assemblyRefTable.[int idx - 1]))
            | _ -> failwith "Invalid ResolutionScope token."
        let typeRef : TypeRef =
            {
                scope = scope
                typeNamespace = row.TypeNamespace
                typeName = row.TypeName
            }
        TypeSpec_Plain typeRef

    member this.LoadTypeRefFromTypeSpecIndex(index) =
        let row = typeSpecTable.[int index - 1]
        let typeSig = decodeTypeSig this row.Signature (ref 0)
        match typeSig with
        | GenericInst(typeSpec, args) ->
            match typeSpec with
            | TypeSig.Class(TypeSpec_Plain typeRef)
            | TypeSig.ValueType(TypeSpec_Plain typeRef) ->
                TypeSpec_GenericInst(typeRef, args)
            | _ -> failwith "invalid TypeRef"
        | _ -> failwith "invalid TypeRef"

    member this.LoadInterfaces(typeDefId) =
        getChildren interfaceImplTable (fun row -> row.Class) typeDefId
        |> List.map begin
            fun row ->
                let token = CodedIndexes.typeDefOrRef.Decode(row.Interface)
                this.LoadTypeRef(token)
        end

    member this.LoadPInvokeImpl(token) =
        let codedIndex = CodedIndexes.memberForwarded.Encode(token)
        getChild implMapTable (fun row -> row.MemberForwarded) codedIndex 
        |> Option.map (
            fun row ->
                let importScope = moduleRefTable.[int row.ImportScope - 1].Name
                (row.MappingFlags, row.ImportName, importScope)
        )

    member this.LoadMarshal(token) =
        let codedIndex = CodedIndexes.hasFieldMarshal.Encode(token)
        getChild fieldMarshalTable (fun row -> row.Parent) codedIndex
        |> Option.map (fun row -> decodeMarshal row.NativeType)

    member this.LoadConstant(token) =
        let codedIndex = CodedIndexes.hasConstant.Encode(token)
        getChild constantTable (fun row -> row.Parent) codedIndex
        |> Option.map (fun row -> decodeConstant row.Type row.Value)


    member this.LoadSemantics(token) =
        let codedIndex = CodedIndexes.hasSemantics.Encode(token)
        getChildren methodSemanticsTable (fun row -> row.Association) codedIndex

    member this.LoadMembers(typeDefId) =
        let (_, methodDefId0, methodDefId1) = getListBounds typeDefTable typeDefId methodDefTable (fun row -> row.MethodList)
        let (_, fieldId0, fieldId1) = getListBounds typeDefTable typeDefId fieldTable (fun row -> row.FieldList)

        let methods =
            if methodDefId0 <> methodDefId1 then
                [
                    for methodDefId in methodDefId0 .. (methodDefId1 - 1u) ->
                        let row, paramId0, paramId1 = getListBounds methodDefTable methodDefId paramTable (fun row -> row.ParamList)
                        let row = methodDefTable.[int methodDefId - 1]
                        let token = (TableNumber.MethodDef, methodDefId)
                        let signature = decodeMethodSig this row.Signature (ref 0)
                        let (retVal, parameters) = this.LoadParams(signature, paramId0, paramId1)
                        let body =
                            if row.Rva <> 0u then
                                let codeType = row.ImplFlags &&& MethodImplAttributes.CodeTypeMask
                                let managed = row.ImplFlags &&& MethodImplAttributes.ManagedMask
                                if codeType = MethodImplAttributes.Native || managed = MethodImplAttributes.Unmanaged then
                                    Diagnostics.Debug.WriteLine(sprintf "TODO: unmanaged/native code method: %s" row.Name)
                                    None
                                else
                                    Some(il.ReadMethodBody(row.Rva))
                            else
                                None
                        let methodDef : MethodDef = {
                            name = row.Name
                            implFlags = row.ImplFlags
                            flags = row.Flags
                            genericParams = this.LoadGenericParams(token)
                            isEntryPoint = (entryPointToken = Some token)
                            signature = signature
                            pinvokeimpl = this.LoadPInvokeImpl(token)
                            retVal = retVal
                            parameters = parameters
                            body = body
                            declSec = this.LoadDeclSecurity(token)
                            customAttrs = this.LoadCustomAttributes(token)
                        }
                        methodDef
                ]
            else
                []

        let fields =
            if fieldId0 <> fieldId1 then
                [
                    for fieldId in fieldId0 .. (fieldId1 - 1u) ->
                        let token = (TableNumber.Field, fieldId)
                        let row = fieldTable.[int fieldId - 1]
                        let fieldDef : FieldDef = {
                            name = row.Name
                            flags = row.Flags
                            typeSig = decodeFieldSig this row.Signature

                            offset =
                                getChild fieldLayoutTable (fun row -> row.Field) fieldId
                                |> Option.map (fun row -> row.Offset)

                            marshal = this.LoadMarshal(token)

                            rva =
                                getChild fieldRVATable (fun row -> row.Field) fieldId
                                |> Option.map (fun row -> row.Rva)

                            constant = this.LoadConstant(token)

                            customAttrs = this.LoadCustomAttributes(token)
                        }
                        fieldDef
                ]
            else
                []

        (methods, fields)

    member this.LoadParams(signature : MethodSig, paramId0, paramId1) =
        let retVal : ParamDef option ref = ref None
        let parameters : ParamDef option[] = Array.create signature.paramTypes.Length None
        if paramId0 <> paramId1 then
            for paramId in paramId0 .. (paramId1 - 1u) do
                let token = (TableNumber.Param, paramId)
                let row = paramTable.[int paramId - 1]
                let paramDef : ParamDef = {
                    name = row.Name
                    flags = row.Flags
                    marshal = this.LoadMarshal(token)
                    constant = this.LoadConstant(token)
                    customAttrs = this.LoadCustomAttributes(token)
                }
                if row.Sequence = 0us then
                    retVal := Some paramDef
                else
                    parameters.[int row.Sequence - 1] <- Some paramDef
        (!retVal, parameters)

    member this.LoadMethodRefs(semRows) =
        [
            for semRow in semRows ->
                (semRow.Semantics, this.LoadMethodRef((TableNumber.MethodDef, semRow.Method)))
        ]

    member this.LoadTypeDefs() =
        [
            for idx in 1u .. uint32 typeDefTable.Length do
                if idx <> 1u then
                    let row = typeDefTable.[int idx - 1]
                    if not(isNested row) then
                        yield this.LoadTypeDef(idx)
        ]

    member this.LoadTypeDef(index) =
        let row = typeDefTable.[int index - 1]
        let token = (TableNumber.TypeDef, index)
        let (methods, fields) = this.LoadMembers(index)
        let layout = getChild classLayoutTable (fun row -> row.Parent) index

        let properties =
            match Map.tryFind index propertyMap with
            | None -> []
            | Some(propertyId0, propertyId1) ->
                [
                    for propertyId in propertyId0 .. propertyId1 - 1u ->
                        let propRow = propertyTable.[int propertyId - 1]
                        let token = (TableNumber.Property, propertyId)
                        let semRows = this.LoadSemantics(token)
                        let hasThis, retType, paramTypes = decodePropertySig this propRow.Type
                        let prop : Property = {
                            flags = propRow.Flags
                            name = propRow.Name
                            hasThis = hasThis
                            retType = retType
                            paramTypes = paramTypes
                            methods = this.LoadMethodRefs(semRows)
                            customAttrs = this.LoadCustomAttributes(token)
                        }
                        prop
                ]

        let events =
            match Map.tryFind index eventMap with
            | None -> []
            | Some(eventId0, eventId1) ->
                [
                    for eventId in eventId0 .. eventId1 - 1u ->
                        let evtRow = eventTable.[int eventId - 1]
                        let token = (TableNumber.Event, eventId)
                        let semRows = this.LoadSemantics(token)
                        let evtType = Option.map this.LoadTypeRef (CodedIndexes.typeDefOrRef.DecodeOpt(evtRow.EventType))
                        let evt : Event = {
                            flags = evtRow.EventFlags
                            name = evtRow.Name
                            typeRef = evtType
                            methods = this.LoadMethodRefs(semRows)
                            customAttrs = this.LoadCustomAttributes(token)
                        }
                        evt
                ]

        let typeDef : TypeDef = {
            typeName = row.TypeName
            typeNamespace = row.TypeNamespace
            flags = row.Flags

            packingSize = Option.map (fun row -> int row.PackingSize) layout

            classSize = Option.map (fun row -> int row.ClassSize) layout

            genericParams = this.LoadGenericParams(token)

            baseType = CodedIndexes.typeDefOrRef.DecodeOpt(row.Extends) |> Option.map this.LoadTypeRef

            interfaces = this.LoadInterfaces(index)

            nestedTypes =
                [
                    for t in nestedClassTable do
                        if t.EnclosingClass = index then
                            yield this.LoadTypeDef(t.NestedClass)
                ]

            methods = methods
            fields = fields

            overrides =
                [
                    for row in getChildren methodImplTable (fun row -> row.Class) index ->
                        let decl = CodedIndexes.methodDefOrRef.Decode(row.MethodDeclaration)
                        let body = CodedIndexes.methodDefOrRef.Decode(row.MethodBody)
                        (this.LoadMethodRef(decl), this.LoadMethodRef(body))
                ]

            properties = properties
            events = events

            declSec = this.LoadDeclSecurity(token)
            customAttrs = this.LoadCustomAttributes(token)
        }
        typeDef

    member this.LoadTypeRef(token) =
        // TODO: memoize
        match token with
        | (TableNumber.TypeDef, index) -> this.LoadTypeRefFromTypeDefIndex(index)
        | (TableNumber.TypeRef, index) -> this.LoadTypeRefFromIndex(index)
        | (TableNumber.TypeSpec, index) -> this.LoadTypeRefFromTypeSpecIndex(index)
        | _ -> failwith "invalid TypeRef token"

    member this.LoadMethodRef(token) =
        // TODO: memoize
        let owner, name, signature =
            match token with
            | (TableNumber.MethodDef, index) -> this.LoadMethodRefFromMethodDefIndex(index)
            | (TableNumber.MemberRef, index) -> this.LoadMemberRef(index)
            | _ -> raise(NotImplementedException(sprintf "%A" token)) // TODO
        let methodRef : MethodRef = {
            typeRef = owner
            methodName = name
            signature = decodeMethodSig this signature (ref 0)
        }
        methodRef

    member this.LoadMemberRef(index) =
        let row = memberRefTable.[int index - 1]
        let owner =
            match CodedIndexes.memberRefParent.Decode(row.Class) with
            | (TableNumber.TypeRef, idx) -> Some(this.LoadTypeRefFromIndex(idx))
            | (TableNumber.ModuleRef, idx) -> raise(new System.NotImplementedException())
            | (TableNumber.MethodDef, idx) ->
                // vararg
                let owner, name, signature = this.LoadMethodDefFromIndex(idx)
                // TODO: check name & signature compatibility
                owner
            | (TableNumber.TypeSpec, idx) -> Some(this.LoadTypeRefFromTypeSpecIndex(idx))
            | _ -> failwith "Invalid MemberRef.Class token."
        (owner, row.Name, row.Signature)

    member this.LoadMethodDefFromIndex(index) =
        let ownerIdx = getParent typeDefTable methodDefTable (fun row -> row.MethodList) index
        let owner = this.LoadTypeRefFromTypeDefIndex(ownerIdx)
        let methodDefRow = methodDefTable.[int index - 1]
        (Some owner, methodDefRow.Name, methodDefRow.Signature)

    member this.LoadManifestResourceTable() =
        [
            for index, row in enumerate 1u manifestResourceTable ->
                let token = (TableNumber.ManifestResource, index)
                let impl =
                    match CodedIndexes.implementation.DecodeOpt(row.Implementation) with
                    | None -> None
                    | Some(TableNumber.File, i) -> Some(Implementation_File fileTable.[int i - 1].Name)
                    | Some(TableNumber.AssemblyRef, i) -> Some(Implementation_AssemblyRef assemblyRefTable.[int i - 1])
                    | _ -> failwith "Invalid ManifestResource.Implementation."
                let mresource : ManifestResource = {
                    name = row.Name
                    flags = row.Flags
                    implementation = impl
                    offset = row.Offset
                    customAttrs = this.LoadCustomAttributes(token)
                }
                mresource
        ]

    member this.LoadAssemblyRefTable() =
        [
            for index, row in enumerate 1u assemblyRefTable ->
                let asmRef : AssemblyRef = {
                    version = row.MajorVersion, row.MinorVersion, row.BuildNumber, row.RevisionNumber
                    flags = row.Flags
                    publicKeyOrToken = row.PublicKeyOrToken
                    name = row.Name
                    culture = row.Culture
                    hashValue = row.HashValue
                    customAttrs = this.LoadCustomAttributes((TableNumber.AssemblyRef, index))
                }
                asmRef
        ]

    member this.LoadFileTable() =
        [
            for index, row in enumerate 1u fileTable ->
                let token = (TableNumber.File, index)
                let fileRef : FileRef = {
                    name = row.Name
                    isEntryPoint = entryPointToken = Some token
                    containsMetadata =
                        match row.Flags with
                        | FileAttributes.ContainsMetaData -> true
                        | FileAttributes.ContainsNoMetaData -> false
                        | _ -> failwith "Invalid file attributes."
                    hash = row.HashValue
                    customAttrs = this.LoadCustomAttributes(token)
                }
                fileRef
        ]

    member this.LoadGenericParams(token) =
        let codedIndex = CodedIndexes.typeOrMethodDef.Encode(token)
        match getChildrenIndices genericParamTable (fun row -> row.Owner) codedIndex with
        | None -> Array.empty
        | Some(gpi0, gpi1) ->
            [|
                for gpi in gpi0 .. gpi1 do
                    let gpRow = genericParamTable.[int gpi]
                    if gpi <> gpi0 + uint32 gpRow.Number then
                        failwith "Wrong GenericParam number."
                    let gp : GenericParam = {
                        name = gpRow.Name
                        flags = gpRow.Flags
                        constraints =
                            [
                                for row in getChildren genericParamConstraintTable (fun row -> row.Owner) gpi ->
                                    this.LoadTypeRef(CodedIndexes.typeDefOrRef.Decode(row.Constraint))
                            ]
                        customAttrs = this.LoadCustomAttributes((TableNumber.GenericParam, gpi + 1u))
                    }
                    yield gp
            |]

    interface IModuleReader with
        member this.GetTypeRef(token) =
            this.LoadTypeRef(token)

let loadModule (path : string) : Module =
    let mr = ModuleLoader(PEImageReader(path))
    let moduleName, moduleGuid = mr.LoadModuleTable()
    let methods, fields = mr.LoadMembers(1u)

    {
        moduleName = moduleName
        moduleGuid = moduleGuid

        moduleRefs = mr.LoadModuleRefTable()
        assembly = mr.LoadAssemblyTable()
        mresources = mr.LoadManifestResourceTable()
        assemblyRefs = mr.LoadAssemblyRefTable()
        fileRefs = mr.LoadFileTable()
        methods = methods
        fields = fields
        typeDefs = mr.LoadTypeDefs()
        customAttrs = mr.LoadCustomAttributes((TableNumber.Module, 1u))
    }
