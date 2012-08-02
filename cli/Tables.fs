namespace Roma.Cli

open LanguagePrimitives

type Guid = System.Guid

type TableNumber =
    | Module = 0x00uy
    | TypeRef = 0x01uy
    | TypeDef = 0x02uy
    | Field = 0x04uy
    | MethodDef = 0x06uy
    | Param = 0x08uy
    | InterfaceImpl = 0x09uy
    | MemberRef = 0x0auy
    | Constant = 0x0buy
    | CustomAttribute = 0x0cuy
    | FieldMarshal = 0x0duy
    | DeclSecurity = 0x0euy
    | ClassLayout = 0x0fuy
    | FieldLayout = 0x10uy
    | StandAloneSig = 0x11uy
    | EventMap = 0x12uy
    | Event = 0x14uy
    | PropertyMap = 0x15uy
    | Property = 0x17uy
    | MethodSemantics = 0x18uy
    | MethodImpl = 0x19uy
    | ModuleRef = 0x1auy
    | TypeSpec = 0x1buy
    | ImplMap = 0x1cuy
    | FieldRVA = 0x1duy
    | Assembly = 0x20uy
    | AssemblyProcessor = 0x21uy
    | AssemblyOS = 0x22uy
    | AssemblyRef = 0x23uy
    | AssemblyRefProcessor = 0x24uy
    | AssemblyRefOS = 0x25uy
    | File = 0x26uy
    | ExportedType = 0x27uy
    | ManifestResource = 0x28uy
    | NestedClass = 0x29uy
    | GenericParam = 0x2auy
    | MethodSpec = 0x2buy
    | GenericParamConstraint = 0x2cuy

type SimpleIndexAttribute(tableNumber) =
    inherit System.Attribute()
    member this.TableNumber = tableNumber

type CodedIndex(nTagBits : int, defList : (TableNumber * int) list) =
    let tagMask = (1 <<< nTagBits) - 1

    let decMap = defList |> List.map (fun (x, y) -> (y, x)) |> Map.ofList

    let encMap = defList |> Map.ofList

    member this.NTagBits = nTagBits

    member this.TableNumbers = seq { for (table, tag) in defList -> table }

    member this.Decode(index) =
        if index = 0 then
            None
        else
            let table = decMap.[index &&& tagMask]
            let idx = index >>> nTagBits
            Some (table, idx)

    member this.Encode(token) =
        match token with
        | Some (table, idx) -> (idx <<< nTagBits) ||| encMap.[table]
        | None -> 0

    static member Create nTagBits defList = CodedIndex(nTagBits, defList)

[<AbstractClass>]
type CodedIndexAttribute() =
    inherit System.Attribute()
    abstract CodedIndex : CodedIndex with get

type TypeDefOrRefCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 2 [
            TableNumber.TypeDef, 0
            TableNumber.TypeRef, 1
            TableNumber.TypeSpec, 2
        ]

    override this.CodedIndex with get() = codedIndex

type HasConstantCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 2 [
            TableNumber.Field, 0
            TableNumber.Param, 1
            TableNumber.Property, 2
        ]

    override this.CodedIndex with get() = codedIndex

type HasCustomAttributeCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 5 [
            TableNumber.MethodDef, 0
            TableNumber.Field, 1
            TableNumber.TypeRef, 2
            TableNumber.TypeDef, 3
            TableNumber.Param, 4
            TableNumber.InterfaceImpl, 5
            TableNumber.MemberRef, 6
            TableNumber.Module, 7
            TableNumber.DeclSecurity, 8
            TableNumber.Property, 9
            TableNumber.Event, 10
            TableNumber.StandAloneSig, 11
            TableNumber.ModuleRef, 12
            TableNumber.TypeSpec, 13
            TableNumber.Assembly, 14
            TableNumber.AssemblyRef, 15
            TableNumber.File, 16
            TableNumber.ExportedType, 17
            TableNumber.ManifestResource, 18
            TableNumber.GenericParam, 19
            TableNumber.GenericParamConstraint, 20
            TableNumber.MethodSpec, 21
        ]

    override this.CodedIndex with get() = codedIndex

type HasFieldMarshalCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 1 [
            TableNumber.Field, 0
            TableNumber.Param, 1
        ]

    override this.CodedIndex with get() = codedIndex

type HasDeclSecurityCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 2 [
            TableNumber.TypeDef, 0
            TableNumber.MethodDef, 1
            TableNumber.Assembly, 2
        ]

    override this.CodedIndex with get() = codedIndex

type MemberRefParentCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 3 [
            TableNumber.TypeDef, 0
            TableNumber.TypeRef, 1
            TableNumber.ModuleRef, 2
            TableNumber.MethodDef, 3
            TableNumber.TypeSpec, 4
        ]

    override this.CodedIndex with get() = codedIndex

type HasSemanticsCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 1 [
            TableNumber.Event, 0
            TableNumber.Property, 1
        ]

    override this.CodedIndex with get() = codedIndex

type MethodDefOrRefCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 1 [
            TableNumber.MethodDef, 0
            TableNumber.MemberRef, 1
        ]

    override this.CodedIndex with get() = codedIndex

type MemberForwardedCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 1 [
            TableNumber.Field, 0
            TableNumber.MethodDef, 1
        ]

    override this.CodedIndex with get() = codedIndex

type ImplementationCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 2 [
            TableNumber.File, 0
            TableNumber.AssemblyRef, 1
            TableNumber.ExportedType, 2
        ]

    override this.CodedIndex with get() = codedIndex

type CustomAttributeTypeCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 3 [
            TableNumber.MethodDef, 2
            TableNumber.MemberRef, 3
        ]

    override this.CodedIndex with get() = codedIndex

type ResolutionScopeCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 2 [
            TableNumber.Module, 0
            TableNumber.ModuleRef, 1
            TableNumber.AssemblyRef, 2
            TableNumber.TypeRef, 3
        ]

    override this.CodedIndex with get() = codedIndex

type TypeOrMethodDefCodedIndexAttribute() =
    inherit CodedIndexAttribute()

    static let codedIndex =
        CodedIndex.Create 1 [
            TableNumber.TypeDef, 0
            TableNumber.MethodDef, 1
        ]

    override this.CodedIndex with get() = codedIndex

type Token = TableNumber * int

type ModuleRow =
    {
        Generation : uint16
        Name : string
        Mvid : Guid
        EncId : Guid
        EncBaseId : Guid
    }

    static member MakeRecord(objs : obj[]) =
        let row : ModuleRow = {
            Generation = objs.[0] :?> uint16
            Name = objs.[1] :?> string
            Mvid = objs.[2] :?> Guid
            EncId = objs.[3] :?> Guid
            EncBaseId = objs.[4] :?> Guid
        }
        row :> obj

type TypeRefRow =
    {
        [<ResolutionScopeCodedIndex()>]
        ResolutionScope : uint32
        TypeName : string
        TypeNamespace : string
    }

    static member MakeRecord(objs : obj[]) =
        let row : TypeRefRow = {
            ResolutionScope = objs.[0] :?> uint32
            TypeName = objs.[1] :?> string
            TypeNamespace = objs.[2] :?> string
        }
        row :> obj

type TypeDefRow =
    {
        Flags : TypeAttributes
        TypeName : string
        TypeNamespace : string
        [<TypeDefOrRefCodedIndex>]
        Extends : uint32
        [<SimpleIndex(TableNumber.Field)>]
        FieldList : uint32
        [<SimpleIndex(TableNumber.MethodDef)>]
        MethodList : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : TypeDefRow = {
            Flags = objs.[0] :?> TypeAttributes
            TypeName = objs.[1] :?> string
            TypeNamespace = objs.[2] :?> string
            Extends = objs.[3] :?> uint32
            FieldList = objs.[4] :?> uint32
            MethodList = objs.[5] :?> uint32
        }
        row :> obj

type FieldRow =
    {
        Flags : FieldAttributes
        Name : string
        Signature : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : FieldRow = {
            Flags = objs.[0] :?> FieldAttributes
            Name = objs.[1] :?> string
            Signature = objs.[2] :?> byte[]
        }
        row :> obj

type MethodDefRow =
    {
        Rva : uint32
        ImplFlags : MethodImplAttributes
        Flags : MethodAttributes
        Name : string
        Signature : byte[]
        [<SimpleIndex(TableNumber.Param)>]
        ParamList : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : MethodDefRow = {
            Rva = objs.[0] :?> uint32
            ImplFlags = objs.[1] :?> MethodImplAttributes
            Flags = objs.[2] :?> MethodAttributes
            Name = objs.[3] :?> string
            Signature = objs.[4] :?> byte[]
            ParamList = objs.[5] :?> uint32
        }
        row :> obj

type ParamRow =
    {
        Flags : ParamAttributes
        Sequence : uint16
        Name : string
    }

    static member MakeRecord(objs : obj[]) =
        let row : ParamRow = {
            Flags = objs.[0] :?> ParamAttributes
            Sequence = objs.[1] :?> uint16
            Name = objs.[2] :?> string
        }
        row :> obj

type InterfaceImplRow =
    {
        [<SimpleIndex(TableNumber.TypeDef)>]
        Class : uint32
        [<TypeDefOrRefCodedIndex>]
        Interface : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : InterfaceImplRow = {
            Class = objs.[0] :?> uint32
            Interface = objs.[1] :?> uint32
        }
        row :> obj

type MemberRefRow =
    {
        [<MemberRefParentCodedIndex>]
        Class : uint32
        Name : string
        Signature : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : MemberRefRow = {
            Class = objs.[0] :?> uint32
            Name = objs.[1] :?> string
            Signature = objs.[2] :?> byte[]
        }
        row :> obj

type ConstantRow =
    {
        Type : byte
        [<HasConstantCodedIndex>]
        Parent : uint32
        Value : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : ConstantRow = {
            Type = objs.[0] :?> byte
            Parent = objs.[1] :?> uint32
            Value = objs.[2] :?> byte[]
        }
        row :> obj

type CustomAttributeRow =
    {
        [<HasCustomAttributeCodedIndex>]
        Parent : uint32
        [<CustomAttributeTypeCodedIndex>]
        Type : uint32
        Value : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : CustomAttributeRow = {
            Parent = objs.[0] :?> uint32
            Type = objs.[1] :?> uint32
            Value = objs.[2] :?> byte[]
        }
        row :> obj

type FieldMarshalRow =
    {
        [<HasFieldMarshalCodedIndex>]
        Parent : uint32
        NativeType : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : FieldMarshalRow = {
            Parent = objs.[0] :?> uint32
            NativeType = objs.[1] :?> byte[]
        }
        row :> obj

type DeclSecurityRow =
    {
        Action : uint16
        [<HasDeclSecurityCodedIndex>]
        Parent : uint32
        PermissionSet : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : DeclSecurityRow = {
            Action = objs.[0] :?> uint16
            Parent = objs.[1] :?> uint32
            PermissionSet = objs.[2] :?> byte[]
        }
        row :> obj

type ClassLayoutRow =
    {
        PackingSize : uint16
        ClassSize : uint32
        [<SimpleIndex(TableNumber.TypeDef)>]
        Parent : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : ClassLayoutRow = {
            PackingSize = objs.[0] :?> uint16
            ClassSize = objs.[1] :?> uint32
            Parent = objs.[2] :?> uint32
        }
        row :> obj

type FieldLayoutRow =
    {
        Offset : uint32
        [<SimpleIndex(TableNumber.Field)>]
        Field : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : FieldLayoutRow = {
            Offset = objs.[0] :?> uint32
            Field = objs.[1] :?> uint32
        }
        row :> obj

type StandAloneSigRow =
    {
        Signature : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : StandAloneSigRow = {
            Signature = objs.[0] :?> byte[]
        }
        row :> obj

type EventMapRow =
    {
        [<SimpleIndex(TableNumber.TypeDef)>]
        Parent : uint32
        [<SimpleIndex(TableNumber.Event)>]
        EventList : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : EventMapRow = {
            Parent = objs.[0] :?> uint32
            EventList = objs.[1] :?> uint32
        }
        row :> obj

type EventRow =
    {
        EventFlags : EventAttributes
        Name : string
        [<TypeDefOrRefCodedIndex>]
        EventType : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : EventRow = {
            EventFlags = objs.[0] :?> EventAttributes
            Name = objs.[1] :?> string
            EventType = objs.[2] :?> uint32
        }
        row :> obj

type PropertyMapRow =
    {
        [<SimpleIndex(TableNumber.TypeDef)>]
        Parent : uint32
        [<SimpleIndex(TableNumber.Property)>]
        PropertyList : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : PropertyMapRow = {
            Parent = objs.[0] :?> uint32
            PropertyList = objs.[1] :?> uint32
        }
        row :> obj

type PropertyRow =
    {
        Flags : PropertyAttributes
        Name : string
        Type : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : PropertyRow = {
            Flags = objs.[0] :?> PropertyAttributes
            Name = objs.[1] :?> string
            Type = objs.[2] :?> byte[]
        }
        row :> obj

type MethodSemanticsRow =
    {
        Semantics : MethodSemanticsAttribute
        [<SimpleIndex(TableNumber.MethodDef)>]
        Method : uint32
        [<HasSemanticsCodedIndex>]
        Association : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : MethodSemanticsRow = {
            Semantics = objs.[0] :?> MethodSemanticsAttribute
            Method = objs.[1] :?> uint32
            Association = objs.[2] :?> uint32
        }
        row :> obj

type MethodImplRow =
    {
        [<SimpleIndex(TableNumber.TypeDef)>]
        Class : uint32
        [<MethodDefOrRefCodedIndex>]
        MethodBody : uint32
        [<MethodDefOrRefCodedIndex>]
        MethodDeclaration : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : MethodImplRow = {
            Class = objs.[0] :?> uint32
            MethodBody = objs.[1] :?> uint32
            MethodDeclaration = objs.[2] :?> uint32
        }
        row :> obj

type ModuleRefRow =
    {
        Name : string
    }

    static member MakeRecord(objs : obj[]) =
        let row : ModuleRefRow = {
            Name = objs.[0] :?> string
        }
        row :> obj

type TypeSpecRow =
    {
        Signature : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : TypeSpecRow = {
            Signature = objs.[0] :?> byte[]
        }
        row :> obj

type ImplMapRow =
    {
        MappingFlags : PInvokeAttributes
        [<MemberForwardedCodedIndex>]
        MemberForwarded : uint32
        ImportName : string
        [<SimpleIndex(TableNumber.ModuleRef)>]
        ImportScope : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : ImplMapRow = {
            MappingFlags = objs.[0] :?> PInvokeAttributes
            MemberForwarded = objs.[1] :?> uint32
            ImportName = objs.[2] :?> string
            ImportScope = objs.[3] :?> uint32
        }
        row :> obj

type FieldRVARow =
    {
        Rva : uint32
        [<SimpleIndex(TableNumber.Field)>]
        Field : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : FieldRVARow = {
            Rva = objs.[0] :?> uint32
            Field = objs.[1] :?> uint32
        }
        row :> obj

type AssemblyRow =
    {
        HashAlgId : AssemblyHashAlgorithm
        MajorVersion : uint16
        MinorVersion : uint16
        BuildNumber : uint16
        RevisionNumber : uint16
        Flags : AssemblyFlags
        PublicKey : byte[]
        Name : string
        Culture : string
    }

    static member MakeRecord(objs : obj[]) =
        let row : AssemblyRow = {
            HashAlgId = objs.[0] :?> AssemblyHashAlgorithm
            MajorVersion = objs.[1] :?> uint16
            MinorVersion = objs.[2] :?> uint16
            BuildNumber = objs.[3] :?> uint16
            RevisionNumber = objs.[4] :?> uint16
            Flags = objs.[5] :?> AssemblyFlags
            PublicKey = objs.[6] :?> byte[]
            Name = objs.[7] :?> string
            Culture = objs.[8] :?> string
        }
        row :> obj

type AssemblyProcessorRow =
    {
        Processor : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : AssemblyProcessorRow = {
            Processor = objs.[0] :?> uint32
        }
        row :> obj

type AssemblyOSRow =
    {
        OSPlatformId : uint32
        OSMajorVersion : uint32
        OSMinorVersion : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : AssemblyOSRow = {
            OSPlatformId = objs.[0] :?> uint32
            OSMajorVersion = objs.[1] :?> uint32
            OSMinorVersion = objs.[2] :?> uint32
        }
        row :> obj

type AssemblyRefRow =
    {
        MajorVersion : uint16
        MinorVersion : uint16
        BuildNumber : uint16
        RevisionNumber : uint16
        Flags : AssemblyFlags
        PublicKeyOrToken : byte[]
        Name : string
        Culture : string
        HashValue : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : AssemblyRefRow = {
            MajorVersion = objs.[0] :?> uint16
            MinorVersion = objs.[1] :?> uint16
            BuildNumber = objs.[2] :?> uint16
            RevisionNumber = objs.[3] :?> uint16
            Flags = objs.[4] :?> AssemblyFlags
            PublicKeyOrToken = objs.[5] :?> byte[]
            Name = objs.[6] :?> string
            Culture = objs.[7] :?> string
            HashValue = objs.[8] :?> byte[]
        }
        row :> obj

type AssemblyRefProcessorRow =
    {
        Processor : uint32
        [<SimpleIndex(TableNumber.AssemblyRef)>]
        AssemblyRef : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : AssemblyRefProcessorRow = {
            Processor = objs.[0] :?> uint32
            AssemblyRef = objs.[1] :?> uint32
        }
        row :> obj

type AssemblyRefOSRow =
    {
        OSPlatformId : uint32
        OSMajorVersion : uint32
        OSMinorVersion : uint32
        [<SimpleIndex(TableNumber.AssemblyRef)>]
        AssemblyRef : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : AssemblyRefOSRow = {
            OSPlatformId = objs.[0] :?> uint32
            OSMajorVersion = objs.[1] :?> uint32
            OSMinorVersion = objs.[2] :?> uint32
            AssemblyRef = objs.[3] :?> uint32
        }
        row :> obj

type FileRow =
    {
        Flags : FileAttributes
        Name : string
        HashValue : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : FileRow = {
            Flags = objs.[0] :?> FileAttributes
            Name = objs.[1] :?> string
            HashValue = objs.[2] :?> byte[]
        }
        row :> obj

type ExportedTypeRow =
    {
        Flags : TypeAttributes
        TypeDefId : uint32
        TypeName : string
        TypeNamespace : string
        [<ImplementationCodedIndex>]
        Implementation : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : ExportedTypeRow = {
            Flags = objs.[0] :?> TypeAttributes
            TypeDefId = objs.[1] :?> uint32
            TypeName = objs.[2] :?> string
            TypeNamespace = objs.[3] :?> string
            Implementation = objs.[4] :?> uint32
        }
        row :> obj

type ManifestResourceRow =
    {
        Offset : uint32
        Flags : ManifestResourceAttributes
        Name : string
        [<ImplementationCodedIndex>]
        Implementation : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : ManifestResourceRow = {
            Offset = objs.[0] :?> uint32
            Flags = objs.[1] :?> ManifestResourceAttributes
            Name = objs.[2] :?> string
            Implementation = objs.[3] :?> uint32
        }
        row :> obj

type NestedClassRow =
    {
        [<SimpleIndex(TableNumber.TypeDef)>]
        NestedClass : uint32
        [<SimpleIndex(TableNumber.TypeDef)>]
        EnclosingClass : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : NestedClassRow = {
            NestedClass = objs.[0] :?> uint32
            EnclosingClass = objs.[1] :?> uint32
        }
        row :> obj

type GenericParamRow =
    {
        Number : uint16
        Flags : GenericParamAttributes
        [<TypeOrMethodDefCodedIndex>]
        Owner : uint32
        Name : string
    }

    static member MakeRecord(objs : obj[]) =
        let row : GenericParamRow = {
            Number = objs.[0] :?> uint16
            Flags = objs.[1] :?> GenericParamAttributes
            Owner = objs.[2] :?> uint32
            Name = objs.[3] :?> string
        }
        row :> obj

type MethodSpecRow =
    {
        [<MethodDefOrRefCodedIndex>]
        Method : uint32
        Instantiation : byte[]
    }

    static member MakeRecord(objs : obj[]) =
        let row : MethodSpecRow = {
            Method = objs.[0] :?> uint32
            Instantiation = objs.[1] :?> byte[]
        }
        row :> obj

type GenericParamConstraintRow =
    {
        [<SimpleIndex(TableNumber.GenericParam)>]
        Owner : uint32
        [<TypeDefOrRefCodedIndex>]
        Constraint : uint32
    }

    static member MakeRecord(objs : obj[]) =
        let row : GenericParamConstraintRow = {
            Owner = objs.[0] :?> uint32
            Constraint = objs.[1] :?> uint32
        }
        row :> obj

module Tables =
    let tokenOptOfValue =
        function
        | 0u -> None
        | (value : uint32) ->
            let table : TableNumber = EnumOfValue(byte(value >>> 24))
            let index = int(value &&& 0xffffffu)
            if index = 0 then
                failwith "invalid token"
            Some (table, index)

    let tokenOfValue value =
        match tokenOptOfValue value with
        | Some token -> token
        | None -> failwith "Null token invalid."

    let tableNumber (enum : TableNumber) =
        EnumToValue enum |> int

    let private recordMakerMap =
        let m = System.Collections.Generic.Dictionary<System.Type, (obj[] -> obj)>()
        [
            typeof<ModuleRow>, ModuleRow.MakeRecord
            typeof<TypeRefRow>, TypeRefRow.MakeRecord
            typeof<TypeDefRow>, TypeDefRow.MakeRecord
            typeof<FieldRow>, FieldRow.MakeRecord
            typeof<MethodDefRow>, MethodDefRow.MakeRecord
            typeof<ParamRow>, ParamRow.MakeRecord
            typeof<InterfaceImplRow>, InterfaceImplRow.MakeRecord
            typeof<MemberRefRow>, MemberRefRow.MakeRecord
            typeof<ConstantRow>, ConstantRow.MakeRecord
            typeof<CustomAttributeRow>, CustomAttributeRow.MakeRecord
            typeof<FieldMarshalRow>, FieldMarshalRow.MakeRecord
            typeof<DeclSecurityRow>, DeclSecurityRow.MakeRecord
            typeof<ClassLayoutRow>, ClassLayoutRow.MakeRecord
            typeof<FieldLayoutRow>, FieldLayoutRow.MakeRecord
            typeof<StandAloneSigRow>, StandAloneSigRow.MakeRecord
            typeof<EventMapRow>, EventMapRow.MakeRecord
            typeof<EventRow>, EventRow.MakeRecord
            typeof<PropertyMapRow>, PropertyMapRow.MakeRecord
            typeof<PropertyRow>, PropertyRow.MakeRecord
            typeof<MethodSemanticsRow>, MethodSemanticsRow.MakeRecord
            typeof<MethodImplRow>, MethodImplRow.MakeRecord
            typeof<ModuleRefRow>, ModuleRefRow.MakeRecord
            typeof<TypeSpecRow>, TypeSpecRow.MakeRecord
            typeof<ImplMapRow>, ImplMapRow.MakeRecord
            typeof<FieldRVARow>, FieldRVARow.MakeRecord
            typeof<AssemblyRow>, AssemblyRow.MakeRecord
            typeof<AssemblyProcessorRow>, AssemblyProcessorRow.MakeRecord
            typeof<AssemblyOSRow>, AssemblyOSRow.MakeRecord
            typeof<AssemblyRefRow>, AssemblyRefRow.MakeRecord
            typeof<AssemblyRefProcessorRow>, AssemblyRefProcessorRow.MakeRecord
            typeof<AssemblyRefOSRow>, AssemblyRefOSRow.MakeRecord
            typeof<FileRow>, FileRow.MakeRecord
            typeof<ExportedTypeRow>, ExportedTypeRow.MakeRecord
            typeof<ManifestResourceRow>, ManifestResourceRow.MakeRecord
            typeof<NestedClassRow>, NestedClassRow.MakeRecord
            typeof<GenericParamRow>, GenericParamRow.MakeRecord
            typeof<MethodSpecRow>, MethodSpecRow.MakeRecord
            typeof<GenericParamConstraintRow>, GenericParamConstraintRow.MakeRecord
        ]
        |> List.iter (fun (k, v) -> m.Add(k, v))
        m

    let getRecordMaker rowType =
        recordMakerMap.[rowType]
