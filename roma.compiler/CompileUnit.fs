namespace Roma.Compiler

open Dwarf

// XXX: no checking against multiple enums with the same name in the same namespace

type AddrSize =
    | Addr32
    | Addr64

type PrimitiveTypeKind =
    | Bool
    | Null
    | Char8
    | Char16
    | Char32
    | SInt8
    | SInt16
    | SInt32
    | SInt64
    | SIntPtr
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | UIntPtr
    | Float32
    | Float64

[<AbstractClass>]
type TypeEntry internal (tag) =
    let node = DwNode(tag)

    member internal this.Node = node

    member this.SetByteSize(size : uint32) =
        node.AddAttr(DwAt.ByteSize, DwValue.Sdata(Int128 size))

    static member internal TypeAttr(te : #TypeEntry) =
        DwAt.Type, DwValue.Ref te.Node

    static member internal TypeAttrOpt(typeEntryOpt) =
        Option.toArray typeEntryOpt
        |> Seq.map TypeEntry.TypeAttr

and internal TypeContainer(node : DwNode) =

    let typeMap = MutableMap<string, TypeEntry>()

    let add name (te : #TypeEntry) =
        typeMap.Add(name, te)
        node.AddChild(te.Node)
        te

    member this.FindType(name) =
        typeMap.[name]

    member this.TryFindType(name) =
        match typeMap.TryGetValue(name) with
        | true, te -> Some te
        | _ -> None

    member this.CreateEnumType(name) =
        add name (EnumTypeEntry(name))

    member this.CreateStructType(name) =
        add name (StructTypeEntry(name))

    member this.CreateClassType(name) =
        add name (ClassTypeEntry(name))

    member this.CreateInterfaceType(name) =
        add name (InterfaceTypeEntry(name))

    member this.CreateTypedef(name) =
        add name (TypedefEntry(name))

and ITypeContainer =
    abstract FindType : string -> TypeEntry
    abstract TryFindType : string -> TypeEntry option
    abstract CreateEnumType : string -> EnumTypeEntry
    abstract CreateStructType : string -> StructTypeEntry
    abstract CreateClassType : string -> ClassTypeEntry
    abstract CreateInterfaceType : string -> InterfaceTypeEntry
    abstract CreateTypedef : string -> TypedefEntry

and EnumTypeEntry internal (name) as this =
    inherit TypeEntry(DwTag.EnumerationType)

    do
        [
            DwAt.Name, DwValue.String name
            DwAt.EnumClass, DwValue.Bool true
        ]
        |> this.Node.AddAttrs

    member this.Name = name

    member this.SetByteSize(size : uint32) =
        this.Node.AddAttr(DwAt.ByteSize, DwValue.Udata(UInt128 size))

    member this.SetUnderlyingType(underlyingType : TypeEntry) =
        this.Node.AddAttr(TypeEntry.TypeAttr underlyingType)

    member this.AddValue(name, value) =
        let node = DwNode(DwTag.Enumerator)
        [
            DwAt.Name, DwValue.String name
            DwAt.ConstValue, DwValue.Sdata value
        ]
        |> node.AddAttrs
        this.Node.AddChild(node)

and StructTypeEntry internal (name) =
    inherit CompositeTypeBase(DwTag.StructureType, name)
    // TODO: DwAt.ByteSize

and ClassTypeEntry internal (name) =
    inherit CompositeTypeBase(DwTag.ClassType, name)
    // TODO: DwAt.ByteSize

and InterfaceTypeEntry internal (name) =
    inherit CompositeTypeBase(DwTag.InterfaceType, name)

and [<AbstractClass>] CompositeTypeBase internal (tag, name) as this =
    inherit TypeEntry(tag)

    do this.Node.AddAttr(DwAt.Name, DwValue.String name)

    let typeContainer = TypeContainer(this.Node)

    let members = MutableList<Member>()
    let subprograms = MutableList<Subprogram>()

    interface ITypeContainer with
        member this.FindType(name) = typeContainer.FindType(name)
        member this.TryFindType(name) = typeContainer.TryFindType(name)
        member this.CreateEnumType(name) = typeContainer.CreateEnumType(name)
        member this.CreateStructType(name) = typeContainer.CreateStructType(name)
        member this.CreateClassType(name) = typeContainer.CreateClassType(name)
        member this.CreateInterfaceType(name) = typeContainer.CreateInterfaceType(name)
        member this.CreateTypedef(name) = typeContainer.CreateTypedef(name)

    member this.Name = name

    member this.AddTypeParameter(name, paramType : TypeEntry) =
        let node = DwNode(DwTag.TemplateTypeParameter)
        [
            DwAt.Name, DwValue.String name
            TypeEntry.TypeAttr paramType
        ]
        |> node.AddAttrs
        this.Node.AddChild(node)

    member this.Inherit(baseType : TypeEntry) =
        let node = DwNode(DwTag.Inheritance)
        [
            TypeEntry.TypeAttr baseType
            // TODO: DwAt.DataMemberLocation
            // TODO: DwAt.Accessibility
        ]
        |> node.AddAttrs
        this.Node.AddChild(node)

    member this.AddMember(name) =
        let mem = Member(name)
        members.Add(mem)
        this.Node.AddChild(mem.Node)
        mem

    member this.AddSubprogram(name) =
        let sub = Subprogram(name)
        subprograms.Add(sub)
        this.Node.AddChild(sub.Node)
        sub

and Member internal (name : string) =
    let node = DwNode(DwTag.Member)
    do node.AddAttr(DwAt.Name, DwValue.String name)

    member this.Node = node

    member this.SetType(memberType : TypeEntry) =
        node.AddAttr(TypeEntry.TypeAttr memberType)

    member this.SetDataMemberLocation(offset : int) =
        node.AddAttr(DwAt.DataMemberLocation, DwValue.Sdata(Int128 offset))

and Subprogram internal (name : string) =
    let node = DwNode(DwTag.Subprogram)
    do node.AddAttr(DwAt.Name, DwValue.String name)

    member this.Node = node

    member this.SetReturnType(memberType) =
        node.AddAttrs(TypeEntry.TypeAttrOpt memberType)

    member this.AddParameter(paramType, nameOpt : string option) =
        let param = FormalParameter(paramType, nameOpt)
        node.AddChild(param.Node)
        param

    member this.AddUnspecifiedParameters() =
        DwNode(DwTag.UnspecifiedParameters)
        |> node.AddChild

    member this.SetObjectPointer(param : FormalParameter) =
        node.AddAttr(DwAt.ObjectPointer, DwValue.Ref param.Node)

    member this.SetVirtual(isPure : bool) =
        node.AddAttr(DwAt.Virtuality, DwValue.Sdata(Int128(if isPure then 2 else 1)))

and FormalParameter internal (paramType : TypeEntry, nameOpt) =
    let node = DwNode(DwTag.FormalParameter)
    do
        [
            yield TypeEntry.TypeAttr paramType
            for name in Option.toArray nameOpt do
                yield DwAt.Name, DwValue.String name
        ]
        |> node.AddAttrs

    member this.Node = node

    member this.SetArtificial() =
        node.AddAttr(DwAt.Artificial, DwValue.Bool true)

and TypedefEntry internal (name) as this =
    inherit TypeEntry(DwTag.Typedef)

    do this.Node.AddAttr(DwAt.Name, DwValue.String name)

    let mutable referencedType : TypeEntry option = None

    member this.ReferencedType
        with get() = referencedType

        and set typeOpt = 
            referencedType <- typeOpt
            TypeEntry.TypeAttrOpt typeOpt
            |> this.Node.AddAttrs

type PrimitiveTypeEntry internal (kind, name, encoding : DwAte, byteSize : uint32) as this =
    inherit TypeEntry(DwTag.BaseType)

    do
        [
            DwAt.Name, DwValue.String name
            DwAt.Encoding, DwValue.Udata(UInt128(uint32 encoding))
            DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
        ]
        |> this.Node.AddAttrs

    member this.Kind = kind

    member this.Name = name

    member this.ByteSize = byteSize

type ConstTypeEntry internal (modifiedType : TypeEntry) as this =
    inherit TypeEntry(DwTag.ConstType)

    do this.Node.AddAttr(DwAt.Type, DwValue.Ref modifiedType.Node)

    member this.ModifiedType = modifiedType

type VolatileTypeEntry internal (modifiedType : TypeEntry) as this =
    inherit TypeEntry(DwTag.VolatileType)

    do this.Node.AddAttr(DwAt.Type, DwValue.Ref modifiedType.Node)

    member this.ModifiedType = modifiedType

type PointerTypeEntry internal (t, byteSize : uint32) as this =
    inherit TypeEntry(DwTag.PointerType)

    do
        [
            yield! TypeEntry.TypeAttrOpt t
            yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
        ]
        |> this.Node.AddAttrs

    member this.ReferencedType = t

type ManagedPointerTypeEntry internal (t : TypeEntry, byteSize : uint32) as this =
    inherit TypeEntry(DwTag.PointerType)

    do
        [
            yield TypeEntry.TypeAttr t
            yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
            yield DwAt.ClrManaged, DwValue.Bool true
        ]
        |> this.Node.AddAttrs

    member this.ReferencedType = t

type ReferenceTypeEntry internal (t, byteSize : uint32) as this =
    inherit TypeEntry(DwTag.ReferenceType)

    do
        [
            yield! TypeEntry.TypeAttrOpt t
            yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
        ]
        |> this.Node.AddAttrs

    member this.ReferencedType = t

type ArrayTypeEntry internal (sizeType : TypeEntry, t : TypeEntry option, sizeOpt) as this =
    inherit TypeEntry(DwTag.ArrayType)

    do
        this.Node.AddAttrs(TypeEntry.TypeAttrOpt t)
        for size in Option.toArray sizeOpt do
            assert(size >= 1u)
            let child = DwNode(DwTag.SubrangeType)
            [
                yield TypeEntry.TypeAttr sizeType
                yield DwAt.UpperBound, DwValue.Udata(UInt128(size - 1u))
            ]
            |> child.AddAttrs
            this.Node.AddChild(child)

type ManagedArrayTypeEntry internal (sizeType : TypeEntry, elemType : TypeEntry, shapeOpt) as this =
    inherit TypeEntry(DwTag.ArrayType)

    do
        [
            yield DwAt.ClrManaged, DwValue.Bool true
            yield TypeEntry.TypeAttr elemType
        ]
        |> this.Node.AddAttrs

        for shape in Option.toArray shapeOpt do
            for (lo : int, sizeOpt : int option) in (shape : list<_>) do
                let node = DwNode(DwTag.SubrangeType)
                [
                    yield DwAt.LowerBound, DwValue.Sdata(Int128 lo)
                    yield! seq { for size in Option.toArray sizeOpt -> DwAt.UpperBound, DwValue.Sdata(Int128(lo + size)) }
                ]
                |> node.AddAttrs
                this.Node.AddChild(node)

    member this.ElementType = elemType

type SubroutineTypeEntry internal (retType : TypeEntry option, paramTypes : TypeEntry option seq) as this =
    inherit TypeEntry(DwTag.SubroutineType)

    do
        [
            yield DwAt.Prototyped, DwValue.Bool true
            yield! TypeEntry.TypeAttrOpt retType
        ]
        |> this.Node.AddAttrs
        for paramType in paramTypes do
            let child = DwNode(DwTag.FormalParameter)
            child.AddAttrs(TypeEntry.TypeAttrOpt paramType)
            this.Node.AddChild(child)

type Variable internal (name : string) =
    let node = DwNode(DwTag.Variable)
    do node.AddAttr(DwAt.Name, DwValue.String name)

    member this.Node = node

    member this.SetType(variableType : TypeEntry) =
        node.AddAttr(TypeEntry.TypeAttr variableType)

type INamespace =
    inherit ITypeContainer
    abstract GetNamespace : string -> INamespace

type internal Namespace(node : DwNode) =
    let nsMap = MutableMap<_, _>()
    let typeContainer = TypeContainer(node)

    interface INamespace with
        member this.GetNamespace(name) = this.GetNamespace(name) :> INamespace
        member this.FindType(name) = typeContainer.FindType(name)
        member this.TryFindType(name) = typeContainer.TryFindType(name)
        member this.CreateEnumType(name) = typeContainer.CreateEnumType(name)
        member this.CreateStructType(name) = typeContainer.CreateStructType(name)
        member this.CreateClassType(name) = typeContainer.CreateClassType(name)
        member this.CreateInterfaceType(name) = typeContainer.CreateInterfaceType(name)
        member this.CreateTypedef(name) = typeContainer.CreateTypedef(name)

    member this.GetNamespace(name : string) =
        match nsMap.TryGetValue(name) with
        | true, ns -> ns
        | _ ->
            let child = DwNode(DwTag.Namespace)
            child.AddAttr(DwAt.Name, DwValue.String name)
            let ns = Namespace(child)
            nsMap.Add(name, ns)
            node.AddChild(child)
            ns

type Module internal (name) =
    let node = DwNode(DwTag.Module)
    do node.AddAttr(DwAt.Name, DwValue.String name)
    let ns = Namespace(node) :> INamespace
    let subprograms = MutableList<_>()

    interface INamespace with
        member this.GetNamespace(name) = ns.GetNamespace(name)
        member this.FindType(name) = ns.FindType(name)
        member this.TryFindType(name) = ns.TryFindType(name)
        member this.CreateEnumType(args) = ns.CreateEnumType(args)
        member this.CreateStructType(name) = ns.CreateStructType(name)
        member this.CreateClassType(name) = ns.CreateClassType(name)
        member this.CreateInterfaceType(name) = ns.CreateInterfaceType(name)
        member this.CreateTypedef(name) = ns.CreateTypedef(name)

    member internal this.Node = node

    member this.Name = name

    member this.AddSubprogram(name) =
        let sub = Subprogram(name)
        subprograms.Add(sub)
        node.AddChild(sub.Node)
        sub

type UnitBase(tag, addrSize : AddrSize) =
    let ptrSize =
        match addrSize with
        | Addr32 -> 4u
        | Addr64 -> 8u

    let node = DwNode(tag)
    do node.AddAttr(DwAt.UseUTF8, DwValue.Bool true)
    let ns = Namespace(node) :> INamespace
    let modules = MutableMap<_, _>()
    let variables = MutableList<_>()
    let subprograms = MutableList<_>()
    let primTypeMap = MutableMap<_, _>()
    let constTypeMap = MutableMap<_, _>()
    let volatileTypeMap = MutableMap<_, _>()
    let pointerTypeMap = (MutableMap<_, _>(), ref None)
    let managedPointerTypeMap = MutableMap<_, _>()
    let referenceTypeMap = (MutableMap<_, _>(), ref None)
    let arrayTypeMap = MutableMap<_, _>()
    let managedArrayTypeMap = MutableMap<_, _>()
    let subroutineTypeMap = MutableMap<_, _>()

    let createPrimType kind =
        let name, encoding, byteSize =
            match kind with
            // FIXME: plang-specific primitive type names
            | PrimitiveTypeKind.Bool -> "bool", DwAte.Boolean, 1u
            | PrimitiveTypeKind.Null -> "null_t", DwAte.Address, ptrSize
            | PrimitiveTypeKind.Char8 -> "char8_t", DwAte.Utf, 1u
            | PrimitiveTypeKind.Char16 -> "char16_t", DwAte.Utf, 2u
            | PrimitiveTypeKind.Char32 -> "char32_t", DwAte.Utf, 4u
            | PrimitiveTypeKind.SInt8 -> "sint8_t", DwAte.Signed, 1u
            | PrimitiveTypeKind.SInt16 -> "sint16_t", DwAte.Signed, 2u
            | PrimitiveTypeKind.SInt32 -> "sint32_t", DwAte.Signed, 4u
            | PrimitiveTypeKind.SInt64 -> "sint64_t", DwAte.Signed, 8u
            | PrimitiveTypeKind.SIntPtr -> "sintptr_t", DwAte.Signed, ptrSize 
            | PrimitiveTypeKind.UInt8 -> "uint8_t", DwAte.Unsigned, 1u
            | PrimitiveTypeKind.UInt16 -> "uint16_t", DwAte.Unsigned, 2u
            | PrimitiveTypeKind.UInt32 -> "uint32_t", DwAte.Unsigned, 4u
            | PrimitiveTypeKind.UInt64 -> "uint64_t", DwAte.Unsigned, 8u
            | PrimitiveTypeKind.UIntPtr -> "uintptr_t", DwAte.Unsigned, ptrSize
            | PrimitiveTypeKind.Float32 -> "float32_t", DwAte.Float, 4u
            | PrimitiveTypeKind.Float64 -> "float64_t", DwAte.Float, 8u
        PrimitiveTypeEntry(kind, name, encoding, byteSize)

    let sizeType =
        lazy (
            let entry = PrimitiveTypeEntry(PrimitiveTypeKind.UIntPtr, "sizetype", DwAte.Unsigned, ptrSize)
            node.AddChild(entry.Node)
            entry
        )

    let createConstType modifiedType = ConstTypeEntry(modifiedType)
    let createVolatileType modifiedType = VolatileTypeEntry(modifiedType)
    let createPointerType typeOpt = PointerTypeEntry(typeOpt, ptrSize)
    let createManagedPointerType referencedType = ManagedPointerTypeEntry(referencedType, ptrSize)
    let createReferenceType typeOpt = ReferenceTypeEntry(typeOpt, ptrSize)
    let createArrayType(typeOpt, size) = ArrayTypeEntry(sizeType.Force(), typeOpt, size)
    let createManagedArrayType(elemType, shape) = ManagedArrayTypeEntry(sizeType.Force(), elemType, shape)
    let createSubroutineType(retType, paramTypes) = SubroutineTypeEntry(retType, paramTypes)

    let memoize (map : MutableMap<_, _>) f key =
        match map.TryGetValue(key) with
        | true, value -> value
        | _ ->
            let value : #TypeEntry = f key
            map.Add(key, value)
            node.AddChild(value.Node)
            value

    let memoizeOpt ((map : MutableMap<_, _>), voidRef) f keyOpt =
        match keyOpt with
        | None ->
            match !voidRef with
            | Some value -> value
            | None ->
                let value : #TypeEntry = f keyOpt
                node.AddChild(value.Node)
                assert(!voidRef = None)
                voidRef := Some value
                value
        | Some key ->
            match map.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value : #TypeEntry = f keyOpt
                map.Add(key, value)
                node.AddChild(value.Node)
                value

    interface INamespace with
        member this.GetNamespace(name) = ns.GetNamespace(name)
        member this.FindType(name) = ns.FindType(name)
        member this.TryFindType(name) = ns.TryFindType(name)
        member this.CreateEnumType(args) = ns.CreateEnumType(args)
        member this.CreateStructType(name) = ns.CreateStructType(name)
        member this.CreateClassType(name) = ns.CreateClassType(name)
        member this.CreateInterfaceType(name) = ns.CreateInterfaceType(name)
        member this.CreateTypedef(name) = ns.CreateTypedef(name)

    member this.GetPrimitiveType(kind) =
        memoize primTypeMap createPrimType kind

    member this.GetConstType(modifiedType) =
        memoize constTypeMap createConstType modifiedType

    member this.GetVolatileType(modifiedType) =
        memoize volatileTypeMap createVolatileType modifiedType

    member this.GetPointerType(typeOpt) =
        memoizeOpt pointerTypeMap createPointerType typeOpt

    member this.GetManagedPointerType(referencedType) =
        memoize managedPointerTypeMap createManagedPointerType referencedType

    member this.GetReferenceType(typeOpt) =
        memoizeOpt referenceTypeMap createReferenceType typeOpt

    member this.GetArrayType(elemType, size) =
        memoize arrayTypeMap createArrayType (elemType, size)

    member this.GetManagedArrayType(elemType, ?shape) =
        memoize managedArrayTypeMap createManagedArrayType (elemType, shape)

    member this.GetSubroutineType(retType, paramTypes) =
        memoize subroutineTypeMap createSubroutineType (retType, paramTypes)

    member this.AddVariable(name) =
        let var = Variable(name)
        variables.Add(var)
        node.AddChild(var.Node)
        var

    member this.AddSubprogram(name) =
        let sub = Subprogram(name)
        subprograms.Add(sub)
        node.AddChild(sub.Node)
        sub

    member this.GetModule(name) =
        match modules.TryGetValue(name) with
        | true, m -> m
        | _ ->
            let m = Module(name)
            modules.Add(name, m)
            node.AddChild(m.Node)
            m

    member internal this.Node = node

type CompileUnit(addrSize : AddrSize) =
    inherit UnitBase(DwTag.CompileUnit, addrSize)
