namespace Roma.Compiler

open Dwarf

type private MutableMap<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type private MutableList<'t> = System.Collections.Generic.List<'t>

// XXX: no checking against multiple enums with the same name in the same namespace

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
type TypeEntry internal (tree : DwTree, tag) =
    let node = tree.Create(tag)

    static member internal Attr typeEntryOpt =
        seq {
            for (typeEntry : TypeEntry) in Option.toArray typeEntryOpt do
                yield DwAt.Type, DwValue.Ref typeEntry.Node
        }

    member internal this.Node = node

and TypeContainer internal (tree, node : DwNode) =

    let typeMap = MutableMap<string, TypeEntry>()

    let add name (newType : #TypeEntry) =
        node.AddChild(newType.Node)
        typeMap.Add(name, newType)
        newType

    member this.FindType(name) =
        typeMap.[name]

    member this.CreateEnumType(name) =
        add name (EnumTypeEntry(tree, name))

    member this.CreateStructType(name) =
        add name (StructTypeEntry(tree, name))

    member this.CreateClassType(name) =
        add name (ClassTypeEntry(tree, name))

    member this.CreateInterfaceType(name) =
        add name (InterfaceTypeEntry(tree, name))

    member this.CreateTypedef(name) =
        add name (TypedefEntry(tree, name))

and ITypeContainer =
    abstract FindType : string -> TypeEntry
    abstract CreateEnumType : string -> EnumTypeEntry
    abstract CreateStructType : string -> StructTypeEntry
    abstract CreateClassType : string -> ClassTypeEntry
    abstract CreateInterfaceType : string -> InterfaceTypeEntry
    abstract CreateTypedef : string -> TypedefEntry

and EnumTypeEntry internal (tree, name) as this =
    inherit TypeEntry(tree, DwTag.EnumerationType)

    do this.Node.AddAttrs(
        [
            DwAt.Name, DwValue.String name
            DwAt.EnumClass, DwValue.Bool true
        ])

    member this.Name = name

    member this.SetByteSize(size : uint32) =
        this.Node.AddAttr(DwAt.ByteSize, DwValue.Udata(UInt128 size))

    member this.SetUnderlyingType(underlyingType : TypeEntry) =
        this.Node.AddAttr(DwAt.Type, DwValue.Ref underlyingType.Node)

    member this.AddValue(name, value) =
        let child =
            tree.Create(
                DwTag.Enumerator,
                [
                    DwAt.Name, DwValue.String name
                    DwAt.ConstValue, DwValue.Sdata value
                ])
        this.Node.AddChild(child)

and StructTypeEntry internal (tree, name) =
    inherit MemberContainer(tree, DwTag.StructureType, name)
    // TODO: DwAt.ByteSize

and ClassTypeEntry internal (tree, name) =
    inherit MemberContainer(tree, DwTag.ClassType, name)
    // TODO: DwAt.ByteSize

and InterfaceTypeEntry internal (tree, name) =
    inherit MemberContainer(tree, DwTag.InterfaceType, name)

and MemberContainer internal (tree, tag, name) as this =
    inherit TypeEntry(tree, tag)

    do this.Node.AddAttr(DwAt.Name, DwValue.String name)

    let typeContainer = TypeContainer(tree, this.Node)
    let members = MutableList<_>()
    let subprograms = MutableList<_>()

    interface ITypeContainer with
        member this.FindType(name) = typeContainer.FindType(name)
        member this.CreateEnumType(name) = typeContainer.CreateEnumType(name)
        member this.CreateStructType(name) = typeContainer.CreateStructType(name)
        member this.CreateClassType(name) = typeContainer.CreateClassType(name)
        member this.CreateInterfaceType(name) = typeContainer.CreateInterfaceType(name)
        member this.CreateTypedef(name) = typeContainer.CreateTypedef(name)

    member this.Name = name

    member this.Inherit(baseType : TypeEntry) =
        let child =
            tree.Create(
                DwTag.Inheritance,
                [
                    DwAt.Type, DwValue.Ref baseType.Node
                    // TODO: DwAt.DataMemberLocation
                    // TODO: DwAt.Accessibility
                ])
        this.Node.AddChild(child)

    member this.AddMember(name) =
        let mem = Member(tree, name)
        members.Add(mem)
        this.Node.AddChild(mem.Node)
        mem

    member this.AddSubprogram(name) =
        let sub = Subprogram(tree, name)
        subprograms.Add(sub)
        this.Node.AddChild(sub.Node)
        sub

and Member internal (tree : DwTree, name : string) =
    let node = tree.Create(DwTag.Member, [ DwAt.Name, DwValue.String name ])
    // TODO: DwAt.DataMemberLocation

    member this.Node = node

    member this.SetType(memberType : TypeEntry option) =
        node.AddAttrs(TypeEntry.Attr memberType)

and Subprogram internal (tree : DwTree, name : string) =
    let node = tree.Create(DwTag.Subprogram, [ DwAt.Name, DwValue.String name ])

    member this.Node = node

    member this.SetReturnType(memberType : TypeEntry option) =
        node.AddAttrs(TypeEntry.Attr memberType)

and TypedefEntry internal (tree, name) as this =
    inherit TypeEntry(tree, DwTag.Typedef)

    do this.Node.AddAttr(DwAt.Name, DwValue.String name)

    let mutable referencedType = None

    member this.ReferencedType
        with get() = referencedType

        and set (typeOpt) = 
            referencedType <- typeOpt
            this.Node.AddAttrs(TypeEntry.Attr typeOpt)

type PrimitiveTypeEntry internal (tree, kind, name, encoding : DwAte, byteSize : uint32) as this =
    inherit TypeEntry(tree, DwTag.BaseType)

    do this.Node.AddAttrs(
        [
            DwAt.Name, DwValue.String name
            DwAt.Encoding, DwValue.Udata(UInt128(LanguagePrimitives.EnumToValue encoding))
            DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
        ])

    member this.Kind = kind

    member this.Name = name

    member this.ByteSize = byteSize

type ConstTypeEntry internal (tree, modifiedType : TypeEntry) as this =
    inherit TypeEntry(tree, DwTag.ConstType)

    do this.Node.AddAttr(DwAt.Type, DwValue.Ref modifiedType.Node)

    member this.ModifiedType = modifiedType

type VolatileTypeEntry internal (tree, modifiedType : TypeEntry) as this =
    inherit TypeEntry(tree, DwTag.VolatileType)

    do this.Node.AddAttr(DwAt.Type, DwValue.Ref modifiedType.Node)

    member this.ModifiedType = modifiedType

type PointerTypeEntry internal (tree, t, byteSize : uint32) as this =
    inherit TypeEntry(tree, DwTag.PointerType)

    do this.Node.AddAttrs(
        [
            yield! TypeEntry.Attr t
            yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
        ])

    member this.ReferencedType = t

type ManagedPointerTypeEntry internal (tree, t : TypeEntry, byteSize : uint32) as this =
    inherit TypeEntry(tree, DwTag.PointerType)

    do this.Node.AddAttrs(
        [
            yield DwAt.Type, DwValue.Ref t.Node
            yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
            yield DwAt.ClrManaged, DwValue.Bool true
        ])

    member this.ReferencedType = t

type ReferenceTypeEntry internal (tree, t, byteSize : uint32) as this =
    inherit TypeEntry(tree, DwTag.ReferenceType)

    do this.Node.AddAttrs(
        [
            yield! TypeEntry.Attr t
            yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
        ])

    member this.ReferencedType = t

type ArrayTypeEntry internal (tree, sizeType : TypeEntry, t, sizeOpt) as this =
    inherit TypeEntry(tree, DwTag.ArrayType)

    do
        this.Node.AddAttrs(TypeEntry.Attr t)
        for size in Option.toArray sizeOpt do
            assert(size >= 1u)
            let child =
                tree.Create(
                    DwTag.SubrangeType,
                    [
                        DwAt.Type, DwValue.Ref sizeType.Node
                        DwAt.UpperBound, DwValue.Udata(UInt128(size - 1u))
                    ])
            this.Node.AddChild(child)

type ManagedArrayTypeEntry internal (tree, sizeType : TypeEntry, elemType : TypeEntry, shapeOpt) as this =
    inherit TypeEntry(tree, DwTag.ArrayType)

    do
        this.Node.AddAttrs(
            [
                DwAt.ClrManaged, DwValue.Bool true
                DwAt.Type, DwValue.Ref elemType.Node
            ])

        for shape in Option.toArray shapeOpt do
            for (lo : int, sizeOpt : int option) in (shape : list<_>) do
                this.Node.AddChild(
                    tree.Create(
                        DwTag.SubrangeType,
                        [
                            yield DwAt.LowerBound, DwValue.Sdata(Int128 lo)
                            yield! seq { for size in Option.toArray sizeOpt -> DwAt.UpperBound, DwValue.Sdata(Int128(lo + size)) }
                        ]))

    member this.ElementType = elemType

type SubroutineTypeEntry internal (tree, retType, paramTypes) as this =
    inherit TypeEntry(tree, DwTag.SubroutineType)

    do
        this.Node.AddAttrs(
            [
                yield DwAt.Prototyped, DwValue.Bool true
                yield! TypeEntry.Attr retType
            ])
        for paramType in paramTypes do
            let child =
                tree.Create(
                    DwTag.FormalParameter,
                    [
                        yield! TypeEntry.Attr paramType
                    ])
            this.Node.AddChild(child)

type INamespace =
    inherit ITypeContainer
    abstract GetNamespace : string -> INamespace

type internal Namespace(tree : DwTree, node : DwNode) =
    let nsMap = MutableMap<_, _>()
    let typeContainer = TypeContainer(tree, node)

    interface INamespace with
        member this.GetNamespace(name) = this.GetNamespace(name) :> INamespace
        member this.FindType(name) = typeContainer.FindType(name)
        member this.CreateEnumType(name) = typeContainer.CreateEnumType(name)
        member this.CreateStructType(name) = typeContainer.CreateStructType(name)
        member this.CreateClassType(name) = typeContainer.CreateClassType(name)
        member this.CreateInterfaceType(name) = typeContainer.CreateInterfaceType(name)
        member this.CreateTypedef(name) = typeContainer.CreateTypedef(name)

    member this.GetNamespace(name : string) =
        match nsMap.TryGetValue(name) with
        | true, ns -> ns
        | _ ->
            let child = tree.Create(DwTag.Namespace, [ DwAt.Name, DwValue.String name ])
            node.AddChild(child)
            let ns = Namespace(tree, child)
            nsMap.Add(name, ns)
            ns

type CompileUnit(addrSize : AddrSize) =
    let ptrSize =
        match addrSize with
        | Addr32 -> 4u
        | Addr64 -> 8u

    let tree = DwTree()
    let node = tree.Create(DwTag.CompileUnit, [ DwAt.UseUTF8, DwValue.Bool true ])
    let globalNamespace = Namespace(tree, node)
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
        PrimitiveTypeEntry(tree, kind, name, encoding, byteSize)

    let sizeType =
        lazy (
            let entry = PrimitiveTypeEntry(tree, PrimitiveTypeKind.UIntPtr, "sizetype", DwAte.Unsigned, ptrSize)
            node.AddChild(entry.Node)
            entry
        )

    let createConstType modifiedType = ConstTypeEntry(tree, modifiedType)
    let createVolatileType modifiedType = VolatileTypeEntry(tree, modifiedType)
    let createPointerType typeOpt = PointerTypeEntry(tree, typeOpt, ptrSize)
    let createManagedPointerType referencedType = ManagedPointerTypeEntry(tree, referencedType, ptrSize)
    let createReferenceType typeOpt = ReferenceTypeEntry(tree, typeOpt, ptrSize)
    let createArrayType(typeOpt, size) = ArrayTypeEntry(tree, sizeType.Force(), typeOpt, size)
    let createManagedArrayType(elemType, shape) = ManagedArrayTypeEntry(tree, sizeType.Force(), elemType, shape)
    let createSubroutineType(retType, paramTypes) = SubroutineTypeEntry(tree, retType, paramTypes)

    let memoize (map : MutableMap<_, _>) f key =
        match map.TryGetValue(key) with
        | true, value -> value
        | _ ->
            let value : #TypeEntry = f key
            node.AddChild(value.Node)
            map.Add(key, value)
            value

    let memoizeOpt ((map : MutableMap<_, _>), voidRef) f keyOpt =
        match keyOpt with
        | None ->
            match !voidRef with
            | Some value -> value
            | None ->
                let value : #TypeEntry = f keyOpt
                node.AddChild(value.Node)
                voidRef := Some value
                value
        | Some key ->
            match map.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value : #TypeEntry = f keyOpt
                node.AddChild(value.Node)
                map.Add(key, value)
                value

    let gns = globalNamespace :> INamespace

    interface INamespace with
        member this.GetNamespace(name) = gns.GetNamespace(name)
        member this.FindType(name) = gns.FindType(name)
        member this.CreateEnumType(args) = gns.CreateEnumType(args)
        member this.CreateStructType(name) = gns.CreateStructType(name)
        member this.CreateClassType(name) = gns.CreateClassType(name)
        member this.CreateInterfaceType(name) = gns.CreateInterfaceType(name)
        member this.CreateTypedef(name) = gns.CreateTypedef(name)

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

    member this.WriteTo(path : string) =
        let text =
            let buffer = System.Text.StringBuilder()
            for line in Dwarf.serialize addrSize node do
                buffer.AppendLine(line) |> ignore
            buffer.ToString()
        if path = "-" then
            System.Console.Out.Write(text)
        else
            System.IO.File.WriteAllText(path, text)
