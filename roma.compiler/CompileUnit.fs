namespace Roma.Compiler

open Dwarf

type private MutableMap<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

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
type TypeEntry internal (tree : DwTree) =
    static member internal Attr typeEntryOpt =
        seq {
            for (typeEntry : TypeEntry) in Option.toArray typeEntryOpt do
                yield DwAt.Type, DwValue.Ref typeEntry.Node
        }

    abstract member Node : DwNode

type PrimitiveTypeEntry internal (tree, kind, name, encoding : DwAte, byteSize : uint32) =

    inherit TypeEntry(tree)

    let node =
        tree.Create(
            DwTag.BaseType,
            [
                yield DwAt.Name, DwValue.String name
                yield DwAt.Encoding, DwValue.Udata(UInt128(LanguagePrimitives.EnumToValue encoding))
                yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
            ])

    override this.Node = node

    member this.Kind = kind

    member this.ByteSize = byteSize


type EnumTypeEntry internal (tree, name) =

    inherit TypeEntry(tree)

    let node = 
        tree.Create(
            DwTag.EnumerationType,
            [
                DwAt.Name, DwValue.String name
                DwAt.EnumClass, DwValue.Bool true
            ])

    override this.Node = node

    member this.SetByteSize(size : uint32) =
        node.AddAttr(DwAt.ByteSize, DwValue.Udata(UInt128 size))

    member this.SetUnderlyingType(underlyingType : TypeEntry) =
        node.AddAttr(DwAt.Type, DwValue.Ref underlyingType.Node)

    member this.AddValue(name, value) =
        let child =
            tree.Create(
                DwTag.Enumerator,
                [
                    DwAt.Name, DwValue.String name
                    DwAt.ConstValue, DwValue.Sdata value
                ])
        node.AddChild(child)

type Member internal (tree : DwTree, name : string, memberType : TypeEntry option) =
    let node =
        tree.Create(
            DwTag.Member,
            [
                yield DwAt.Name, DwValue.String name
                yield! TypeEntry.Attr memberType
                // TODO: DwAt.DataMemberLocation
            ])
    member this.Node = node

type StructTypeEntry internal (tree, name : string) =

    inherit TypeEntry(tree)

    let node = 
        tree.Create(
            DwTag.StructureType,
            [
                DwAt.Name, DwValue.String name
                // TODO: DwAt.ByteSize
            ])
    let members = System.Collections.Generic.List<_>()

    override this.Node = node

    member this.Inherit(baseType : TypeEntry) =
        let child =
            tree.Create(
                DwTag.Inheritance,
                [
                    DwAt.Type, DwValue.Ref baseType.Node
                    // TODO: DwAt.DataMemberLocation
                    // TODO: DwAt.Accessibility
                ])
        node.AddChild(child)

    member this.AddMember(name, memberType) =
        let mem = Member(tree, name, memberType)
        members.Add(mem)
        node.AddChild(mem.Node)

type TypedefEntry internal (tree, name) =
    inherit TypeEntry(tree)

    let node =
        tree.Create(DwTag.Typedef, [ DwAt.Name, DwValue.String name ])

    let mutable referencedType = None

    override this.Node = node

    member this.ReferencedType
        with get() = referencedType

        and set (typeOpt) = 
            referencedType <- typeOpt
            for at, value in TypeEntry.Attr typeOpt do
                node.AddAttr(at, value)

type ConstTypeEntry internal (tree, t) =
    inherit TypeEntry(tree)

    let node =
        tree.Create(
            DwTag.ConstType,
            [
                yield! TypeEntry.Attr t
            ])

    override this.Node = node

type PointerTypeEntry internal (tree, t, byteSize : uint32) =
    inherit TypeEntry(tree)

    let node =
        tree.Create(
            DwTag.PointerType,
            [
                yield! TypeEntry.Attr t
                yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
            ])

    override this.Node = node

type ReferenceTypeEntry internal (tree, t, byteSize : uint32) =
    inherit TypeEntry(tree)

    let node =
        tree.Create(
            DwTag.ReferenceType,
            [
                yield! TypeEntry.Attr t
                yield DwAt.ByteSize, DwValue.Udata(UInt128 byteSize)
            ])

    override this.Node = node

type ArrayTypeEntry internal (tree, sizeType : TypeEntry, t, sizeOpt) =
    inherit TypeEntry(tree)

    let node =
        tree.Create(
            DwTag.ArrayType,
            [
                yield! TypeEntry.Attr t
            ])

    do
        for size in Option.toArray sizeOpt do
            assert(size >= 1u)
            let child =
                tree.Create(
                    DwTag.SubrangeType,
                    [
                        DwAt.Type, DwValue.Ref sizeType.Node
                        DwAt.UpperBound, DwValue.Udata(UInt128(size - 1u))
                    ])
            node.AddChild(child)

    override this.Node = node

type SubroutineTypeEntry internal (tree, retType, paramTypes) =
    inherit TypeEntry(tree)

    let node =
        tree.Create(
            DwTag.SubroutineType,
            [
                yield DwAt.Prototyped, DwValue.Bool true
                yield! TypeEntry.Attr retType
            ])

    do
        for paramType in paramTypes do
            let child =
                tree.Create(
                    DwTag.FormalParameter,
                    [
                        yield! TypeEntry.Attr paramType
                    ])
            node.AddChild(child)

    override this.Node = node

type INamespace =
    abstract GetNamespace : string -> INamespace
    abstract FindType : string -> TypeEntry
    abstract CreateEnumType : string -> EnumTypeEntry
    abstract CreateStructType : string -> StructTypeEntry
    abstract CreateTypedef : string -> TypedefEntry

type internal Namespace(tree : DwTree, node : DwNode) =
    let nsMap = MutableMap<_, _>()
    let typeMap = MutableMap<_, TypeEntry>()

    let add name (newType : #TypeEntry) =
        node.AddChild(newType.Node)
        typeMap.Add(name, newType)
        newType

    interface INamespace with
        member this.GetNamespace(name) =
            this.GetNamespace(name) :> INamespace
        member this.FindType(name) =
            this.FindType(name)
        member this.CreateEnumType(name) =
            this.CreateEnumType(name)
        member this.CreateStructType(name) =
            this.CreateStructType(name)
        member this.CreateTypedef(name) =
            this.CreateTypedef(name)

    member this.GetNamespace(name : string) =
        match nsMap.TryGetValue(name) with
        | true, ns -> ns
        | _ ->
            let child = tree.Create(DwTag.Namespace, [ DwAt.Name, DwValue.String name ])
            node.AddChild(child)
            let ns = Namespace(tree, child)
            nsMap.Add(name, ns)
            ns

    member this.FindType(name) =
        typeMap.[name]

    member this.CreateEnumType(name) =
        add name (EnumTypeEntry(tree, name))

    member this.CreateStructType(name) =
        add name (StructTypeEntry(tree, name))

    member this.CreateTypedef(name) =
        add name (TypedefEntry(tree, name))

type CompileUnit(addrSize : AddrSize) =
    let ptrSize =
        match addrSize with
        | Addr32 -> 4u
        | Addr64 -> 8u

    let tree = DwTree()
    let node = tree.Create(DwTag.CompileUnit)

    let globalNamespace = Namespace(tree, node)
    let primTypeMap = MutableMap<_, _>()
    let constTypeMap = MutableMap<_, _>()
    let pointerTypeMap = MutableMap<_, _>()
    let referenceTypeMap = MutableMap<_, _>()
    let arrayTypeMap = MutableMap<_, _>()
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
        Lazy.Create(
            fun() ->
                let entry = PrimitiveTypeEntry(tree, PrimitiveTypeKind.UIntPtr, "sizetype", DwAte.Unsigned, ptrSize)
                node.AddChild(entry.Node)
                entry
        )

    let createConstType typeOpt = ConstTypeEntry(tree, typeOpt)
    let createPointerType typeOpt = PointerTypeEntry(tree, typeOpt, ptrSize)
    let createReferenceType typeOpt = ReferenceTypeEntry(tree, typeOpt, ptrSize)
    let createArrayType(typeOpt, size) = ArrayTypeEntry(tree, sizeType.Force(), typeOpt, size)
    let createSubroutineType(retType, paramTypes) = SubroutineTypeEntry(tree, retType, paramTypes)

    let memoize (map : MutableMap<_, _>) f key =
        match map.TryGetValue(key) with
        | true, value -> value
        | _ ->
            let value : #TypeEntry = f key
            node.AddChild(value.Node)
            map.Add(key, value)
            value

    interface INamespace with
        member this.GetNamespace(name) =
            (globalNamespace :> INamespace).GetNamespace(name)
        member this.FindType(name) =
            (globalNamespace :> INamespace).FindType(name)
        member this.CreateEnumType(args) =
            (globalNamespace :> INamespace).CreateEnumType(args)
        member this.CreateStructType(name) =
            (globalNamespace :> INamespace).CreateStructType(name)
        member this.CreateTypedef(name) =
            (globalNamespace :> INamespace).CreateTypedef(name)

    member this.GetPrimitiveType(kind) =
        memoize primTypeMap createPrimType kind

    member this.GetConstType(typeOpt) =
        memoize constTypeMap createConstType typeOpt

    member this.GetPointerType(typeOpt) =
        memoize pointerTypeMap createPointerType typeOpt

    member this.GetReferenceType(typeOpt) =
        memoize referenceTypeMap createReferenceType typeOpt

    member this.GetArrayType(elemType, size) =
        memoize arrayTypeMap createArrayType (elemType, size)

    member this.GetSubroutineType(retType, paramTypes) =
        memoize subroutineTypeMap createSubroutineType (retType, paramTypes)

    member this.WriteTo(path : string) =
        let lines = Dwarf.serialize addrSize node
        if path = "-" then
            for line in lines do
                System.Console.Out.WriteLine(line)
        else
            System.IO.File.WriteAllLines(path, lines)
