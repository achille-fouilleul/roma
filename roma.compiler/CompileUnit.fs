namespace Roma.Compiler

open Dwarf

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
type TypeEntry() =
    abstract member CreateNode : DwTree -> DwNode

type PrimitiveTypeEntry(name : string, encoding : DwAte, byteSize : byte) =

    inherit TypeEntry()

    let mutable nodeOpt : DwNode option = None

    member this.ByteSize = byteSize

    member this.Node = Option.get nodeOpt

    override this.CreateNode(tree) =
        let node =
            tree.Create(
                DwTag.BaseType,
                [
                    yield DwAt.Name, DwValue.String name
                    yield DwAt.Encoding, DwValue.U8(byte(LanguagePrimitives.EnumToValue encoding))
                    yield DwAt.ByteSize, DwValue.U8 byteSize
                ])
        nodeOpt <- Some node
        node

type EnumTypeEntry(name : string, underlyingType : PrimitiveTypeEntry, values : (string * Int128) list) =

    inherit TypeEntry()

    override this.CreateNode(tree) =
        let enumType =
            tree.Create(
                DwTag.EnumerationType,
                [
                    DwAt.Name, DwValue.String name
                    DwAt.EnumClass, DwValue.Bool true
                    DwAt.ByteSize, DwValue.U8 underlyingType.ByteSize
                    DwAt.Type, DwValue.Ref underlyingType.Node
                ])
        for name, value in values do
            let child =
                tree.Create(
                    DwTag.Enumerator,
                    [
                        DwAt.Name, DwValue.String name
                        DwAt.ConstValue, DwValue.Sdata value
                    ])
            enumType.AddChild(child)
        enumType

type INamespace =
    abstract GetNamespace : string -> INamespace
    abstract Enums : System.Collections.Generic.ICollection<EnumTypeEntry>

type internal Namespace() =
    let nsMap = System.Collections.Generic.Dictionary<_, _>()
    let enums = System.Collections.Generic.List<EnumTypeEntry>()

    interface INamespace with
        member this.GetNamespace(name : string) =
            this.GetNamespace(name) :> INamespace
        member this.Enums =
            this.Enums :> System.Collections.Generic.ICollection<_>

    member this.GetNamespace(name : string) =
        match nsMap.TryGetValue(name) with
        | true, ns -> ns
        | false, _ ->
            let ns = Namespace()
            nsMap.Add(name, ns)
            ns

    member this.Enums = enums

    member this.BuildTree(tree : DwTree, node : DwNode) =
        for kvp in nsMap do
            let child = tree.Create(DwTag.Namespace, [ DwAt.Name, DwValue.String kvp.Key ])
            kvp.Value.BuildTree(tree, child)
            node.AddChild(child)
        for enum in enums do
            node.AddChild(enum.CreateNode(tree))

type CompileUnit(addrSize : AddrSize) =
    let rootNamespace = Namespace()
    let primTypeMap = System.Collections.Generic.Dictionary<_, _>()

    interface INamespace with
        member this.GetNamespace(name) =
            (rootNamespace :> INamespace).GetNamespace(name)
        member this.Enums =
            (rootNamespace :> INamespace).Enums

    member this.GetPrimitiveType(kind : PrimitiveTypeKind) =
        match primTypeMap.TryGetValue(kind) with
        | true, entry -> entry
        | false, _ ->
            let ptrSize =
                match addrSize with
                | Addr32 -> 4
                | Addr64 -> 8
            let name, encoding, byteSize =
                match kind with
                // FIXME: plang-specific primitive type names
                | PrimitiveTypeKind.Bool -> "bool", DwAte.Boolean, 1
                | PrimitiveTypeKind.Null -> "null_t", DwAte.Address, ptrSize
                | PrimitiveTypeKind.Char8 -> "char8_t", DwAte.Utf, 1
                | PrimitiveTypeKind.Char16 -> "char16_t", DwAte.Utf, 2
                | PrimitiveTypeKind.Char32 -> "char32_t", DwAte.Utf, 4
                | PrimitiveTypeKind.SInt8 -> "sint8_t", DwAte.Signed, 1
                | PrimitiveTypeKind.SInt16 -> "sint16_t", DwAte.Signed, 2
                | PrimitiveTypeKind.SInt32 -> "sint32_t", DwAte.Signed, 4
                | PrimitiveTypeKind.SInt64 -> "sint64_t", DwAte.Signed, 8
                | PrimitiveTypeKind.SIntPtr -> "sintptr_t", DwAte.Signed, ptrSize 
                | PrimitiveTypeKind.UInt8 -> "uint8_t", DwAte.Unsigned, 1
                | PrimitiveTypeKind.UInt16 -> "uint16_t", DwAte.Unsigned, 2
                | PrimitiveTypeKind.UInt32 -> "uint32_t", DwAte.Unsigned, 4
                | PrimitiveTypeKind.UInt64 -> "uint64_t", DwAte.Unsigned, 8
                | PrimitiveTypeKind.UIntPtr -> "uintptr_t", DwAte.Unsigned, ptrSize
                | PrimitiveTypeKind.Float32 -> "float32_t", DwAte.Float, 4
                | PrimitiveTypeKind.Float64 -> "float64_t", DwAte.Float, 8
            let entry = PrimitiveTypeEntry(name, encoding, byte byteSize)
            primTypeMap.Add(kind, entry)
            entry

    member this.WriteTo(path : string) =
        let tree = DwTree()

        let compileUnit =
            let node = tree.Create(DwTag.CompileUnit)
            for kvp in primTypeMap do
                node.AddChild(kvp.Value.CreateNode(tree))
            rootNamespace.BuildTree(tree, node)
            node

        let lines = Dwarf.serialize addrSize compileUnit
        if path = "-" then
            for line in lines do
                System.Console.Out.WriteLine(line)
        else
            System.IO.File.WriteAllLines(path, lines)
