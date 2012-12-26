namespace Roma.Compiler

open Dwarf

// XXX: no checking against multiple enums with the same name in the same namespace

type EnumType =
    {
        name : string
        values : (string * Int128) list
    }

    member internal this.CreateNode(tree : DwTree) =
        let enumType =
            tree.Create(
                DwTag.EnumerationType,
                [
                    DwAt.Name, DwValue.String this.name
                    DwAt.EnumClass, DwValue.Bool true
                ])
        for name, value in this.values do
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
    abstract Enums : System.Collections.Generic.ICollection<EnumType>

type internal Namespace() =
    let nsMap = System.Collections.Generic.Dictionary<_, _>()
    let enums = System.Collections.Generic.List<EnumType>()

    interface INamespace with
        member this.GetNamespace(name : string) =
            this.GetNamespace(name) :> INamespace
        member this.Enums =
            this.Enums :> System.Collections.Generic.ICollection<EnumType>

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

    interface INamespace with
        member this.GetNamespace(name) =
            rootNamespace.GetNamespace(name) :> INamespace
        member this.Enums =
            rootNamespace.Enums :> System.Collections.Generic.ICollection<EnumType>

    member this.WriteTo(path : string) =
        let tree = DwTree()

        let compileUnit =
            let node = tree.Create(DwTag.CompileUnit)
            rootNamespace.BuildTree(tree, node)
            node

        let lines = Dwarf.serialize addrSize compileUnit
        if path = "-" then
            for line in lines do
                System.Console.Out.WriteLine(line)
        else
            System.IO.File.WriteAllLines(path, lines)
