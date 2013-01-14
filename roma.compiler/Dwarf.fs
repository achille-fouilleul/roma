module Roma.Compiler.Dwarf

type AddrSize =
    | Addr32
    | Addr64

type DwTag =
    | ArrayType = 0x01
    | ClassType = 0x02
    | EntryPoint = 0x03
    | EnumerationType = 0x04
    | FormalParameter = 0x05
    | ImportedDeclaration = 0x08
    | Label = 0x0a
    | LexicalBlock = 0x0b
    | Member = 0x0d
    | PointerType = 0x0f
    | ReferenceType = 0x10
    | CompileUnit = 0x11
    | StringType = 0x12
    | StructureType = 0x13
    | SubroutineType = 0x15
    | Typedef = 0x16
    | UnionType = 0x17
    | UnspecifiedParameters = 0x18
    | Variant = 0x19
    | CommonBlock = 0x1a
    | CommonInclusion = 0x1b
    | Inheritance = 0x1c
    | InlinedSubroutine = 0x1d
    | Module = 0x1e
    | PrtToMemberType = 0x1f
    | SetType = 0x20
    | SubrangeType = 0x21
    | WithStmt = 0x22
    | AccessDeclaration = 0x23
    | BaseType = 0x24
    | CatchBlock = 0x25
    | ConstType = 0x26
    | Constant = 0x27
    | Enumerator = 0x28
    | FileType = 0x29
    | Friend = 0x2a
    | Namelist = 0x2b
    | NamelistItem = 0x2c
    | PackedType = 0x2d
    | Subprogram = 0x2e
    | TemplateTypeParameter = 0x2f
    | TemplateValueParameter = 0x30
    | ThrowType = 0x31
    | TryBlock = 0x32
    | VariantPart = 0x33
    | Variable = 0x34
    | VolatileType = 0x35
    | DwarfProcedure = 0x36
    | RestrictType = 0x37
    | InterfaceType = 0x38
    | Namespace = 0x39
    | ImportedModule = 0x3a
    | UnspecifiedType = 0x3b
    | PartialUnit = 0x3c
    | ImportedUnit = 0x3d
    | Condition = 0x3f
    | SharedType = 0x40
    | TypeUnit = 0x41
    | RvalueReferenceType = 0x42
    | TemplateAlias = 0x43

type DwAt =
    | Sibling = 0x01
    | Location = 0x02
    | Name = 0x03
    | Ordering = 0x09
    | ByteSize = 0x0b
    | BitOffset = 0x0c
    | BitSize = 0x0d
    | StmtList = 0x10
    | LowPc = 0x11
    | HighPc = 0x12
    | Language = 0x13
    | Discr = 0x15
    | DiscrValue = 0x16
    | Visibility = 0x17
    | Import = 0x18
    | StringLength = 0x19
    | CommonReference = 0x1a
    | CompDir = 0x1b
    | ConstValue = 0x1c
    | ContainingType = 0x1d
    | DefaultValue = 0x1e
    | Inline = 0x20
    | IsOptional = 0x21
    | LowerBound = 0x22
    | Producer = 0x25
    | Prototyped = 0x27
    | ReturnAddr = 0x2a
    | StartScope = 0x2c
    | BitStride = 0x2e
    | UpperBound = 0x2f
    | AbstractOrigin = 0x31
    | Accessibility = 0x32
    | AddressClass = 0x33
    | Artificial = 0x34
    | BaseTypes = 0x35
    | CallingConvention = 0x36
    | Count = 0x37
    | DataMemberLocation = 0x38
    | DeclColumn = 0x39
    | DeclFile = 0x3a
    | DeclLine = 0x3b
    | Declaration = 0x3c
    | DiscrList = 0x3d
    | Encoding = 0x3e
    | External = 0x3f
    | FrameBase = 0x40
    | Friend = 0x41
    | IdentifierCase = 0x42
    | MacroInfo = 0x43
    | NamelistItem = 0x44
    | Priority = 0x45
    | Segment = 0x46
    | Specification = 0x47
    | StaticLink = 0x48
    | Type = 0x49
    | UseLocation = 0x4a
    | VariableParameter = 0x4b
    | Virtuality = 0x4c
    | VtableElemLocation = 0x4d
    | Allocated = 0x4e
    | Associated = 0x4f
    | DataLocation = 0x50
    | ByteStride = 0x51
    | EntryPc = 0x52
    | UseUTF8 = 0x53
    | Extension = 0x54
    | Ranges = 0x55
    | Trampoline = 0x56
    | CallColumn = 0x57
    | CallFile = 0x58
    | CallLine = 0x59
    | Description = 0x5a
    | BinaryScale = 0x5b
    | DecimalScale = 0x5c
    | Small = 0x5d
    | DecimalSign = 0x5e
    | DigitCount = 0x5f
    | PictureString = 0x60
    | Mutable = 0x61
    | ThreadsScaled = 0x62
    | Explicit = 0x63
    | ObjectPointer = 0x64
    | Endianity = 0x65
    | Elemental = 0x66
    | Pure = 0x67
    | Recursive = 0x68
    | Signature = 0x69
    | MainSubprogram = 0x6a
    | DataBitOffset = 0x6b
    | ConstExpr = 0x6c
    | EnumClass = 0x6d
    | LinkageName = 0x6e

    // CLR extensions
    | ClrManaged = 0x3c00

type DwAte =
    | Address = 0x01
    | Boolean = 0x02
    | ComplexFloat = 0x03
    | Float = 0x04
    | Signed = 0x05
    | SignedChar = 0x06
    | Unsigned = 0x07
    | UnsignedChar = 0x08
    | ImaginaryFloat = 0x09
    | PackedDecimal = 0x0a
    | NumericString = 0x0b
    | Edited = 0x0c
    | SignedFixed = 0x0d
    | UnsignedFixed = 0x0e
    | DecimalFloat = 0x0f
    | Utf = 0x10

type DwForm =
    | Addr = 0x01
    | Block2 = 0x03
    | Block4 = 0x04
    | Data2 = 0x05
    | Data4 = 0x06
    | Data8 = 0x07
    | String = 0x08
    | Block = 0x09
    | Block1 = 0x0a
    | Data1 = 0x0b
    | Flag = 0x0c
    | Sdata = 0x0d
    | Strp = 0x0e
    | Udata = 0x0f
    | RefAddr = 0x10
    | Ref1 = 0x11
    | Ref2 = 0x12
    | Ref4 = 0x13
    | Ref8 = 0x14
    | RefUdata = 0x15
    | Indirect = 0x16
    | SecOffset = 0x17
    | Exprloc = 0x18
    | FlagPresent = 0x19
    | RefSig8 = 0x20

type DwAbbrev =
    {
        tag : DwTag
        hasChildren : bool
        attrs : (DwAt * DwForm) list
    }

type DwTree() =
    member this.Create(tag, ?attrsOpt) =
        DwNode(this, tag, defaultArg attrsOpt [])

and DwNode(tree : DwTree, tag : DwTag, attrs : (DwAt * DwValue) list) =
    let attrs =
        let m = System.Collections.Generic.Dictionary<DwAt, DwValue>()
        for at, value in attrs do
            m.Add(at, value)
        m

    let children = System.Collections.Generic.List<DwNode>()

    let mutable frozen = false

    member this.Tree = tree

    member this.Tag = tag

    member this.Attrs = seq { for kvp in attrs -> (kvp.Key, kvp.Value) }

    member this.Children = children.AsReadOnly()

    member this.AddAttr(at, value) =
        if frozen then
            raise(System.InvalidOperationException())
        attrs.Add(at, value)

    member this.AddAttrs(kvs : #seq<_>) =
        if frozen then
            raise(System.InvalidOperationException())
        for k, v in kvs do
            attrs.Add(k, v)

    member this.AddChild(child : DwNode) =
        if frozen then
            raise(System.InvalidOperationException())
        if child.Tree <> tree then
            raise(System.InvalidOperationException())
        children.Add(child)

    member this.Freeze() =
        if not frozen then
            frozen <- true
            for i = 0 to children.Count - 2 do
                let child = children.[i]
                if child.Children.Count <> 0 then
                    child.AddAttr(DwAt.Sibling, DwValue.Ref(children.[i + 1]))
            for child in children do
                child.Freeze()

and DwValue =
    | Bool of bool
    | Sdata of Int128
    | Udata of UInt128
    | String of string
    | Ref of DwNode

let private section name =
        match name with
        | ".debug_info"
        | ".debug_abbrev"
        | ".debug_loc"
        | ".debug_aranges"
        | ".debug_line" ->
            sprintf "\t.section %s,\"\",@progbits" name
        | ".debug_str" ->
            sprintf "\t.section %s,\"MS\",@progbits,1" name
        | _ -> raise(System.NotSupportedException())

let serialize addrSize root =
    let stringLabelMap = System.Collections.Generic.Dictionary<_, _>()

    let labelOfString s =
        match stringLabelMap.TryGetValue(s) with
        | true, label -> label
        | _ ->
            let label = Asm.createLabel()
            stringLabelMap.Add(s, label)
            label

    let formOfValue (value : DwValue) =
        match value with
        | DwValue.Bool true -> DwForm.FlagPresent // TODO: or Flag
        | DwValue.Bool _ -> DwForm.Flag
        | DwValue.Sdata _ -> DwForm.Sdata
        | DwValue.Udata _ -> DwForm.Udata
        | DwValue.String _ -> DwForm.Strp // TODO: or String
        | DwValue.Ref _ ->
            match addrSize with
            | Addr32 -> DwForm.Ref4
            | Addr64 -> DwForm.Ref8

    let nodeLabelMap = System.Collections.Generic.Dictionary<_, _>()

    let rec pass1 (node : DwNode) =
        if not(nodeLabelMap.ContainsKey(node)) then
            node.Freeze()
            nodeLabelMap.Add(node, Asm.createLabel())
            for at, value in node.Attrs do
                match value with
                | DwValue.Ref node -> pass1 node
                | _ -> ()
            for child in node.Children do
                pass1 child

    pass1 root

    let abbrevMap = System.Collections.Generic.Dictionary<_, _>()
    let abbrevIndex = ref 1u

    let rec pass2 (node : DwNode) =
        let children = node.Children
        let hasChildren = children.Count <> 0
        let abbrev : DwAbbrev =
            {
                tag = node.Tag
                hasChildren = hasChildren
                attrs = [ for at, value in node.Attrs -> (at, formOfValue value) ]
            }
        let index =
            match abbrevMap.TryGetValue(abbrev) with
            | true, index -> index
            | _ ->
                let index = !abbrevIndex
                abbrevMap.Add(abbrev, index)
                abbrevIndex := index + 1u
                index
        seq {
            yield Asm.Label(nodeLabelMap.[node])
            yield Asm.Uleb128(UInt128 index)
            for at, value in node.Attrs do
                match value with
                | DwValue.Bool true -> ()
                | DwValue.Bool x -> yield Asm.U8(if x then 1uy else 0uy)
                | DwValue.Udata x -> yield Asm.Uleb128 x
                | DwValue.Sdata x -> yield Asm.Sleb128 x
                | DwValue.String s -> yield Asm.Expr32(labelOfString s)
                | DwValue.Ref node ->
                    let label = nodeLabelMap.[node]
                    match addrSize with
                    | Addr32 -> yield Asm.Ref32 label
                    | Addr64 -> yield Asm.Ref64 label
            for child in children do
                yield! pass2 child
            if hasChildren then
                yield Asm.U8 0uy
        }

    let dwarfVersion = 4us
    let address_size =
        match addrSize with
        | Addr32 -> 4uy
        | Addr64 -> 8uy

    [
        // .debug_info section
        yield section ".debug_info"

        let infoLines =
            seq {
                // compilation unit header
                yield Asm.Expr32 ".Ldebug_info_end - .Ldebug_info_start"
                yield Asm.Label ".Ldebug_info_start"
                yield Asm.U16 dwarfVersion
                yield Asm.Expr32 ".Ldebug_abbrev"
                yield Asm.U8 address_size

                yield! pass2 root

                yield Asm.Label ".Ldebug_info_end"
            }

        yield! Asm.toStrings infoLines

        // .debug_section section
        yield section ".debug_abbrev"

        let abbrevLines =
            seq {
                yield Asm.Label ".Ldebug_abbrev"
                let abbrevs =
                    abbrevMap
                    |> Seq.sortBy (fun kvp -> kvp.Value)
                for kvp in abbrevs do
                    let abbrev = kvp.Key
                    let index = kvp.Value
                    yield Asm.Uleb128(UInt128 index)
                    yield Asm.Uleb128(UInt128(LanguagePrimitives.EnumToValue abbrev.tag))
                    yield Asm.U8(if abbrev.hasChildren then 1uy else 0uy)
                    for at, form in abbrev.attrs do
                        yield Asm.Uleb128(UInt128(LanguagePrimitives.EnumToValue at))
                        yield Asm.Uleb128(UInt128(LanguagePrimitives.EnumToValue form))
                    yield Asm.U8 0uy
                    yield Asm.U8 0uy
                yield Asm.U8 0uy
            }

        yield! Asm.toStrings abbrevLines

        // .debug_str section
        if stringLabelMap.Count <> 0 then
            yield section ".debug_str"
            let lines =
                seq {
                    for kvp in stringLabelMap do
                        yield Asm.Label kvp.Value
                        yield Asm.String kvp.Key
                }
            yield! Asm.toStrings lines
    ]