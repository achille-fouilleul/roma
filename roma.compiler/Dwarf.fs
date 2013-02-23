module Roma.Compiler.Dwarf

open Roma.Compiler

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
    | PtrToMemberType = 0x1f
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

let mutable private nodeId = 0

type DwNode(tag : DwTag, attrs : (DwAt * DwValue) list) =

    let id = System.Threading.Interlocked.Increment(&nodeId)

    let attrs =
        let m = System.Collections.Generic.Dictionary<DwAt, DwValue>()
        for at, value in attrs do
            m.Add(at, value)
        m

    let children = System.Collections.Generic.List<DwNode>()

    let mutable frozen = false

    let mutable parent : DwNode option = None

    member this.Id = id

    member this.Parent
        with get() = parent
        and private set node =
            if frozen then
                raise(System.InvalidOperationException())
            if node = None then
                raise(System.ArgumentNullException())
            if parent <> None then
                raise(System.InvalidOperationException())
            parent <- node

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
        child.Parent <- Some this
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

let internal dwarfSection name =
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

let private nodeName (node : DwNode) =
    node.Attrs
    |> Seq.tryPick (
        function
        | DwAt.Name, DwValue.String s -> Some s
        | _ -> None
    )

let inline private enumBytes e =
    Leb128.encode(uint32(LanguagePrimitives.EnumToValue e))

let private stringBytes (s : string) =
    seq {
        yield! System.Text.Encoding.UTF8.GetBytes(s)
        yield 0uy
    }

let private sigAttrs =
    [
        DwAt.Name
        DwAt.Accessibility
        DwAt.AddressClass
        DwAt.Allocated
        DwAt.Artificial
        DwAt.Associated
        DwAt.BinaryScale
        DwAt.BitOffset
        DwAt.BitSize
        DwAt.BitStride
        DwAt.ByteSize
        DwAt.ByteStride
        DwAt.ConstExpr
        DwAt.ConstValue
        DwAt.ContainingType
        DwAt.Count
        DwAt.DataBitOffset
        DwAt.DataLocation
        DwAt.DataMemberLocation
        DwAt.DecimalScale
        DwAt.DecimalSign
        DwAt.DefaultValue
        DwAt.DigitCount
        DwAt.Discr
        DwAt.DiscrList
        DwAt.DiscrValue
        DwAt.Encoding
        DwAt.EnumClass
        DwAt.Endianity
        DwAt.Explicit
        DwAt.IsOptional
        DwAt.Location
        DwAt.LowerBound
        DwAt.Mutable
        DwAt.Ordering
        DwAt.PictureString
        DwAt.Prototyped
        DwAt.Small
        DwAt.Segment
        DwAt.StringLength
        DwAt.ThreadsScaled
        DwAt.UpperBound
        DwAt.UseLocation
        DwAt.UseUTF8
        DwAt.VariableParameter
        DwAt.Virtuality
        DwAt.Visibility
        DwAt.VtableElemLocation

        // CLR-specific attributes
        DwAt.ClrManaged
    ]

let rec nodeEq (node1 : DwNode) (node2 : DwNode) =
    seq {
        yield node1.Tag = node2.Tag
        yield Seq.length node1.Attrs = Seq.length node2.Attrs
        let atps = Seq.zip node1.Attrs node2.Attrs
        yield atps |> Seq.forall (fun ((at1, val1), (at2, val2)) -> at1 = at2 && valueEq val1 val2)
        yield Seq.length node1.Children = Seq.length node2.Children
        let nps = Seq.zip node1.Children node2.Children
        yield nps |> Seq.forall (fun (a, b) -> nodeEq a b)
    }
    |> Seq.forall id

and valueEq (val1 : DwValue) (val2 : DwValue) =
    match val1, val2 with
    | DwValue.Ref node1, DwValue.Ref node2 -> nodeEq node1 node2
    | _ -> val1 = val2

let computeTypeSignature (typeNode : DwNode) =

    let context (node : DwNode) =
        let rec loop nodeOpt path =
            match nodeOpt with
            | Some node ->
                match nodeName node with
                | Some name -> loop node.Parent ((node.Tag, name) :: path)
                | None -> path
            | _ -> path
 
        let path = loop node.Parent []
        seq {
            for (tag, name) in path do
                yield byte 'C'
                yield! enumBytes tag
                yield! stringBytes name
        }

    let v = MutableList<_>()

    let rec visit (node : DwNode) =
        assert(Seq.forall ((<>) node) v)
        v.Add(node)
        seq {
            yield byte 'D'
            yield! enumBytes node.Tag
            let attrMap =
                let attrMap = node.Attrs |> Map.ofSeq
                match attrMap |> Map.tryFind DwAt.Specification with
                | None -> attrMap
                | Some value ->
                    match value with
                    | DwValue.Ref node ->
                        let specAttrMap = node.Attrs |> Map.ofSeq
                        match specAttrMap |> Map.tryFind DwAt.Declaration with
                        | Some(DwValue.Bool true) -> () // OK
                        | _ -> failwith "DW_AT_specification does not refer to a DIE with DW_AT_declaration."
                        seq {
                            yield! Map.toSeq specAttrMap
                            yield! Map.toSeq attrMap
                        }
                        |> Map.ofSeq
                    | _ -> failwith "DW_AT_specification must refer to another DIE."

            let processAttr at =
                seq {
                    match attrMap |> Map.tryFind at with
                    | None -> ()
                    | Some value ->
                        let addAttr marker form xs =
                            seq {
                                yield byte marker
                                yield! enumBytes at
                                yield! enumBytes form
                                yield! xs
                            }

                        match value with
                        | DwValue.Bool x ->
                            yield! addAttr 'A' DwForm.Sdata (Leb128.encode(if x then 1uy else 0uy))
                        | DwValue.Sdata x ->
                            yield! addAttr 'A' DwForm.Sdata (Leb128.encode x)
                        | DwValue.Udata x ->
                            yield! addAttr 'A' DwForm.Udata (Leb128.encode x)
                        | DwValue.String x ->
                            yield! addAttr 'A' DwForm.String (stringBytes x)
                        | DwValue.Ref node ->
                            match v |> Seq.tryFindIndex (nodeEq node) with
                            | Some index ->
                                yield byte 'R'
                                yield! enumBytes at
                                yield! Leb128.encode(uint32 index)
                            | None ->
                                yield byte 'T'
                                yield! enumBytes at
                                yield! context node
                                yield! visit node
                }

            for at in sigAttrs do
                yield! processAttr at

            match node.Tag with
            | DwTag.PointerType
            | DwTag.ReferenceType
            | DwTag.RvalueReferenceType
            | DwTag.PtrToMemberType ->
                (*
                If the tag in Step 3 is one of DW_TAG_pointer_type, DW_TAG_reference_type,
                DW_TAG_rvalue_reference_type, DW_TAG_ptr_to_member_type, or DW_TAG_friend,
                and the referenced type (via the DW_AT_type or DW_AT_friend attribute) has
                a DW_AT_name attribute, append to S the letter 'N', the DWARF attribute
                code (DW_AT_type or DW_AT_friend), the context of the type (according to
                the method in Step 2), the letter 'E', and the name of the type. For
                DW_TAG_friend, if the referenced entry is a DW_TAG_subprogram, the context
                is omitted and the name to be used is the ABI-specific name of the
                subprogram (e.g., the mangled linker name).
                If the tag in Step 3 is not one of DW_TAG_pointer_type,
                DW_TAG_reference_type, DW_TAG_rvalue_reference_type,
                DW_TAG_ptr_to_member_type, or DW_TAG_friend, but has a
                DW_AT_type attribute, or if the referenced type (via the DW_AT_type or
                DW_AT_friend attribute) does not have a DW_AT_name attribute, the
                attribute is processed according to the method in Step 4 for an attribute
                that refers to another type entry.
                *)
                match attrMap |> Map.tryFind DwAt.Type with
                | None -> ()
                | Some(DwValue.Ref node) ->
                    match nodeName node with
                    | Some name ->
                        yield byte 'N'
                        yield! enumBytes DwAt.Type
                        yield! context node
                        yield byte 'E'
                        yield! stringBytes name
                    | None ->
                        yield! processAttr DwAt.Type
                | _ -> failwith "DW_AT_type must refer to a type."
            | DwTag.Friend ->
                raise(System.NotImplementedException()) // TODO
            | _ ->
                yield! processAttr DwAt.Type

            for child in node.Children do
                (*
                Visit each child C of the debugging information entry as follows: If C is
                a nested type entry or a member function entry, and has a DW_AT_name
                attribute, append to S the letter 'S', the tag of C, and its name;
                otherwise, process C recursively by performing Steps 3 through 7,
                appending the result to S.
                *)
                match nodeName child with
                | Some name ->
                    match child.Tag with
                    | DwTag.ArrayType
                    | DwTag.StructureType
                    | DwTag.ClassType
                    | DwTag.UnionType
                    | DwTag.EnumerationType
                    | DwTag.PointerType
                    | DwTag.ReferenceType
                    // TODO: other type tags
                    | DwTag.Subprogram ->
                        yield byte 'S'
                        yield! enumBytes child.Tag
                        yield! stringBytes name
                    | _ -> yield! visit child
                | None -> yield! visit child

            yield 0uy
        }

    let s = 
        seq {
            yield! context typeNode
            yield! visit typeNode
        }
        |> Seq.toArray

    if false then
        s
        |> Seq.map (sprintf "%02x")
        |> String.concat " "
        |> printfn "%s"

    let hash =
        use md5 = System.Security.Cryptography.MD5.Create()
        md5.ComputeHash(s)
    Array.sub hash 8 8
    |> Array.fold (fun n x -> (n <<< 8) ||| (uint64 x)) 0UL

let serialize (stringLabelMap : MutableMap<_, _>, abbrevMap : MutableMap<_, _>, abbrevIndex) root =

    let labelOfString s =
        match stringLabelMap.TryGetValue(s) with
        | true, label -> label
        | _ ->
            let label = Asm.createLabel()
            stringLabelMap.Add(s, label)
            label

    let nodeLabelMap = MutableMap<_, _>()
    let nodeSet = MutableSet<_>()

    let rec pass1 (node : DwNode) =
        node.Freeze()
        if not(nodeLabelMap.ContainsKey(node.Id)) then
            nodeLabelMap.Add(node.Id, Asm.createLabel())
        if nodeSet.Add(node.Id) then
            for at, value in node.Attrs do
                match value with
                | DwValue.Ref node ->
                    node.Freeze()
                    if not(nodeLabelMap.ContainsKey(node.Id)) then
                        nodeLabelMap.Add(node.Id, Asm.createLabel())
                | _ -> ()
            for child in node.Children do
                pass1 child

    pass1 root

    let rec pass2 (node : DwNode) =
        let children = node.Children
        let hasChildren = children.Count <> 0
        let abbrev : DwAbbrev =
            {
                tag = node.Tag
                hasChildren = hasChildren
                attrs =
                    [
                        for at, value in node.Attrs ->
                            let form =
                                match value with
                                | DwValue.Bool true -> DwForm.FlagPresent // TODO: or Flag
                                | DwValue.Bool _ -> DwForm.Flag
                                | DwValue.Sdata _ -> DwForm.Sdata
                                | DwValue.Udata _ -> DwForm.Udata
                                | DwValue.String s -> if s.Length <= 3 then DwForm.String else DwForm.Strp
                                | DwValue.Ref node -> if nodeSet.Contains(node.Id) then DwForm.Ref4 else DwForm.RefSig8
                            at, form
                    ]
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
            yield Asm.Label(nodeLabelMap.[node.Id])
            yield Asm.Uleb128(UInt128 index)
            for at, value in node.Attrs do
                match value with
                | DwValue.Bool true -> ()
                | DwValue.Bool x -> yield Asm.U8(if x then 1uy else 0uy)
                | DwValue.Udata x -> yield Asm.Uleb128 x
                | DwValue.Sdata x -> yield Asm.Sleb128 x
                | DwValue.String s ->
                    if s.Length <= 3 then
                        yield Asm.String(s)
                    else
                        yield Asm.Expr32(labelOfString s)
                | DwValue.Ref node ->
                    if nodeSet.Contains(node.Id) then
                        yield Asm.Ref32(nodeLabelMap.[node.Id])
                    else
                        let sig8 = computeTypeSignature node
                        for i = 0 to 7 do
                            yield Asm.U8(byte((sig8 >>> ((7 - i) * 8)) &&& 0xffUL))
            for child in children do
                yield! pass2 child
            if hasChildren then
                yield Asm.U8 0uy
        }

    pass2 root, nodeLabelMap
