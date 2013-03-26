namespace Roma.Cli.Compiler

open System
open Roma.Cli
open Roma.Compiler
open Roma.Cli.Compiler

type TypeLayout =
    {
        size : uint32
        alignment : uint32
    }

type FieldId =
    | VtablePtr
    | SyncBlock
    | UserField of FieldDef

type FieldLayout =
    {
        field : FieldId
        offset : uint32
    }

type CompositeTypeLayout =
    {
        size : uint32
        alignment : uint32
        baseType : CompositeTypeLayout option
        fields : FieldLayout list
    }

[<AutoOpen>]
module private LayoutPrivate =
    let align alignment value =
        let tmp = value + alignment - 1u
        let r = tmp % alignment
        tmp - r

type TypeLayoutManager(addrSize : AddrSize) =
    let ptrSize =
        match addrSize with
        | Addr32 -> 4u
        | Addr64 -> 8u

    let mutable typeLayoutMap = Map.empty

    let primTypeSize kind =
        match kind with
        | Bool -> 1u
        | Null -> ptrSize
        | Char8 -> 1u
        | Char16 -> 2u
        | Char32 -> 5u
        | SInt8 -> 1u
        | SInt16 -> 2u
        | SInt32 -> 4u
        | SInt64 -> 8u
        | SIntPtr -> ptrSize
        | UInt8 -> 1u
        | UInt16 -> 2u
        | UInt32 -> 4u
        | UInt64 -> 8u
        | UIntPtr -> ptrSize
        | Float32 -> 4u
        | Float64 -> 8u

    member this.GetTypeLayout(t : Type) =
        match t with
        | VoidType -> invalidArg "t" "Cannot compute layout of void type."
        | PrimitiveType kind
        | EnumType(kind, _) ->
            let size = primTypeSize kind
            { TypeLayout.size = size; TypeLayout.alignment = size }
        | ArrayType _ | GCRefType _ | ByRefType _ | PointerType _ ->
            { TypeLayout.size = ptrSize; TypeLayout.alignment = ptrSize }
        | CompositeType info ->
            let layout = this.GetCompositeTypeLayout(info)
            {
                TypeLayout.size = layout.size
                TypeLayout.alignment = layout.alignment
            }
        | VolatileModifier t -> this.GetTypeLayout(t)

    member this.GetCompositeTypeLayout(info : CompositeTypeInfo) =
        match typeLayoutMap.TryFind(info) with
        | Some layout -> layout
        | None ->
            // TODO: value/reference overlap invalid
            // TODO: exact reference/reference overlap unverifiable
            let typeRef = info.typeRef
            let typeDef = typeRef.TypeDef
            if typeDef.IsInterface then
                failwith "Cannot compute layout of this type."

            let addLayout layout =
                typeLayoutMap <- Map.add info layout typeLayoutMap
                layout

            let compute baseLayout =
                let fieldTypes =
                    [
                        for field in info.Fields do
                            if not field.IsStatic then
                                yield field, field.FieldType
                    ]
                let baseSize = match baseLayout with | Some l -> l.size | None -> 0u
                let minSize = match typeDef.classSize with | Some s -> uint32 s | None -> 0u

                match info with
                | _ when typeDef.IsExplicitLayout ->
                    if baseSize <> 0u then
                        failwith "Explicit-layout type cannot have a base type."
                    let fields =
                        [
                            for field, fieldType in fieldTypes ->
                                let layout : FieldLayout =
                                    {
                                        field = FieldId.UserField field.fieldDef
                                        offset = Option.get field.fieldDef.offset
                                    }
                                layout
                        ]
                    let fieldLayouts = [ for _, t in fieldTypes -> this.GetTypeLayout(t) ]
                    let maxAlign = seq { for l in fieldLayouts -> l.alignment } |> Seq.fold max 1u
                    let maxSize = seq { for l in fieldLayouts -> l.size } |> Seq.fold max 0u
                    let alignment =
                        match info.TypeDef.packingSize with
                        | None | Some 0 -> maxAlign
                        | Some s -> min maxAlign (uint32 s)
                    let layout : CompositeTypeLayout =
                        {
                            size = align alignment (max minSize maxSize)
                            alignment = alignment
                            baseType = baseLayout
                            fields = fields
                        }
                    addLayout layout
                | _ ->
                    let mkLayout fieldTypes =
                        let offset = ref baseSize
                        let maxAlign = ref(match baseLayout with | Some l -> l.alignment | None -> 1u)
                        let fields =
                            [
                                for field, fieldType in fieldTypes do
                                    let fieldLayout = this.GetTypeLayout(fieldType)
                                    let fieldAlignment =
                                        match typeDef.packingSize with
                                        | None | Some 0 -> fieldLayout.alignment
                                        | Some s -> min fieldLayout.alignment (uint32 s)
                                    offset := align fieldAlignment !offset
                                    let layout : FieldLayout =
                                        {
                                            field = UserField field.fieldDef
                                            offset = !offset
                                        }
                                    offset := !offset + fieldLayout.size
                                    if !maxAlign < fieldAlignment then
                                        maxAlign := fieldAlignment
                                    yield layout
                            ]
                        let layout : CompositeTypeLayout =
                            {
                                size = align !maxAlign (max minSize !offset)
                                alignment = !maxAlign
                                baseType = baseLayout
                                fields = fields
                            }
                        addLayout layout

                    if typeDef.IsSequentialLayout then
                        mkLayout fieldTypes
                    else
                        fieldTypes
                        |> List.sortBy (fun (field, fieldType) -> this.GetTypeLayout(fieldType).size)
                        |> List.rev
                        |> mkLayout

            let baseTypeOpt = info.BaseType
            match baseTypeOpt with
            | Some baseType ->
                match baseType.typeRef with
                | SysTypeRef("System", "Enum") ->
                    failwith "Cannot compute layout of enum type."
                | SysTypeRef("System", "ValueType") when not(isSysType("System", "Enum") typeRef) ->
                    compute None
                | _ ->
                    baseTypeOpt
                    |> Option.map this.GetCompositeTypeLayout
                    |> compute
            | None ->
                match typeRef with
                | SysTypeRef("System", "Object") ->
                    let vtable : FieldLayout =
                        {
                            field = VtablePtr
                            offset = 0u
                        }
                    let syncBlock : FieldLayout =
                        {
                            field = SyncBlock
                            offset = ptrSize
                        }
                    let layout : CompositeTypeLayout =
                        {
                            size = 2u * ptrSize
                            alignment = ptrSize
                            baseType = None
                            fields = [ vtable; syncBlock ]
                        }
                    addLayout layout
                | _ -> failwith "Base type required."
