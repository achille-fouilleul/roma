namespace Roma.Cli

open System
open LittleEndian

type ByteReader(array, index) =
    let mutable offset = index

    member this.Offset = offset

    member this.U8() =
        let x = Array.get array offset
        offset <- offset + 1
        x

    member this.U16() =
        let x = u16 array offset
        offset <- offset + 2
        x

    member this.U32() =
        let x = u32 array offset
        offset <- offset + 4
        x

    member this.S32() =
        this.U32() |> int32

type Heaps(stringHeapData, userStringHeapData, blobHeapData, guidHeapData, heapSizes) =
    static let decodeBlob data index =
        match int(Array.get data index) with
        | n when n <= 0x7f -> Array.sub data (index + 1) n
        | n when n <= 0xbf ->
            let n = int(BigEndian.u16 data index) &&& 0x3fff
            Array.sub data (index + 2) n
        | n when n <= 0xdf ->
            let n = int(BigEndian.u32 data index) &&& 0x1fffffff
            Array.sub data (index + 4) n
        | _ -> failwith "invalid blob or user string length"

    let getIndexReader isWide : (ByteReader -> int) =
        if isWide then
            fun byteReader -> byteReader.U32() |> int
        else
            fun byteReader -> byteReader.U16() |> int

    let stringReader =
        let readIndex = getIndexReader ((heapSizes &&& 0x01uy) <> 0uy)
        let cache = System.Collections.Generic.Dictionary<_, _>()
        fun byteReader ->
            let index = readIndex(byteReader)
            let mutable s : string = null
            if not(cache.TryGetValue(index, &s)) then
                let n = Array.IndexOf(stringHeapData, 0uy, index) - index
                let bytes = Array.sub stringHeapData index n
                s <- System.Text.Encoding.UTF8.GetString(bytes)
                cache.Add(index, s)
            s :> obj

    let guidReader =
        let readIndex = getIndexReader ((heapSizes &&& 0x02uy) <> 0uy)
        fun byteReader ->
            let index = readIndex(byteReader)
            let guid =
                if index <> 0 then
                    let off = (index - 1) * 16
                    Guid(Array.sub guidHeapData off 16)
                else
                    Guid()
            guid :> obj

    let blobReader =
        let readIndex = getIndexReader ((heapSizes &&& 0x04uy) <> 0uy)
        let cache = System.Collections.Generic.Dictionary<_, _>()
        fun byteReader ->
            let index = readIndex(byteReader)
            let mutable blob : byte[] = null
            if not(cache.TryGetValue(index, &blob)) then
                blob <- decodeBlob blobHeapData index
                cache.Add(index, blob)
            blob :> obj

    member this.StringReader = stringReader
    member this.GuidReader = guidReader
    member this.BlobReader = blobReader

    member this.ReadUserString(token) =
        if (token &&& 0xff000000u) <> 0x70000000u then
            raise(ArgumentException "invalid string token")
        let blob = decodeBlob userStringHeapData (int(token &&& 0xffffffu))
        if (blob.Length % 2) <> 0 then
            System.Text.Encoding.Unicode.GetString(blob, 0, blob.Length - 1)
        else
            System.Text.Encoding.Unicode.GetString(blob)

module private TableHelpers =
    let private (|SimpleIndexField|CodedIndexField|Other|) (field : System.Reflection.PropertyInfo) =
        let customAttrs =
            field.GetCustomAttributes(true)
            |> Array.choose (
                function
                | :? SimpleIndexAttribute as attr -> Some(SimpleIndexField attr.TableNumber)
                | :? CodedIndexAttribute as attr -> Some(CodedIndexField attr.CodedIndex)
                | _ -> None)
        if customAttrs.Length <> 0 then
            assert(customAttrs.Length = 1)
            assert(field.PropertyType = typeof<uint32>)
            customAttrs.[0]
        else
            Other

    let private getReader (heaps : Heaps) rowCounts table (rowType : Type) field : (ByteReader -> obj) =
        let u16reader (byteReader : ByteReader) =
            byteReader.U16() |> uint32 |> box

        let u32reader (byteReader : ByteReader) =
            byteReader.U32() |> box

        match field with
        | SimpleIndexField tableNumber ->
            if Array.get rowCounts (Tables.tableNumber table) < 65536 then
                u16reader
            else
                u32reader
        | CodedIndexField codedIndex ->
            let fitsIn16Bits table =
                (rowCounts.[Tables.tableNumber table] <<< codedIndex.NTagBits) < 65536
            if Seq.forall fitsIn16Bits codedIndex.TableNumbers then
                u16reader
            else
                u32reader
        | Other ->
            match field.PropertyType with
            | t when t = typeof<string> -> heaps.StringReader
            | t when t = typeof<byte> ->
                fun byteReader ->
                    let x = byteReader.U8()
                    ignore(byteReader.U8())
                    box x
            | t when t = typeof<uint16> -> fun byteReader -> byteReader.U16() |> box
            | t when t = typeof<uint32> -> fun byteReader -> byteReader.U32() |> box
            | t when t = typeof<Guid> -> heaps.GuidReader
            | t when t.IsEnum ->
                match Enum.GetUnderlyingType(t) with
                | ut when ut = typeof<uint16> ->
                    fun byteReader -> Enum.ToObject(t, byteReader.U16())
                | ut when ut = typeof<uint32> ->
                    fun byteReader -> Enum.ToObject(t, byteReader.U32())
                | _ -> raise(InvalidOperationException "internal error")
            | t when t = typeof<byte[]> -> heaps.BlobReader
            | _ -> raise(InvalidOperationException "internal error")

    let makeRowReader heaps rowCounts table rowType =
        let fields = Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(rowType)
        let makeRecord = Tables.getRecordMaker rowType
        let fieldReaders =
                [|
                    for field in fields ->
                        getReader heaps rowCounts table rowType field
                |]
        fun byteReader ->
            let vals =
                [|
                    for fieldReader in fieldReaders ->
                        fieldReader byteReader
                |]
            makeRecord vals

type MetadataReader(peImgReader : PEImageReader) =
    let cliHeader =
        match peImgReader.CliHeader with
        | None -> raise(ArgumentException("CLI header missing"))
        | Some cliHeader -> cliHeader
    let metadata =
        if cliHeader.metaData.IsZero then
            raise(ArgumentException("Metadata missing"))
        peImgReader.Read(cliHeader.metaData)
    do if (u32 metadata 0 <> 0x424a5342u) then
        failwith "Invalid metadata signature"
    let readStream name =
        let x = u32 metadata 12 |> int
        let nStreams = u16 metadata (16 + x + 2) |> int
        let rec findStream i off =
            if i < nStreams then
                let streamOff = u32 metadata off |> int
                let streamSize = u32 metadata (off + 4)
                assert(streamSize > 0u)
                let e = Array.IndexOf(metadata, 0uy, off + 8)
                let streamName = metadata.[off + 8 .. e - 1] |> System.Text.Encoding.ASCII.GetString
                if streamName = name then
                    Array.sub metadata streamOff (int streamSize)
                else
                    findStream (i + 1) ((e + 4) &&& ~~~3)
            else
                Array.empty
        findStream 0 (16 + x + 4)

    let stringHeapData = readStream "#Strings"

    let userStringHeapData = readStream "#US"

    let blobHeapData = readStream "#Blob"

    let guidHeapData = readStream "#GUID"

    let tableStreamData = readStream "#~"
    do
        match tableStreamData.[4], tableStreamData.[5] with
        | 1uy, 0uy | 2uy, 0uy -> ()
        | maj, min -> failwithf "Unsupported version %d.%d of table schemata" maj min

    let heapSizes = tableStreamData.[6]
    let valid = u64 tableStreamData 8

    let heaps = Heaps(stringHeapData, userStringHeapData, blobHeapData, guidHeapData, heapSizes)

    let byteReader = ByteReader(tableStreamData, 24)

    let rowCounts =
        [|
            for i in 0 .. 63 ->
                if valid &&& (1UL <<< i) <> 0UL then
                    let rowCount = byteReader.U32() |> int
                    assert(rowCount <> 0)
                    rowCount
                else
                    0
        |]

    let readTable table : 't[]=
        let rowCount = rowCounts.[Tables.tableNumber table]
        let readRow = TableHelpers.makeRowReader heaps rowCounts table typeof<'t>
        [|
            for i in 1 .. rowCount ->
                readRow byteReader :?> 't
        |]

    let moduleTable : ModuleRow[] = readTable TableNumber.Module
    let typeRefTable : TypeRefRow[] = readTable TableNumber.TypeRef
    let typeDefTable : TypeDefRow[] = readTable TableNumber.TypeDef
    let fieldTable : FieldRow[] = readTable TableNumber.Field
    let methodDefTable : MethodDefRow[] = readTable TableNumber.MethodDef
    let paramTable : ParamRow[] = readTable TableNumber.Param
    let interfaceImplTable : InterfaceImplRow[] = readTable TableNumber.InterfaceImpl
    let memberRefTable : MemberRefRow[] = readTable TableNumber.MemberRef
    let constantTable : ConstantRow[] = readTable TableNumber.Constant
    let customAttributeTable : CustomAttributeRow[] = readTable TableNumber.CustomAttribute
    let fieldMarshalTable : FieldMarshalRow[] = readTable TableNumber.FieldMarshal
    let declSecurityTable : DeclSecurityRow[] = readTable TableNumber.DeclSecurity
    let classLayoutTable : ClassLayoutRow[] = readTable TableNumber.ClassLayout
    let fieldLayoutTable : FieldLayoutRow[] = readTable TableNumber.FieldLayout
    let standAloneSigTable : StandAloneSigRow[] = readTable TableNumber.StandAloneSig
    let eventMapTable : EventMapRow[] = readTable TableNumber.EventMap
    let eventTable : EventRow[] = readTable TableNumber.Event
    let propertyMapTable : PropertyMapRow[] = readTable TableNumber.PropertyMap
    let propertyTable : PropertyRow[] = readTable TableNumber.Property
    let methodSemanticsTable : MethodSemanticsRow[] = readTable TableNumber.MethodSemantics
    let methodImplTable : MethodImplRow[] = readTable TableNumber.MethodImpl
    let moduleRefTable : ModuleRefRow[] = readTable TableNumber.ModuleRef
    let typeSpecTable : TypeSpecRow[] = readTable TableNumber.TypeSpec
    let implMapTable : ImplMapRow[] = readTable TableNumber.ImplMap
    let fieldRVATable : FieldRVARow[] = readTable TableNumber.FieldRVA
    let assemblyTable : AssemblyRow[] = readTable TableNumber.Assembly
    let assemblyProcessorTable : AssemblyProcessorRow[] = readTable TableNumber.AssemblyProcessor
    let assemblyOSTable : AssemblyOSRow[] = readTable TableNumber.AssemblyOS
    let assemblyRefTable : AssemblyRefRow[] = readTable TableNumber.AssemblyRef
    let assemblyRefProcessorTable : AssemblyRefProcessorRow[] = readTable TableNumber.AssemblyRefProcessor
    let assemblyRefOSTable : AssemblyRefOSRow[] = readTable TableNumber.AssemblyRefOS
    let fileTable : FileRow[] = readTable TableNumber.File
    let exportedTypeTable : ExportedTypeRow[] = readTable TableNumber.ExportedType
    let manifestResourceTable : ManifestResourceRow[] = readTable TableNumber.ManifestResource
    let nestedClassTable : NestedClassRow[] = readTable TableNumber.NestedClass
    let genericParamTable : GenericParamRow[] = readTable TableNumber.GenericParam
    let methodSpecTable : MethodSpecRow[] = readTable TableNumber.MethodSpec
    let genericParamConstraintTable : GenericParamConstraintRow[] = readTable TableNumber.GenericParamConstraint

    member this.Heaps = heaps

    member this.ModuleTable = moduleTable
    member this.TypeRefTable = typeRefTable
    member this.TypeDefTable = typeDefTable
    member this.FieldTable = fieldTable
    member this.MethodDefTable = methodDefTable
    member this.ParamTable = paramTable
    member this.InterfaceImplTable = interfaceImplTable
    member this.MemberRefTable = memberRefTable
    member this.ConstantTable = constantTable
    member this.CustomAttributeTable = customAttributeTable
    member this.FieldMarshalTable = fieldMarshalTable
    member this.DeclSecurityTable = declSecurityTable
    member this.ClassLayoutTable = classLayoutTable
    member this.FieldLayoutTable = fieldLayoutTable
    member this.StandAloneSigTable = standAloneSigTable
    member this.EventMapTable = eventMapTable
    member this.EventTable = eventTable
    member this.PropertyMapTable = propertyMapTable
    member this.PropertyTable = propertyTable
    member this.MethodSemanticsTable = methodSemanticsTable
    member this.MethodImplTable = methodImplTable
    member this.ModuleRefTable = moduleRefTable
    member this.TypeSpecTable = typeSpecTable
    member this.ImplMapTable = implMapTable
    member this.FieldRVATable = fieldRVATable
    member this.AssemblyTable = assemblyTable
    member this.AssemblyProcessorTable = assemblyProcessorTable
    member this.AssemblyOSTable = assemblyOSTable
    member this.AssemblyRefTable = assemblyRefTable
    member this.AssemblyRefProcessorTable = assemblyRefProcessorTable
    member this.AssemblyRefOSTable = assemblyRefOSTable
    member this.FileTable = fileTable
    member this.ExportedTypeTable = exportedTypeTable
    member this.ManifestResourceTable = manifestResourceTable
    member this.NestedClassTable = nestedClassTable
    member this.GenericParamTable = genericParamTable
    member this.MethodSpecTable = methodSpecTable
    member this.GenericParamConstraintTable = genericParamConstraintTable

type CILReader(peImgReader : PEImageReader) =
    member this.ReadMethodBody(rva) =
        let hd = peImgReader.[rva]
        let off, codeSize, maxStack, localVarSigToken, initLocals, moreSects =
            match hd &&& 0x03uy with
            | 0x02uy ->
                1u, uint32(hd >>> 2), 8, None, false, false
            | 0x03uy ->
                let hd = ByteReader(peImgReader.Read(rva, 12u), 0)
                let tmp = hd.U16()
                let flags = tmp &&& 0x0fffus
                if (tmp >>> 12) <> 3us then
                    failwith "Invalid method header size."
                let moreSects = (flags &&& 0x8us) <> 0us
                let initLocals = (flags &&& 0x10us) <> 0us
                let maxStack = hd.U16() |> int
                let codeSize = hd.U32()
                let localVarSigTok = hd.U32() |> Tables.tokenOptOfValue 
                uint32 hd.Offset, codeSize, maxStack, localVarSigTok, initLocals, moreSects
            | _ -> failwith "Invalid method header type."
        let codeRva = rva + off
        let codeBytes = peImgReader.Read(codeRva, codeSize)
        let excRva = (codeRva + codeSize + 3u) &&& ~~~3u
        let instrs = codeBytes // TODO: decodeInstructions
        let excClauses =
            if moreSects then
                let flags = peImgReader.[excRva]
                if (flags &&& 0x01uy) <> 0x01uy then
                    failwith "Invalid exception table header."
                if (flags &&& 0x80uy) <> 0x00uy then
                    raise (new System.NotImplementedException())
                let clauses =
                    if (flags &&& 0x40uy) <> 0x00uy then
                        let dataSize = (u32(peImgReader.Read(excRva, 4u)) 0) >>> 8 |> int
                        let n = (dataSize - 4 + 23) / 24
                        let reader = ByteReader(peImgReader.Read(excRva + 4u, uint32(24 * n)), 0)
                        [
                            for k in 1 .. n ->
                                let flags = reader.U32() |> uint16
                                let ranges : ExceptionClauseRanges = {
                                    tryOffset = reader.S32()
                                    tryLength = reader.S32()
                                    handlerOffset = reader.S32()
                                    handlerLength = reader.S32()
                                }
                                let classTokenOrFilterOffset = reader.U32()
                                (flags, ranges, classTokenOrFilterOffset)
                        ]
                    else
                        let dataSize = peImgReader.[excRva + 1u] |> int
                        let n = (dataSize - 4 + 11) / 12
                        let reader = ByteReader(peImgReader.Read(excRva + 4u, uint32(12 * n)), 0)
                        [
                            for k in 1 .. n ->
                                let flags = reader.U16()
                                let ranges : ExceptionClauseRanges = {
                                    tryOffset = reader.U16() |> int
                                    tryLength = reader.U8() |> int
                                    handlerOffset = reader.U16() |> int
                                    handlerLength = reader.U8() |> int
                                }
                                let classTokenOrFilterOffset = reader.U32()
                                (flags, ranges, classTokenOrFilterOffset)
                        ]
                [
                    for (flags, ranges, classTokenOrFilterOffset) in clauses ->
                        match flags with
                        | 0us ->
                            let token = Tables.tokenOfValue classTokenOrFilterOffset
                            ExceptionClause.Catch(ranges, token)
                        | 1us ->
                            let offset = int classTokenOrFilterOffset
                            ExceptionClause.Filter(ranges, offset)
                        | 2us -> ExceptionClause.Finally ranges
                        | 4us -> ExceptionClause.Fault ranges
                        | _ -> failwith "Invalid exception clause flags."
                ]
            else
                []
        let methodBody : MethodBody = {
            maxStack = maxStack
            localVarSigToken = localVarSigToken
            initLocals = initLocals
            excClauses = excClauses
            instrs = instrs
        }
        methodBody
