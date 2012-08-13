namespace Roma.Cli

open System

open LittleEndian

type CILReader(peImgReader : PEImageReader, ml : IModuleLoader) =
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
                    raise(NotImplementedException())
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
                            ExceptionClause.Catch(ranges, ml.GetTypeRef(token))
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
            locals = ml.GetLocalVarSig(localVarSigToken)
            initLocals = initLocals
            excClauses = excClauses
            instrs = instrs
        }
        methodBody

