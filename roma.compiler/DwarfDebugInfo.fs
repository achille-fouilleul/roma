namespace Roma.Compiler

open Dwarf

type DwarfDebugInfo(addrSize) =
    let compileUnit = CompileUnit(addrSize)
    let typeUnits = MutableList<TypeUnit>()
    
    member this.CompileUnit = compileUnit

    member this.Serialize() =
        seq {
            let stringLabelMap = System.Collections.Generic.Dictionary<_, _>()
            let abbrevMap = System.Collections.Generic.Dictionary<_, _>()
            let abbrevIndex = ref 1u

            let serialize = Dwarf.serialize (stringLabelMap, abbrevMap, abbrevIndex)

            let dwarfVersion = 4us
            let address_size =
                match addrSize with
                | Addr32 -> 4uy
                | Addr64 -> 8uy

            let typeUnitIndexRef = ref 0
            for typeUnit in typeUnits do

                // .debug_types section
                let signature = Dwarf.computeTypeSignature typeUnit.Node
                let typeUnitIndex = !typeUnitIndexRef
                typeUnitIndexRef := typeUnitIndex + 1

                let lineSeq, nodeLabelMap = serialize typeUnit.Node

                yield sprintf "\t.section .debug_types,\"G\",@progbits,wt.%08x,comdat" signature

                yield!
                    seq {
                        // type unit header
                        let startLabel = sprintf ".Ldebug_types%d_start" typeUnitIndex
                        let endLabel = sprintf ".Ldebug_types%d_end" typeUnitIndex
                        yield Asm.Expr32 (sprintf "%s - %s" endLabel startLabel)
                        yield Asm.Label startLabel
                        yield Asm.U16 dwarfVersion
                        yield Asm.Expr32 ".Ldebug_abbrev"
                        yield Asm.U8 address_size
                        yield Asm.U64 signature
                        yield Asm.Ref32(nodeLabelMap.[typeUnit.TypeNode.Id])

                        yield! Seq.toArray lineSeq

                        yield Asm.Label endLabel
                    }
                    |> Asm.toStrings

            // .debug_info section
            yield dwarfSection ".debug_info"

            let lineSeq, _ = serialize compileUnit.Node

            yield!
                seq {
                    // compilation unit header
                    yield Asm.Expr32 ".Ldebug_info_end - .Ldebug_info_start"
                    yield Asm.Label ".Ldebug_info_start"
                    yield Asm.U16 dwarfVersion
                    yield Asm.Expr32 ".Ldebug_abbrev"
                    yield Asm.U8 address_size

                    yield! Seq.toArray lineSeq

                    yield Asm.Label ".Ldebug_info_end"
                }
                |> Asm.toStrings

            // .debug_section section
            yield dwarfSection ".debug_abbrev"

            yield!
                seq {
                    yield Asm.Label ".Ldebug_abbrev"
                    let abbrevs =
                        abbrevMap
                        |> Seq.sortBy (fun kvp -> kvp.Value)
                    for kvp in abbrevs do
                        let abbrev = kvp.Key
                        let index = kvp.Value
                        yield Asm.Uleb128(UInt128 index)
                        yield Asm.Uleb128(UInt128(uint32 abbrev.tag))
                        yield Asm.U8(if abbrev.hasChildren then 1uy else 0uy)
                        for at, form in abbrev.attrs do
                            yield Asm.Uleb128(UInt128(uint32 at))
                            yield Asm.Uleb128(UInt128(uint32 form))
                        yield Asm.U8 0uy
                        yield Asm.U8 0uy
                    yield Asm.U8 0uy
                }
                |> Asm.toStrings

            // .debug_str section
            if stringLabelMap.Count <> 0 then
                yield dwarfSection ".debug_str"
                yield!
                    seq {
                        for kvp in stringLabelMap do
                            yield Asm.Label kvp.Value
                            yield Asm.String kvp.Key
                    }
                    |> Asm.toStrings
        }

