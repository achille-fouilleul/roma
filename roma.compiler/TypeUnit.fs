namespace Roma.Compiler

open Dwarf

type TypeUnit(addrSize : AddrSize) =
    inherit UnitBase(DwTag.TypeUnit, addrSize)

    let mutable typeNode : DwNode option = None

    member internal this.TypeNode
        with get() =
            Option.get typeNode
        and set value =
            if Option.isSome typeNode then
                raise(System.InvalidOperationException())
            typeNode <- Some value
