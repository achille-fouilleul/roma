module Roma.Plang.CodeGen

open Roma.Plang.Parsing
open Roma.Plang.Analysis
open Roma.Compiler
open Dwarf

let run (defs : TopLevelDef list) =

    let targetAddrSize = Addr32 // TODO: target-dependent

    let cu = CompileUnit(targetAddrSize)
    let globalNs = cu :> INamespace

    let enumMap = Analysis.createEnumMap defs
    for (name, (typeExpr, values)) in Map.toSeq enumMap do
        globalNs.Enums.Add(EnumTypeEntry(name, cu.GetPrimitiveType(typeExpr), Map.toList values))

    cu.WriteTo("-")
