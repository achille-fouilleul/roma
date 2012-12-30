module Roma.Plang.CodeGen

open Roma.Plang.Parsing
open Roma.Compiler
open Dwarf

let run (defs : TopLevelDef list) =

    let targetAddrSize = Addr32 // TODO: target-dependent

    let cu = CompileUnit(targetAddrSize)
    let globalNs = cu :> INamespace

    Analysis.checkTopLevelConflicts defs

    let enumMap = Analysis.createEnumMap defs
    for (name, (typeExpr, values)) in Map.toSeq enumMap do
        let enumType = globalNs.CreateEnumType(name, cu.GetPrimitiveType(typeExpr))
        for name, value in Map.toSeq values do
            enumType.AddValue(name, value)

    let structs =
        defs |> Seq.choose (
            function
            | TopStruct structDef ->
                Some(structDef, globalNs.CreateStructType(structDef.name))
            | _ -> None
        )

    let typeAliases =
        defs |> Seq.choose (
            function
            | TopTypeAlias typeAliasDef ->
                Some(typeAliasDef, globalNs.CreateTypedef(typeAliasDef.name))
            | _ -> None
        )

    let rec evalConstExpr =
        function
        | _ -> raise(System.NotImplementedException()) // TODO

    and evalTypeExpr =
        function
        | VoidType -> None
        | PrimitiveType kind -> Some(cu.GetPrimitiveType(kind) :> TypeEntry)
        | PointerType(typeExpr, ro) ->
            Some(cu.GetPointerType(evalConstType(typeExpr, ro)) :> TypeEntry)
        | GCRefType typeExpr ->
            let pointedType = evalTypeExpr typeExpr
            raise(System.NotImplementedException()) // TODO
        | ByRefType(typeExpr, ro) ->
            Some(cu.GetReferenceType(evalConstType(typeExpr, ro)) :> TypeEntry)
        | ArrayType(typeExpr, sizeExpr) ->
            let elemType = evalTypeExpr typeExpr
            let size = Option.map evalConstExpr sizeExpr
            Some(cu.GetArrayType(elemType, size) :> TypeEntry)
        | FunctionType(retTypeExpr, paramTypeExprs) ->
            let retType = evalTypeExpr retTypeExpr
            let paramTypes = paramTypeExprs |> List.map evalTypeExpr
            Some(cu.GetSubroutineType(retType, paramTypes) :> TypeEntry)
        | NamedType name -> Some(globalNs.FindType(name))

    and evalConstType(typeExpr, ro) =
        let origType = evalTypeExpr typeExpr
        if ro then
            Some(cu.GetConstType(origType) :> TypeEntry)
        else
            origType

    for (def, entry) in structs do
        for name in Option.toArray def.baseName do
            entry.Inherit(globalNs.FindType(name))
        for (name, _, typeExpr) in def.fields do
            entry.AddMember(name, evalTypeExpr typeExpr)

    for (def, entry) in typeAliases do
        entry.SetReferencedType(evalTypeExpr def.typeExpr)

    cu.WriteTo("-")
