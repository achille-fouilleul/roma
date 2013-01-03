module Roma.Plang.CodeGen

open Roma.Plang.Parsing
open Roma.Compiler
open Dwarf
open Analysis

let run (defs : TopLevelDef list) =

    let targetAddrSize = Addr32 // TODO: target-dependent

    let cu = CompileUnit(targetAddrSize)
    let globalNs = cu :> INamespace

    let structMap =
        defs
        |> Seq.choose (
            function
            | TopStruct structDef ->
                Some(structDef, globalNs.CreateStructType(structDef.name))
            | _ -> None
        )
        |> Map.ofSeq

    let enumMap =
        defs
        |> Seq.choose (
            function
            | TopEnum enumDef ->
                Some(enumDef, globalNs.CreateEnumType(enumDef.name))
            | _ -> None
        )
        |> Map.ofSeq

    let depGraph = mkDependencyGraph defs
    let orderedDefs = [ for def, _ in depGraph -> def ]

    let typeAliases =
        orderedDefs |> Seq.choose (
            function
            | TopTypeAlias typeAliasDef ->
                Some(typeAliasDef, globalNs.CreateTypedef(typeAliasDef.name))
            | _ -> None
        )

    let rec evalConstExpr =
        function
        | NumberExpr s -> strToConstant s
        | _ -> raise(System.NotImplementedException()) // TODO

    and evalTypeExpr =
        function
        | VoidType -> None
        | PrimitiveType kind -> Some(cu.GetPrimitiveType(kind) :> TypeEntry)
        | PointerType(typeExpr, ro) ->
            Some(cu.GetPointerType(evalConstModifiedType(typeExpr, ro)) :> TypeEntry)
        | GCRefType typeExpr ->
            let pointedType = evalTypeExpr typeExpr
            raise(System.NotImplementedException()) // TODO
        | ByRefType(typeExpr, ro) ->
            Some(cu.GetReferenceType(evalConstModifiedType(typeExpr, ro)) :> TypeEntry)
        | ArrayType(typeExpr, sizeExpr) ->
            let elemType = evalTypeExpr typeExpr
            let size =
                sizeExpr
                |> Option.map (
                    fun expr ->
                        let r = evalConstExpr expr
                        Checked.uint32(r.Value.ToBigInteger())
                )
            Some(cu.GetArrayType(elemType, size) :> TypeEntry)
        | FunctionType(retTypeExpr, paramTypeExprs) ->
            let retType = evalTypeExpr retTypeExpr
            let paramTypes = paramTypeExprs |> List.map evalTypeExpr
            Some(cu.GetSubroutineType(retType, paramTypes) :> TypeEntry)
        | NamedType name -> Some(globalNs.FindType(name))

    and evalConstModifiedType(typeExpr, ro) =
        let origType = evalTypeExpr typeExpr
        if ro then
            Some(cu.GetConstType(origType) :> TypeEntry)
        else
            origType

    for (def, entry) in typeAliases do
        entry.ReferencedType <- evalTypeExpr def.typeExpr

    for def in orderedDefs do
        match def with
        | TopEnum enumDef ->
            let entry = enumMap.[enumDef]
            let diag = Diagnostics()
            enumDef.values
            |> Seq.groupBy (fun (name, _, _) -> name)
            |> Seq.iter (
                fun (name, xs) ->
                    let xs = Array.ofSeq xs
                    if xs.Length > 1 then
                        for i = 0 to xs.Length - 1 do
                            let (_, pos, desc) = xs.[i]
                            if i = 0 then
                                diag.Error(pos, sprintf "'%s' declared here." name)
                            else
                                diag.Error(pos, sprintf "'%s' redeclared here." name)
            )
            diag.Report()

            let underlyingTypeEntry, (underlyingTypeKind, underlyingTypeSize) =
                let error() =
                    // TODO: report error
                    failwith "Type is not valid as underlying enum type."

                let rec follow (entry : TypeEntry) =
                    match entry with
                    | :? PrimitiveTypeEntry as primEntry ->
                        if IntegerConstant.IsValidKind primEntry.Kind then
                            primEntry.Kind, primEntry.ByteSize
                        else
                            error()
                    | :? TypedefEntry as typedefEntry ->
                        match typedefEntry.ReferencedType with
                        | Some referencedType -> follow referencedType
                        | None -> error()
                    | _ -> error()

                match evalTypeExpr enumDef.underlyingType with
                | Some entry0 -> entry0, follow entry0
                | None -> error()

            entry.SetByteSize(underlyingTypeSize)
            entry.SetUnderlyingType(underlyingTypeEntry)

            let index = ref(IntegerConstant(underlyingTypeKind, Int128.Zero))
            let values =
                seq {
                    for name, pos, exprOpt in enumDef.values do
                        let value =
                            match exprOpt with
                            | None -> !index
                            | Some expr ->
                                let value = evalConstExpr expr
                                // TODO: report error (overflow)
                                IntegerConstant(underlyingTypeKind, value.Value)
                        yield name, value.Value
                        // TODO: report error (overflow)
                        index := IntegerConstant(underlyingTypeKind, value.Value + Int128.One)
                }
            for name, value in values do
                entry.AddValue(name, value)

        | TopStruct structDef ->
            let entry = structMap.[structDef]
            for name in Option.toArray structDef.baseName do
                entry.Inherit(globalNs.FindType(name))
            for (name, _, typeExpr) in structDef.fields do
                // TODO: check against multiple fields with the same name
                entry.AddMember(name, evalTypeExpr typeExpr)

        | TopTypeAlias _ -> ()

        | TopConst _ -> raise(System.NotImplementedException()) // TODO

        | TopVar _ -> raise(System.NotImplementedException()) // TODO

        | TopFun _ -> raise(System.NotImplementedException()) // TODO

    cu.WriteTo("-")
