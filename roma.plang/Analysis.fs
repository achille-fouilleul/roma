module Roma.Plang.Analysis

open Roma.Compiler
open Scanning
open Parsing

type private List<'t> = System.Collections.Generic.List<'t>

type Diagnostics() =
    let warnings = List<_>()
    let errors = List<_>()

    let report kind (pos : SourcePosition, text) =
        System.Console.Error.WriteLine("{0}:{1}:{2}: {3}: {4}", pos.path, pos.line, pos.pos, kind, text)

    member this.Warning(pos, text) =
        warnings.Add((pos, text))

    member this.Error(pos, text) =
        errors.Add((pos, text))

    member this.Report() =
        for warning in warnings do
            report "warning" warning
        for error in errors do
            report "error" error
        if errors.Count > 0 then
            raise(System.Exception())

let private sortTopologically input =
    let rec loop result input =
        if List.isEmpty input then
            result
        else
            let depsSatisfied (def, deps) =
                deps
                |> Set.forall (
                    fun dep ->
                        result |> List.exists (fun (def, _) -> def = dep)
                )
            let a, b = List.partition depsSatisfied input
            if List.isEmpty a then
                failwith "Cycle detected." // TODO: report error
            loop (result @ a) b
    loop [] input

let mkDependencyGraph (defs : TopLevelDef list) =
    let entityGroups =
        seq {
            for def in defs ->
                let name, pos, desc =
                    match def with
                    | TopEnum enumDef -> enumDef.name, enumDef.pos, "enum"
                    | TopStruct structDef -> structDef.name, structDef.pos, "struct"
                    | TopFun funDef -> funDef.name, funDef.pos, "function"
                    | TopConst constDef -> constDef.name, constDef.pos, "constant"
                    | TopVar varDef -> varDef.name, varDef.pos, "variable"
                    | TopTypeAlias typeAliasDef -> typeAliasDef.name, typeAliasDef.pos, "type alias"
                def, name, pos, desc
        }
        |> Seq.groupBy (fun (_, name, _, _) -> name)

    // check against top-level conflicts

    let diag = Diagnostics()
    let defMap =
        seq {
            for (name, xs) in entityGroups do
                let xs = Array.ofSeq xs
                if xs.Length > 1 then
                    for i = 0 to xs.Length - 1 do
                        let _, _, pos, desc = xs.[i]
                        if i = 0 then
                            diag.Error(pos, sprintf "'%s' declared as %s here." name desc)
                        else
                            diag.Error(pos, sprintf "'%s' redeclared as %s here." name desc)
                else
                    let def, _, _, _ = xs.[0]
                    yield name, def
        }
        |> Map.ofSeq

    diag.Report()

    // build dependency graph

    let globalIdExprDeps name deps =
        let def = Map.find name defMap // TODO: report key-not-found error
        match def with
        | TopEnum _ | TopFun _ | TopConst _ | TopVar _ ->
            Set.add def deps
        | TopStruct _ | TopTypeAlias _ ->
            // TODO: report error
            failwith "Id does not refer to an enum, function, constant, or variable."

    let rec constExprDeps locals expr deps =
        match expr with
        | NullExpr | BoolExpr _ | NumberExpr _ | StringExpr _ -> deps
        | ArrayExpr exprs -> constExprListDeps locals exprs deps
        | IdExpr name ->
            if Set.contains name locals then
                // TODO: report error
                failwith "Reference to local in constant expression."
            else
                globalIdExprDeps name deps
        | IndexExpr(arr, index) ->
            deps
            |> constExprDeps locals arr
            |> constExprDeps locals index
        | CallExpr(f, args) ->
            deps
            |> constExprDeps locals f
            |> constExprListDeps locals args
        | MemberRefExpr(expr, _) -> constExprDeps locals expr deps
        | StructExpr(expr, inits) ->
            deps
            |> constExprDeps locals expr
            |> constExprListDeps locals [ for (_, expr) in inits -> expr ]
        | SizeofExpr typeExpr -> typeExprDeps locals typeExpr deps
        | CastExpr(typeExpr, expr) ->
            deps
            |> typeExprDeps locals typeExpr
            |> constExprDeps locals expr
        | UnExpr(OpPostIncr, _) | UnExpr(OpPostDecr, _)
        | UnExpr(OpPreIncr, _) | UnExpr(OpPreDecr, _) ->
            failwith "Increment/decrement invalid in constant expression." // TODO: report error
        | UnExpr(OpAddr, _) -> deps
        | UnExpr(_, expr) ->
            constExprDeps locals expr deps
        | BinExpr(_, e1, e2) ->
            deps
            |> constExprDeps locals e1
            |> constExprDeps locals e2
        | CondExpr(e1, e2, e3) ->
            deps
            |> constExprDeps locals e1
            |> constExprDeps locals e2
            |> constExprDeps locals e3
        | AssignExpr _ ->
            failwith "Assignment invalid in constant expression." // TODO: report error

    and constExprListDeps locals exprs deps =
        List.fold (fun deps expr -> constExprDeps locals expr deps) deps exprs

    and typeExprDeps locals typeExpr deps =
        match typeExpr with
        | VoidType | PrimitiveType _
        | PointerType _ | GCRefType _ | ByRefType _ -> deps
        | ArrayType(typeExpr, sizeOpt) ->
            deps
            |> typeExprDeps locals typeExpr
            |> constExprListDeps locals (Option.toList sizeOpt)
        | FunctionType(retType, paramTypes) ->
            deps
            |> typeExprDeps locals retType
            |> typeExprListDeps locals paramTypes
        | NamedType name ->
            let def = Map.find name defMap // TODO: report key-not-found error
            match def with
            | TopEnum _ | TopStruct _ -> Set.add def deps
            | TopFun _ | TopConst _ | TopVar _ -> failwith "Id does not refer to an enum or struct type."
            | TopTypeAlias aliasDef -> typeExprDeps locals aliasDef.typeExpr deps // TODO: guard against infinite loops

    and typeExprOptDeps locals typeExprOpt deps =
        Option.fold (fun deps typeExpr -> typeExprDeps locals typeExpr deps) deps typeExprOpt

    and typeExprListDeps locals typeExprs deps =
        List.fold (fun deps typeExpr -> typeExprDeps locals typeExpr deps) deps typeExprs

    let rec statementDeps locals stmt deps =
        match stmt with
        | LabeledStmt(_, stmt) -> statementDeps locals stmt deps
        | CaseStmt(expr, stmt) ->
            deps
            |> constExprDeps locals expr
            |> statementDeps locals stmt
        | DefaultStmt stmt -> statementDeps locals stmt deps
        | CompoundStmt items -> blockItemListDeps locals items deps
        | ExprStmt expr -> exprDeps locals expr deps
        | NullStmt -> deps
        | IfStmt(expr, thenStmt, elseStmtOpt) ->
            deps
            |> exprDeps locals expr
            |> statementDeps locals thenStmt
            |> statementListDeps locals (Option.toList elseStmtOpt)
        | SwitchStmt(expr, stmt) ->
            deps
            |> exprDeps locals expr
            |> statementDeps locals stmt
        | WhileStmt(expr, stmt) ->
            deps
            |> exprDeps locals expr
            |> statementDeps locals stmt
        | DoWhileStmt(stmt, expr) ->
            deps
            |> statementDeps locals stmt
            |> exprDeps locals expr
        | ForStmt(nameOpt, typeOpt, initOpt, condOpt, incrOpt, stmt) ->
            let locals =
                match nameOpt with
                | None -> locals
                | Some name -> locals |> Set.add name
            deps
            |> typeExprOptDeps locals typeOpt
            |> exprOptDeps locals initOpt
            |> exprOptDeps locals condOpt
            |> exprOptDeps locals incrOpt
            |> statementDeps locals stmt
        | GotoStmt _ | ContinueStmt | BreakStmt -> deps
        | ReturnStmt exprOpt -> exprOptDeps locals exprOpt deps
        | TryBlockStmt(stmt, excHandlers) ->
            deps
            |> statementDeps locals stmt
            |> excHandlerListDeps locals excHandlers
        | ThrowStmt exprOpt -> exprOptDeps locals exprOpt deps

    and statementListDeps locals stmts deps =
        List.fold (fun deps stmt -> statementDeps locals stmt deps) deps stmts

    and exprDeps locals expr deps =
        match expr with
        | NullExpr | BoolExpr _ | NumberExpr _ | StringExpr _ -> deps
        | ArrayExpr exprs -> exprListDeps locals exprs deps
        | IdExpr name ->
            if Set.contains name locals then
                deps
            else
                globalIdExprDeps name deps
        | IndexExpr(e1, e2) -> exprListDeps locals [ e1; e2 ] deps
        | CallExpr(f, args) ->
            exprListDeps locals (f :: args) deps
        | MemberRefExpr(expr, _) -> exprDeps locals expr deps
        | StructExpr(expr, inits) ->
            deps
            |> exprDeps locals expr
            |> exprListDeps locals [ for _, expr in inits -> expr ]
        | SizeofExpr typeExpr -> typeExprDeps locals typeExpr deps
        | CastExpr(typeExpr, expr) ->
            deps
            |> typeExprDeps locals typeExpr
            |> exprDeps locals expr
        | UnExpr(_, expr) -> exprDeps locals expr deps
        | BinExpr(_, e1, e2) -> exprListDeps locals [ e1; e2 ] deps
        | CondExpr(e1, e2, e3) -> exprListDeps locals [ e1; e2; e3] deps
        | AssignExpr(_, e1, e2) -> exprListDeps locals [ e1; e2 ] deps

    and exprOptDeps locals exprOpt deps =
        Option.fold (fun deps expr -> exprDeps locals expr deps) deps exprOpt

    and exprListDeps locals exprs deps =
        List.fold (fun deps expr -> exprDeps locals expr deps) deps exprs

    and blockItemListDeps locals blockItems deps =
        let blockItemDeps (locals, deps) blockItem =
            match blockItem with
            | StmtBlockItem stmt ->
                let deps = statementDeps locals stmt deps
                locals, deps
            | DeclBlockItem varDef ->
                let locals = Set.add varDef.name locals
                let deps =
                    deps
                    |> typeExprOptDeps locals varDef.varType
                    |> exprOptDeps locals varDef.init
                locals, deps
        List.fold blockItemDeps (locals, deps) blockItems |> snd

    and excHandlerListDeps locals excHandlers deps =
        let excHandlerDeps deps excHandler =
            match excHandler with
            | CatchHandler(expr, stmt) ->
                deps
                |> exprDeps locals expr
                |> statementDeps locals stmt
            | FinallyHandler stmt ->
                statementDeps locals stmt deps
        List.fold excHandlerDeps deps excHandlers

    let findStructByName name =
        let def = defMap |> Map.find name
        match def with
        | TopStruct _ -> def
        | _ -> failwith "Struct expected." // TODO: report error

    let funDeps (funDef : FunDef) deps =
        match funDef.body with
        | None -> deps
        | Some stmt ->
            let locals = Set.ofList [ for name, _, typeExpr in funDef.pars -> name ]
            statementDeps locals stmt deps

    [
        for _, def in Map.toSeq defMap ->
            let locals = Set.empty
            let deps =
                match def with
                | TopEnum enumDef ->
                    Set.empty
                    |> typeExprDeps locals enumDef.underlyingType
                    |> constExprListDeps locals [ for _, _, expr in enumDef.values do yield! Option.toArray expr ]
                | TopStruct structDef ->
                    Set.empty
                    |> Set.union (Option.toArray structDef.baseName |> Array.map findStructByName |> Set.ofArray)
                    |> typeExprListDeps locals [ for _, _, typeExpr in structDef.fields -> typeExpr ]
                | TopFun funDef ->
                    Set.empty
                    |> typeExprDeps locals funDef.retType
                    |> typeExprListDeps locals [ for _, _, typeExpr in funDef.pars -> typeExpr ]
                    |> funDeps funDef
                | TopConst constDef ->
                    Set.empty
                    |> typeExprListDeps locals (Option.toList constDef.constType)
                    |> constExprDeps locals constDef.init
                | TopVar varDef ->
                    Set.empty
                    |> typeExprListDeps locals (Option.toList varDef.varType)
                    |> constExprListDeps locals (Option.toList varDef.init)
                | TopTypeAlias aliasDef -> Set.empty
                    
            def, deps
    ]
    |> sortTopologically

let private isValidIntegerConstantPrimitiveTypeKind kind =
    // TODO: consider including bool, char{8,16,32}...
    match kind with
    | SInt8 | SInt16 | SInt32 | SInt64
    | UInt8 | UInt16 | UInt32 | UInt64 -> true
    | _ -> false

type IntegerConstant(kind : PrimitiveTypeKind, value : Int128) =
    do
        if not(isValidIntegerConstantPrimitiveTypeKind kind) then
            failwith "Invalid type for integer constant."
        let lo, hi =
            match kind with
            | SInt8 -> Int128(int64(System.SByte.MinValue)), Int128(int64(System.SByte.MaxValue))
            | SInt16 -> Int128(int64(System.Int16.MinValue)), Int128(int64(System.Int16.MaxValue))
            | SInt32 -> Int128(int64(System.Int32.MinValue)), Int128(int64(System.Int32.MaxValue))
            | SInt64 -> Int128(int64(System.Int64.MinValue)), Int128(int64(System.Int64.MaxValue))
            | UInt8 -> Int128.Zero, Int128(uint64(System.Byte.MaxValue))
            | UInt16 -> Int128.Zero, Int128(uint64(System.UInt16.MaxValue))
            | UInt32 -> Int128.Zero, Int128(uint64(System.UInt32.MaxValue))
            | UInt64 -> Int128.Zero, Int128(uint64(System.UInt64.MaxValue))
            | _ -> raise(System.NotSupportedException())
        if not(value >= lo && value <= hi) then
            failwith "Overflow."

    member this.Kind = kind
    member this.Value = value

    static member IsValidKind(kind : PrimitiveTypeKind) =
        isValidIntegerConstantPrimitiveTypeKind kind

let private minSInt32 = bigint(System.Int32.MinValue)
let private maxSInt32 = bigint(System.Int32.MaxValue)
let private maxUInt32 = bigint(System.UInt32.MaxValue)
let private minSInt64 = bigint(System.Int64.MinValue)
let private maxSInt64 = bigint(System.Int64.MaxValue)
let private maxUInt64 = bigint(System.UInt64.MaxValue)

let internal strToConstant (s : string) =
    // TODO: report error
    match System.Numerics.BigInteger.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.NumberFormatInfo.InvariantInfo) with
    | true, value ->
        match value with
        | value when value >= minSInt32 && value <= maxSInt32 ->
            IntegerConstant(SInt32, Int128 value)
        | value when value >= 0I && value <= maxUInt32 ->
            IntegerConstant(UInt32, Int128 value)
        | value when value >= minSInt64 && value <= maxSInt64 ->
            IntegerConstant(SInt64, Int128 value)
        | value when value >= 0I && value <= maxUInt64 ->
            IntegerConstant(UInt64, Int128 value)
        | _ ->
            failwith "Cannot represent integer literal."
    | _ ->
        raise(System.NotImplementedException()) // TODO

let rec private evalConstExpr expr =
    match expr with
    | NumberExpr s -> strToConstant s
    | _ -> raise(System.NotImplementedException()) // TODO
