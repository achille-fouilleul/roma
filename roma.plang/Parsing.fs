﻿module Roma.Plang.Parsing

open Roma.Plang.Scanning

type private List<'t> = System.Collections.Generic.List<'t>

type UnOp =
    | OpPostIncr
    | OpPostDecr
    | OpPreIncr
    | OpPreDecr
    | OpAddr
    | OpDeref
    | OpPos
    | OpNeg
    | OpBitNot
    | OpNot

type BinOp =
    | OpMul
    | OpDiv
    | OpMod
    | OpAdd
    | OpSub
    | OpLsh
    | OpRsh
    | OpLT
    | OpGT
    | OpLE
    | OpGE
    | OpEq
    | OpNE
    | OpBitAnd
    | OpBitXor
    | OpBitOr
    | OpAnd
    | OpOr

type AssignOp =
    | OpSimpleAssign
    | OpMulAssign
    | OpDivAssign
    | OpModAssign
    | OpAddAssign
    | OpSubAssign
    | OpLshAssign
    | OpRshAssign
    | OpAndAssign
    | OpOrAssign
    | OpXorAssign

type PrimitiveTypeKind =
    | Bool
    | Null
    | Char8
    | Char16
    | Char32
    | SInt8
    | SInt16
    | SInt32
    | SInt64
    | SIntPtr
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | UIntPtr
    | Float32
    | Float64

type Expr =
    | NullExpr
    | BoolExpr of bool
    | NumberExpr of string // TODO
    | StringExpr of string
    | ArrayExpr of Expr list
    | IdExpr of string
    | IndexExpr of Expr * Expr
    | CallExpr of Expr * Expr list
    | StructExpr of Expr * (string * Expr) list
    | SizeofExpr of TypeExpr
    | UnExpr of UnOp * Expr
    | BinExpr of BinOp * Expr * Expr
    | CondExpr of Expr * Expr * Expr
    | AssignExpr of AssignOp * Expr * Expr

and TypeExpr =
    | VoidType
    | PrimitiveType of PrimitiveTypeKind
    | PointerType of TypeExpr * bool (* readonly *)
    | GCRefType of TypeExpr
    | ByRefType of TypeExpr * bool (* readonly *)
    | ArrayType of TypeExpr * Expr option
    | FunctionType of TypeExpr * TypeExpr list
    | TypeName of string

type EnumDef =
    {
        name : string
        pos : SourcePosition
        underlyingType : TypeExpr
        values : (string * Expr option) list
    }

type StructDef =
    {
        name : string
        pos : SourcePosition
        baseName : string option
        fields : (string * TypeExpr) list
    }

type VarDef =
    {
        name : string
        pos : SourcePosition
        varType : TypeExpr option
        init : Expr option
    }

type Statement =
    | LabeledStmt of string * Statement
    | CaseStmt of Expr * Statement
    | DefaultStmt of Statement
    | CompoundStmt of BlockItem list
    | ExprStmt of Expr
    | NullStmt
    | IfStmt of Expr * Statement * Statement option
    | SwitchStmt of Expr * Statement
    | WhileStmt of Expr * Statement
    | DoWhileStmt of Statement * Expr
    | ForStmt of string option * TypeExpr option * Expr option * Expr option * Expr option * Statement
    | GotoStmt of string
    | ContinueStmt
    | BreakStmt
    | ReturnStmt of Expr option
    | TryBlockStmt of Statement * ExceptionHandler list
    | ThrowStmt of Expr option

and BlockItem =
    | StmtBlockItem of Statement
    | DeclBlockItem of VarDef

and ExceptionHandler =
    | CatchHandler of Expr * Statement
    | FinallyHandler of Statement

type FunDef =
    {
        name : string
        pos : SourcePosition
        pars : (string * SourcePosition * TypeExpr) list
        retType : TypeExpr
        body : Statement option
    }

type TopLevelDef =
    | TopEnum of EnumDef
    | TopStruct of StructDef
    | TopFun of FunDef
    | TopVar of VarDef

let (|ParseOpt|_|) f (tokens : Token list) =
    f tokens

let private error (token : Token) text =
    let sp = token.pos
    failwithf "%s:%d:%d: %s" sp.path sp.line sp.pos text

let private errorUnexpectedStr actToks (expStr : string) =
    match actToks with
    | [] ->
        // TODO: include file name in error message
        failwithf "Unexpected end of file; expected %s." expStr
    | actTok :: _ ->
        error actTok (sprintf "expected %s; got %s." expStr (tokenKindToStr actTok.value))

let private errorUnexpected actToks expTok =
    errorUnexpectedStr actToks (tokenKindToStr expTok)

let parseList parseItemOpt itemDesc sep (tokens : Token list) =
    match parseItemOpt tokens with
    | None -> tokens, []
    | Some(tokens, first) ->
        let buf = List<_>()
        buf.Add(first)
        let rec loop tokens =
            match tokens with
            | { value = value } :: tokens when value = sep ->
                match parseItemOpt tokens with
                | Some(tokens, item) ->
                    buf.Add(item)
                    loop tokens
                | None -> errorUnexpectedStr tokens itemDesc
            | _ ->
                tokens, List.ofSeq buf
        loop tokens

let private expect expTok tokens =
    match tokens with
    | actTok :: tokens' when actTok.value = expTok -> tokens'
    | _ -> errorUnexpected tokens expTok

let private expectId tokens =
    match tokens with
    | { pos = pos; value = TokId(name) } :: tl -> tl, name, pos
    | _ -> errorUnexpectedStr tokens "identifier"

let private parseReadonly tokens =
    match tokens with
    | { value = TokReadonly } :: tokens' -> tokens', true
    | _ -> tokens, false

let private postIncrOpMap =
    [
        TokPlusPlus, OpPostIncr
        TokMinusMinus, OpPostDecr
    ]
    |> Map.ofList

let private preIncrOpMap =
    [
        TokPlusPlus, OpPreIncr
        TokMinusMinus, OpPreDecr
    ]
    |> Map.ofList

let private unOpMap =
    [
        TokAnd, OpAddr
        TokStar, OpDeref
        TokPlus, OpPos
        TokMinus, OpNeg
        TokNot, OpBitNot
        TokExcl, OpNot
    ]
    |> Map.ofList

let private mulOpMap =
    [
        TokStar, OpMul
        TokSlash, OpDiv
        TokMod, OpMod
    ]
    |> Map.ofList

let private addOpMap =
    [
        TokPlus, OpAdd
        TokMinus, OpSub
    ]
    |> Map.ofList

let private shiftOpMap =
    [
        TokLsh, OpLsh
        TokRsh, OpRsh
    ]
    |> Map.ofList

let private relOpMap =
    [
        TokLT, OpLT
        TokGT, OpGT
        TokLE, OpLE
        TokGE, OpGE
    ]
    |> Map.ofList

let private eqOpMap =
    [
        TokEqEq, OpEq
        TokNeq, OpNE
    ]
    |> Map.ofList

let private bitAndOpMap = Map.ofList [ TokAnd, OpBitAnd ]

let private bitXorOpMap = Map.ofList [ TokHat, OpBitXor ]

let private bitOrOpMap = Map.ofList [ TokOr, OpBitOr ]

let private andOpMap = Map.ofList [ TokAndAnd, OpAnd ]

let private orOpMap = Map.ofList [ TokOrOr, OpOr ]

let private assignOpMap =
    [
        TokEq, OpSimpleAssign
        TokStarEq, OpMulAssign
        TokSlashEq, OpDivAssign
        TokModEq, OpModAssign
        TokPlusEq, OpAddAssign
        TokMinusEq, OpSubAssign
        TokLshEq, OpLshAssign
        TokRshEq, OpRshAssign
        TokAndEq, OpAndAssign
        TokOrEq, OpOrAssign
        TokHatEq, OpXorAssign
    ]
    |> Map.ofList

let private primitiveTypeMap =
    [
        TokBool, PrimitiveTypeKind.Bool
        TokNull_t, PrimitiveTypeKind.Null
        TokChar8, PrimitiveTypeKind.Char8
        TokChar16, PrimitiveTypeKind.Char16
        TokChar32, PrimitiveTypeKind.Char32
        TokSInt8, PrimitiveTypeKind.SInt8
        TokSInt16, PrimitiveTypeKind.SInt16
        TokSInt32, PrimitiveTypeKind.SInt32
        TokSInt64, PrimitiveTypeKind.SInt64
        TokSIntPtr, PrimitiveTypeKind.SIntPtr
        TokUInt8, PrimitiveTypeKind.UInt8
        TokUInt16, PrimitiveTypeKind.UInt16
        TokUInt32, PrimitiveTypeKind.UInt32
        TokUInt64, PrimitiveTypeKind.UInt64
        TokUIntPtr, PrimitiveTypeKind.UIntPtr
        TokFloat32, PrimitiveTypeKind.Float32
        TokFloat64, PrimitiveTypeKind.Float64
    ]
    |> Map.ofList

let private parseBinOp opMap parseInner tokens =
    match parseInner tokens with
    | None -> None
    | Some(tokens, expr1) ->
        let rec loop(tokens, expr1) =
            let tokens, opOpt =
                match tokens with
                | { value = value } :: tokens' ->
                    match Map.tryFind value opMap with
                    | None -> tokens, None
                    | Some op -> tokens', Some op
                | _ -> tokens, None
            match opOpt with
            | Some op ->
                match parseInner tokens with
                | Some(tokens, expr2) ->
                    loop(tokens, BinExpr(op, expr1, expr2))
                | None -> errorUnexpectedStr tokens "expression"
            | None -> Some(tokens, expr1)
        loop(tokens, expr1)

let rec private parsePrimaryExprOpt tokens =
    match tokens with
    | { value = TokNull } :: tokens ->
        Some(tokens, NullExpr)
    | { value = TokFalse } :: tokens ->
        Some(tokens, BoolExpr false)
    | { value = TokTrue } :: tokens ->
        Some(tokens, BoolExpr true)
    | { value = TokNumber s } :: tokens ->
        Some(tokens, NumberExpr s)
    | { value = TokString s } :: tokens ->
        Some(tokens, StringExpr s)
    | { value = TokId s } :: tokens ->
        Some(tokens, IdExpr s)
    | { value = TokLParen } :: tokens ->
        let tokens, expr = parseExpr tokens
        let tokens = expect TokRParen tokens
        Some(tokens, expr)
    | { value = TokLBrace } :: tokens ->
        let tokens, exprs = parseList parseExprOpt "expression" TokComma tokens
        let tokens = expect TokRBrace tokens
        Some(tokens, ArrayExpr exprs)
    | _ -> None

and private parsePostfixExprOpt tokens =
    match parsePrimaryExprOpt tokens with
    | None -> None
    | Some(tokens, expr) ->
        let rec loop(tokens, expr) =
            match tokens with
            | { value = TokLBracket } :: tokens ->
                let tokens, indexExpr = parseExpr tokens
                let tokens = expect TokRBracket tokens
                loop(tokens, IndexExpr(expr, indexExpr))
            | { value = TokLParen } :: tokens ->
                let tokens, args = parseList parseExprOpt "argument" TokComma tokens
                let tokens = expect TokRParen tokens
                loop(tokens, CallExpr(expr, args))
            | { value = TokPeriod } :: tokens
            | { value = TokArrow } :: tokens ->
                raise(System.NotImplementedException()) // TODO
            | { value = value } :: tokens when postIncrOpMap.ContainsKey(value) ->
                loop(tokens, UnExpr(postIncrOpMap.[value], expr))
            | { value = TokLBrace } :: tokens ->
                let tokens, inits = 
                    let xs = List<_>()
                    let rec loop tokens =
                        match tokens with
                        | { value = TokPeriod } :: { value = TokId name } :: { value = TokEq } :: tokens ->
                            let tokens, expr = parseExpr tokens
                            xs.Add(name, expr)
                            match tokens with
                            | { value = TokComma } :: tokens ->
                                match tokens with
                                | { value = TokRBrace } :: _ -> tokens
                                | _ -> loop tokens
                            | _ -> tokens
                        | _ -> tokens
                    let tokens = loop tokens
                    tokens, List.ofSeq xs
                let tokens = expect TokRBrace tokens
                loop(tokens, StructExpr(expr, inits))
            | _ ->
                Some(tokens, expr)
        loop(tokens, expr)

and private parseUnaryExprOpt tokens =
    match tokens with
    | { value = value } :: tokens when preIncrOpMap.ContainsKey(value) ->
        match parseUnaryExprOpt tokens with
        | None -> errorUnexpectedStr tokens "expression"
        | Some(tokens, expr) ->
            Some(tokens, UnExpr(preIncrOpMap.[value], expr))
    | { value = value } :: tokens when unOpMap.ContainsKey(value) ->
        match parseCastExprOpt tokens with
        | None -> errorUnexpectedStr tokens "expression"
        | Some(tokens, expr) ->
            Some(tokens, UnExpr(unOpMap.[value], expr))
    | { value = TokSizeof } :: tokens ->
        let tokens = expect TokLParen tokens
        let tokens, typeExpr = parseTypeExpr tokens
        let tokens = expect TokRParen tokens
        Some(tokens, SizeofExpr(typeExpr))
    | _ ->
        parsePostfixExprOpt tokens

and private parseCastExprOpt tokens =
    match tokens with
    // TODO: casts
    | _ -> parseUnaryExprOpt tokens

and private parseMulExprOpt tokens =
    parseBinOp mulOpMap parseCastExprOpt tokens

and private parseAddExprOpt tokens =
    parseBinOp addOpMap parseMulExprOpt tokens

and private parseShiftExprOpt tokens =
    parseBinOp shiftOpMap parseAddExprOpt tokens

and private parseRelExprOpt tokens =
    parseBinOp relOpMap parseShiftExprOpt tokens

and private parseEqExprOpt tokens =
    parseBinOp eqOpMap parseRelExprOpt tokens

and private parseBitAndExprOpt tokens =
    parseBinOp bitAndOpMap parseEqExprOpt tokens

and private parseBitXorExprOpt tokens =
    parseBinOp bitXorOpMap parseBitAndExprOpt tokens

and private parseBitOrExprOpt tokens =
    parseBinOp bitOrOpMap parseBitXorExprOpt tokens

and private parseAndExprOpt tokens =
    parseBinOp andOpMap parseBitOrExprOpt tokens

and private parseOrExprOpt tokens =
    parseBinOp orOpMap parseAndExprOpt tokens

and private parseCondExprOpt tokens =
    match parseOrExprOpt tokens with
    | None -> None
    | Some(tokens, expr) ->
        match tokens with
        | { value = TokQues } :: tokens ->
            let tokens, thenExpr = parseExpr tokens
            let tokens = expect TokColon tokens
            match parseCondExprOpt tokens with
            | None -> errorUnexpectedStr tokens "expression"
            | Some(tokens, elseExpr) ->
                Some(tokens, CondExpr(expr, thenExpr, elseExpr))
        | _ -> Some(tokens, expr)

and private parseExprOpt tokens =
    match parseUnaryExprOpt tokens with
    | None -> None
    | Some(tokens', expr1) ->
        match tokens' with
        | { value = value } :: tokens when assignOpMap.ContainsKey(value) ->
            let tokens, expr2 = parseExpr tokens
            Some(tokens, AssignExpr(assignOpMap.[value], expr1, expr2))
        | _ -> parseCondExprOpt tokens

and private parseExpr tokens =
    match parseExprOpt tokens with
    | Some result -> result
    | None -> errorUnexpectedStr tokens "expression"

and private parseTypeExprOpt tokens =
    match tokens with
    | { value = TokVoid } :: tokens ->
        Some(tokens, TypeExpr.VoidType)
    | { value = value } :: tokens when primitiveTypeMap.ContainsKey(value) ->
        Some(tokens, TypeExpr.PrimitiveType(primitiveTypeMap.[value]))
    | { value = TokStar } :: tokens ->
        let tokens, ro = parseReadonly tokens
        let tokens, typeExpr = parseTypeExpr tokens
        Some(tokens, TypeExpr.PointerType(typeExpr, ro))
    | { value = TokHat } :: tokens ->
        let tokens, typeExpr = parseTypeExpr tokens
        Some(tokens, TypeExpr.GCRefType typeExpr)
    | { value = TokAnd } :: tokens ->
        let tokens, ro = parseReadonly tokens
        let tokens, typeExpr = parseTypeExpr tokens
        Some(tokens, TypeExpr.ByRefType(typeExpr, ro))
    | { value = TokLBracket } :: tokens ->
        let tokens, exprOpt =
            match tokens with
            | ParseOpt parseExprOpt (tokens, expr) -> tokens, Some expr
            | _ -> tokens, None
        let tokens = expect TokRBracket tokens
        let tokens, elemType = parseTypeExpr tokens
        Some(tokens, TypeExpr.ArrayType(elemType, exprOpt))
    | { value = TokFun } :: tokens ->
        let tokens = expect TokLParen tokens
        let tokens, paramTypes = parseList parseAbstractParamOpt "parameter" TokComma tokens
        let tokens = expect TokRParen tokens
        let tokens, retType = parseTypeAnnotation tokens
        Some(tokens, TypeExpr.FunctionType(retType, paramTypes))
    | { value = TokId name } :: tokens ->
        Some(tokens, TypeExpr.TypeName name)
    | _ -> None

and private parseTypeExpr tokens =
    match parseTypeExprOpt tokens with
    | Some result -> result
    | None -> errorUnexpectedStr tokens "type expression"

and private parseTypeAnnotationOpt tokens =
    match tokens with
    | { value = TokColon } :: tokens -> Some(parseTypeExpr tokens)
    | _ -> None

and private parseTypeAnnotation tokens =
    match parseTypeAnnotationOpt tokens with
    | Some result -> result
    | None -> errorUnexpectedStr tokens "type annotation"
    
and private parseAbstractParamOpt tokens =
    match tokens with
    | { value = TokId _ } :: { value = TokColon } :: tokens ->
        // ignore name for now
        let tokens, paramType = parseTypeExpr tokens
        Some(tokens, paramType)
    | _ -> parseTypeExprOpt tokens

let private parseEnumValueOpt tokens =
    match tokens with
    | { value = TokId name } :: tokens ->
        let tokens, exprOpt =
            match tokens with
            | { value = TokEq } :: tokens ->
                let tokens, expr = parseExpr tokens
                tokens, Some expr
            | _ -> tokens, None
        Some(tokens, (name, exprOpt))
    | _ -> None

let private parseEnumOpt tokens =
    match tokens with
    | { value = TokEnum } :: tokens ->
        let tokens, name, pos = expectId tokens
        let tokens, typeExpr = parseTypeAnnotation tokens
        let tokens = expect TokLBrace tokens
        let tokens, values = parseList parseEnumValueOpt "enum value" TokComma tokens
        let tokens = expect TokRBrace tokens
        let enumDef : EnumDef =
            {
                name = name
                pos = pos
                underlyingType = typeExpr
                values = values
            }
        Some(tokens, enumDef)
    | _ -> None

let private parseStructOpt tokens =
    match tokens with
    | { value = TokStruct } :: tokens ->
        let tokens, name, pos = expectId tokens
        let tokens, baseName =
            match tokens with
            | { value = TokColon } :: tokens ->
                let tokens, name, pos (* TODO: use pos *) = expectId tokens
                tokens, Some name
            | _ -> tokens, None
        let tokens = expect TokLBrace tokens
        let fields = List<_>()
        let rec loop tokens =
            match tokens with
            | { value = TokId name; pos = pos (* TODO: use pos *) } :: tokens ->
                let tokens, typeExpr = parseTypeAnnotation tokens
                fields.Add(name, typeExpr)
                let tokens = expect TokSemicolon tokens
                loop tokens
            | _ -> tokens
        let tokens = loop tokens
        let tokens = expect TokRBrace tokens
        let structDef : StructDef =
            {
                name = name
                pos = pos
                baseName = baseName
                fields = List.ofSeq fields
            }
        Some(tokens, structDef)
    | _ -> None

let private parseVarOpt tokens =
    match tokens with
    | { value = TokVar } :: tokens ->
        let tokens, ro = parseReadonly tokens
        let tokens, name, pos = expectId tokens
        let tokens, typeExprOpt =
            match tokens with
            | ParseOpt parseTypeAnnotationOpt (tokens, typeExpr) ->
                tokens, Some typeExpr
            | _ -> tokens, None
        let tokens, initOpt =
            match tokens with
            | { value = TokEq } :: tokens ->
                let tokens, expr = parseExpr tokens
                tokens, Some expr
            | _ -> tokens, None
        let tokens = expect TokSemicolon tokens
        let varDef : VarDef =
            {
                name = name
                pos = pos
                varType = typeExprOpt
                init = initOpt
            }
        Some(tokens, varDef)
    | _ -> None

let parseExprBetweenParens tokens =
    let tokens = expect TokLParen tokens
    let tokens, expr = parseExpr tokens
    let tokens = expect TokRParen tokens
    tokens, expr

let rec private parseStatementOpt tokens =
    match tokens with
    | { value = TokId name } :: { value = TokColon } :: tokens ->
        let tokens, stmt = parseStatement tokens
        Some(tokens, LabeledStmt(name, stmt))
    | { value = TokCase } :: tokens ->
        let tokens, expr = parseExpr tokens
        let tokens = expect TokColon tokens
        let tokens, stmt = parseStatement tokens
        Some(tokens, CaseStmt(expr, stmt))
    | { value = TokDefault } :: tokens ->
        let tokens = expect TokColon tokens
        let tokens, stmt = parseStatement tokens
        Some(tokens, DefaultStmt stmt)
    | { value = TokLBrace } :: _ ->
        Some(parseCompoundStmt tokens)
    | ParseOpt parseExprOpt (tokens, expr) ->
        let tokens = expect TokSemicolon tokens
        Some(tokens, ExprStmt expr)
    | { value = TokSemicolon } :: tokens ->
        Some(tokens, NullStmt)
    | { value = TokIf } :: tokens ->
        let tokens, expr = parseExprBetweenParens tokens
        let tokens, thenStmt = parseStatement tokens
        let tokens, elseStmtOpt =
            match tokens with
            | { value = TokElse } :: tokens ->
                let tokens, elseStmt = parseStatement tokens
                tokens, Some elseStmt
            | _ -> tokens, None
        Some(tokens, IfStmt(expr, thenStmt, elseStmtOpt))
    | { value = TokSwitch } :: tokens -> 
        let tokens, expr = parseExprBetweenParens tokens
        let tokens, stmt = parseStatement tokens
        Some(tokens, SwitchStmt(expr, stmt))
    | { value = TokWhile } :: tokens ->
        let tokens, expr = parseExprBetweenParens tokens
        let tokens, stmt = parseStatement tokens
        Some(tokens, WhileStmt(expr, stmt))
    | { value = TokDo } :: tokens ->
        let tokens, stmt = parseStatement tokens
        let tokens = expect TokWhile tokens
        let tokens, expr = parseExprBetweenParens tokens
        let tokens = expect TokSemicolon tokens
        Some(tokens, DoWhileStmt(stmt, expr))
    | { value = TokFor } :: tokens ->
        let tokens = expect TokLParen tokens
        let tokens, varNameOpt, varTypeOpt, initOpt =
            match tokens with
            | { value = TokVar } :: tokens ->
                let tokens, name, pos (* TODO: use pos *) = expectId tokens
                let tokens, typeExprOpt =
                    match parseTypeAnnotationOpt tokens with
                    | None -> tokens, None
                    | Some(tokens, typeExpr) -> tokens, Some typeExpr
                let tokens = expect TokEq tokens
                let tokens, expr = parseExpr tokens
                tokens, Some name, typeExprOpt, Some expr
            | ParseOpt parseExprOpt (tokens, expr) ->
                tokens, None, None, Some expr
            | _ -> tokens, None, None, None
        let tokens = expect TokSemicolon tokens
        let tokens, condOpt =
            match tokens with
            | ParseOpt parseExprOpt (tokens, expr) -> tokens, Some expr
            | _ -> tokens, None
        let tokens = expect TokSemicolon tokens
        let tokens, iterOpt =
            match tokens with
            | ParseOpt parseExprOpt (tokens, expr) -> tokens, Some expr
            | _ -> tokens, None
        let tokens = expect TokRParen tokens
        let tokens, stmt = parseStatement tokens
        Some(tokens, ForStmt(varNameOpt, varTypeOpt, initOpt, condOpt, iterOpt, stmt))
    | { value = TokGoto } :: tokens ->
        let tokens, name, pos = expectId tokens
        let tokens = expect TokSemicolon tokens
        Some(tokens, GotoStmt name)
    | { value = TokContinue } :: tokens ->
        let tokens = expect TokSemicolon tokens
        Some(tokens, ContinueStmt)
    | { value = TokBreak } :: tokens ->
        let tokens = expect TokSemicolon tokens
        Some(tokens, BreakStmt)
    | { value = TokReturn } :: tokens ->
        let tokens, exprOpt =
            match tokens with
            | ParseOpt parseExprOpt (tokens, expr) -> tokens, Some expr
            | _ -> tokens, None
        let tokens = expect TokSemicolon tokens
        Some(tokens, ReturnStmt exprOpt)
    | { value = TokTry } :: tokens ->
        let tokens, stmt = parseCompoundStmt tokens
        let handlers = List<_>()
        let rec loop tokens =
            match parseExceptionHandlerOpt tokens with
            | None ->
                if handlers.Count = 0 then
                    errorUnexpectedStr tokens "exception handler"
                tokens
            | Some(tokens, handler) ->
                handlers.Add(handler)
                loop tokens
        let tokens = loop tokens
        Some(tokens, TryBlockStmt(stmt, List.ofSeq handlers))
    | { value = TokThrow } :: tokens ->
        let tokens, exprOpt =
            match parseExprOpt tokens with
            | Some(tokens, expr) -> tokens, Some expr
            | None -> tokens, None
        let tokens = expect TokSemicolon tokens
        Some(tokens, ThrowStmt exprOpt)
    | _ -> None

and private parseStatement tokens =
    match parseStatementOpt tokens with
    | None -> errorUnexpectedStr tokens "statement"
    | Some result -> result

and private parseBlockItemOpt tokens =
    match tokens with
    | ParseOpt parseVarOpt (tokens, varDef) -> Some(tokens, DeclBlockItem varDef)
    | ParseOpt parseStatementOpt (tokens, stmt) -> Some(tokens, StmtBlockItem stmt)
    | _ -> None

and private parseCompoundStmt tokens =
    let tokens = expect TokLBrace tokens
    let items = List<_>()
    let rec loop tokens =
        match parseBlockItemOpt tokens with
        | None -> tokens
        | Some(tokens, item) ->
            items.Add(item)
            loop tokens
    let tokens = loop tokens
    let tokens = expect TokRBrace tokens
    tokens, CompoundStmt(List.ofSeq items)

and private parseExceptionHandlerOpt tokens =
    match tokens with
    | { value = TokCatch } :: tokens ->
        let tokens, expr = parseExprBetweenParens tokens
        let tokens, stmt = parseCompoundStmt tokens
        Some(tokens, CatchHandler(expr, stmt))
    | { value = TokFinally } :: tokens ->
        let tokens, stmt = parseCompoundStmt tokens
        Some(tokens, FinallyHandler stmt)
    | _ -> None

let private parseParamOpt tokens =
    match tokens with
    | { value = TokId name; pos = pos } :: tokens ->
        let tokens = expect TokColon tokens
        let tokens, paramType = parseTypeExpr tokens
        Some(tokens, (name, pos, paramType))
    | _ -> None

let private parseFunOpt tokens =
    match tokens with
    | { value = TokFun } :: tokens ->
        let tokens, name, pos = expectId tokens
        let tokens = expect TokLParen tokens
        let tokens, pars = parseList parseParamOpt "parameter" TokComma tokens
        let tokens = expect TokRParen tokens
        let tokens, retType = parseTypeAnnotation tokens
        let tokens, body =
            match tokens with
            | { value = TokSemicolon } :: tokens -> tokens, None
            | { value = TokLBrace } :: _ ->
                let tokens, body = parseCompoundStmt tokens
                tokens, Some body
            | _ -> errorUnexpectedStr tokens "semicolon or body"
        let funDef : FunDef =
            {
                name = name
                pos = pos
                pars = pars
                retType = retType
                body = body
            }
        Some(tokens, funDef)
    | _ -> None

let private parseTopLevelDefOpt tokens =
    match tokens with
    | [] -> None
    | ParseOpt parseEnumOpt (tokens, enumDef) -> Some(tokens, TopEnum enumDef)
    | ParseOpt parseStructOpt (tokens, structDef) -> Some(tokens, TopStruct structDef)
    | ParseOpt parseVarOpt (tokens, varDef) -> Some(tokens, TopVar varDef)
    | ParseOpt parseFunOpt (tokens, funDef) -> Some(tokens, TopFun funDef)
    | _ -> errorUnexpectedStr tokens "top-level definition"

let parse path =
    let tokens =
        Scanning.scan path
        |> List.ofSeq
    let defs = List<_>()
    let rec loop tokens =
        match parseTopLevelDefOpt tokens with
        | Some(tokens, def) ->
            defs.Add(def)
            loop tokens
        | None -> List.ofSeq defs
    loop tokens
