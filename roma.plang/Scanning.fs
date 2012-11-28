module Roma.Plang.Scanning

type SourcePosition =
    {
        path : string
        line : int
        pos : int
    }

type TokenValue =
    | TokChar of char
    | TokId of string
    | TokNumber of string
    | TokString of string

    | TokBool
    | TokBreak
    | TokCase
    | TokCast
    | TokCatch
    | TokChar8
    | TokChar16
    | TokChar32
    | TokContinue
    | TokDefault
    | TokDo
    | TokElse
    | TokEnum
    | TokFalse
    | TokFinally
    | TokFloat32
    | TokFloat64
    | TokFor
    | TokFun
    | TokGoto
    | TokIf
    | TokNull
    | TokNull_t
    | TokReadonly
    | TokReturn
    | TokSInt8
    | TokSInt16
    | TokSInt32
    | TokSInt64
    | TokSIntPtr
    | TokSizeof
    | TokStruct
    | TokSwitch
    | TokThrow
    | TokTrue
    | TokTry
    | TokType
    | TokUInt8
    | TokUInt16
    | TokUInt32
    | TokUInt64
    | TokUIntPtr
    | TokUnion
    | TokUsing
    | TokVar
    | TokVoid
    | TokWhile

    | TokNeq
    | TokExcl
    | TokModEq
    | TokMod
    | TokAndAnd
    | TokAndEq
    | TokAnd
    | TokLParen
    | TokRParen
    | TokStarEq
    | TokStar
    | TokPlusPlus
    | TokPlusEq
    | TokPlus
    | TokComma
    | TokMinusMinus
    | TokMinusEq
    | TokArrow
    | TokMinus
    | TokPeriod
    | TokSlashEq
    | TokSlash
    | TokColon
    | TokSemicolon
    | TokLshEq
    | TokLsh
    | TokLE
    | TokLT
    | TokEqEq
    | TokEq
    | TokRshEq
    | TokGE
    | TokRsh
    | TokGT
    | TokQues
    | TokLBracket
    | TokRBracket
    | TokHatEq
    | TokHat
    | TokLBrace
    | TokOrEq
    | TokOrOr
    | TokOr
    | TokRBrace
    | TokNot
    | TokEllipsis

let tokenKindToStr tok =
    match tok with
    | TokChar _ -> "character literal"
    | TokId _ -> "identifier"
    | TokNumber _ -> "number"
    | TokString _ -> "string literal"

    | TokBool -> "'bool'"
    | TokBreak -> "'break'"
    | TokCase -> "'case'"
    | TokCast -> "'cast'"
    | TokCatch -> "'catch'"
    | TokChar8 -> "'char8_t'"
    | TokChar16 -> "'char16_t'"
    | TokChar32 -> "'char32_t'"
    | TokContinue -> "'continue'"
    | TokDefault -> "'default'"
    | TokDo -> "'do'"
    | TokElse -> "'else'"
    | TokEnum -> "'enum'"
    | TokFalse -> "'false'"
    | TokFinally -> "'finally'"
    | TokFloat32 -> "'float32_t'"
    | TokFloat64 -> "'float64_t'"
    | TokFor -> "'for'"
    | TokFun -> "'fun'"
    | TokGoto -> "'goto'"
    | TokIf -> "'if'"
    | TokNull -> "'null'"
    | TokNull_t -> "'null_t'"
    | TokReadonly -> "'readonly'"
    | TokReturn -> "'return'"
    | TokSInt8 -> "'sint8_t'"
    | TokSInt16 -> "'sint16_t'"
    | TokSInt32 -> "'sint32_t'"
    | TokSInt64 -> "'sint64_t'"
    | TokSIntPtr -> "'sintptr_t'"
    | TokSizeof -> "'sizeof'"
    | TokStruct -> "'struct'"
    | TokSwitch -> "'switch'"
    | TokThrow -> "'throw'"
    | TokTrue -> "'true'"
    | TokTry -> "'try'"
    | TokType -> "'type'"
    | TokUInt8 -> "'uint8_t'"
    | TokUInt16 -> "'uint16_t'"
    | TokUInt32 -> "'uint32_t'"
    | TokUInt64 -> "'uint64_t'"
    | TokUIntPtr -> "'uintptr_t'"
    | TokUnion -> "'union'"
    | TokUsing -> "'using'"
    | TokVar -> "'var'"
    | TokVoid -> "'void'"
    | TokWhile -> "'while'"

    | TokNeq -> "'!='"
    | TokExcl -> "'!'"
    | TokModEq -> "'%='"
    | TokMod -> "'%'"
    | TokAndAnd -> "'&&'"
    | TokAndEq -> "'&='"
    | TokAnd -> "'&'"
    | TokLParen -> "'('"
    | TokRParen -> "')'"
    | TokStarEq -> "'*='"
    | TokStar -> "'*'"
    | TokPlusPlus -> "'++'"
    | TokPlusEq -> "'+='"
    | TokPlus -> "'+'"
    | TokComma -> "','"
    | TokMinusMinus -> "'--'"
    | TokMinusEq -> "'-='"
    | TokArrow -> "'->'"
    | TokMinus -> "'-'"
    | TokPeriod -> "'.'"
    | TokSlashEq -> "'/='"
    | TokSlash -> "'/'"
    | TokColon -> "':'"
    | TokSemicolon -> "';'"
    | TokLshEq -> "'<<='"
    | TokLsh -> "'<<'"
    | TokLE -> "'<='"
    | TokLT -> "'<'"
    | TokEqEq -> "'=='"
    | TokEq -> "'='"
    | TokRshEq -> "'>>='"
    | TokGE -> "'>='"
    | TokRsh -> "'>>'"
    | TokGT -> "'>'"
    | TokQues -> "'?'"
    | TokLBracket -> "'['"
    | TokRBracket -> "']'"
    | TokHatEq -> "'^='"
    | TokHat -> "'^'"
    | TokLBrace -> "'{'"
    | TokOrEq -> "'|='"
    | TokOrOr -> "'||'"
    | TokOr -> "'|'"
    | TokRBrace -> "'}'"
    | TokNot -> "'~'"
    | TokEllipsis -> "'...'"

type Token =
    {
        pos : SourcePosition
        value : TokenValue
    }

let private isAsciiSpace c =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'

let private punctStart = Set.ofSeq "!%&()*+,-/:;<=>?[]^{|}~"

let private isPunctStart c =
    punctStart.Contains(c)

let private isAsciiDigit c =
    c >= '0' && c <= '9'

let private isAsciiAlpha c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

let private isAsciiAlnum c =
    isAsciiDigit c || isAsciiAlpha c

type private Scanner(path : string, s : string) =
    let mutable i = 0
    let mutable line = 1
    let mutable pos = 1
    let tokens = System.Collections.Generic.List<_>()
    let errors = System.Collections.Generic.List<_>()
    let mutable tokenStart = None

    member private this.Peek() =
        if i < s.Length then
            Some(s.[i])
        else
            None

    member private this.Peek(n) =
        if i + n < s.Length then
            Some(s.[i + n])
        else
            None

    member private this.Advance() =
        assert(i < s.Length)
        let c = s.[i]
        if c = '\n' then
            line <- line + 1
            pos <- 1
        else
            pos <- pos + 1
        i <- i + 1

    member private this.Advance(n) =
        for i = 1 to n do
            this.Advance()

    member private this.Read() =
        assert(i < s.Length)
        let c = s.[i]
        this.Advance()
        c

    member private this.Position =
        let sp : SourcePosition =
            {
                path = path
                line = line
                pos = pos
            }
        sp

    member private this.Error(s : string) =
        errors.Add((this.Position, s))

    member private this.ErrorEof() =
        this.Error("Unexpected error of input.")

    member this.Run() =
        let rec loop() =
            let rec skipSpace() =
                match this.Peek() with
                | Some c when isAsciiSpace c ->
                    this.Advance()
                    skipSpace()
                | _ -> ()
            skipSpace()

            let la = this.Peek(1)
            match this.Peek() with
            | None -> ()
            | Some c ->
                this.MarkTokenStart()
                match c with
                | c when isAsciiSpace c -> this.Advance()
                | c when isAsciiAlpha c || c = '_' -> this.ScanIdOrKeyword()
                | c when isAsciiDigit c -> this.ScanNumber()
                | '"' -> this.ScanString()
                | '\'' -> this.ScanChar()
                | '.' ->
                    match la, this.Peek(2) with
                    | Some '.', Some '.' ->
                        this.YieldToken(TokEllipsis)
                        this.Advance(3)
                    | Some c, _ when isAsciiDigit c -> this.ScanNumber()
                    | _ ->
                        this.YieldToken(TokPeriod)
                        this.Advance(1)
                | '/' ->
                    match la with
                    | Some '/' ->
                        this.Advance(2)
                        this.SkipSingleLineComment()
                    | Some '*' ->
                        this.Advance(2)
                        this.SkipDelimitedComment()
                    | _ -> this.ScanPunctuator()
                | c when isPunctStart c -> this.ScanPunctuator()
                | _ ->
                    this.Error("Invalid character.")
                    this.Advance()
                loop()
        loop()
        tokens

    member private this.SkipDelimitedComment() =
        let rec loop() =
            match this.Peek() with
            | None -> this.ErrorEof()
            | Some '*' when this.Peek(1) = Some '/' ->
                this.Advance(2)
            | _ ->
                this.Advance()
                loop()
        loop()

    member private this.SkipSingleLineComment() =
        let rec loop() =
            match this.Peek() with
            | None -> this.ErrorEof()
            | Some '\n' ->
                this.Advance()
            | _ ->
                this.Advance()
                loop()
        loop()

    member private this.ScanIdOrKeyword() =
        let buf = System.Text.StringBuilder()
        let rec loop() =
            match this.Peek() with
            | Some c when isAsciiAlnum c || c = '_' ->
                buf.Append(c) |> ignore
                this.Advance()
                loop()
            | _ -> ()
        loop()
        let kind =
            match buf.ToString() with
            | "bool" -> TokBool
            | "break" -> TokBreak
            | "case" -> TokCase
            | "cast" -> TokCast
            | "catch" -> TokCatch
            | "char8" -> TokChar8
            | "char16" -> TokChar16
            | "char32" -> TokChar32
            | "continue" -> TokContinue
            | "default" -> TokDefault
            | "do" -> TokDo
            | "else" -> TokElse
            | "enum" -> TokEnum
            | "false" -> TokFalse
            | "finally" -> TokFinally
            | "float32" -> TokFloat32
            | "float64" -> TokFloat64
            | "for" -> TokFor
            | "fun" -> TokFun
            | "goto" -> TokGoto
            | "if" -> TokIf
            | "null" -> TokNull
            | "null_t" -> TokNull_t
            | "readonly" -> TokReadonly
            | "return" -> TokReturn
            | "sint8" -> TokSInt8
            | "sint16" -> TokSInt16
            | "sint32" -> TokSInt32
            | "sint64" -> TokSInt64
            | "sintptr" -> TokSIntPtr
            | "sizeof" -> TokSizeof
            | "struct" -> TokStruct
            | "switch" -> TokSwitch
            | "throw" -> TokThrow
            | "true" -> TokTrue
            | "try" -> TokTry
            | "type" -> TokType
            | "uint8" -> TokUInt8
            | "uint16" -> TokUInt16
            | "uint32" -> TokUInt32
            | "uint64" -> TokUInt64
            | "uintptr" -> TokUIntPtr
            | "union" -> TokUnion
            | "using" -> TokUsing
            | "var" -> TokVar
            | "void" -> TokVoid
            | "while" -> TokWhile
            | s -> TokId s
        this.YieldToken(kind)

    member private this.ScanNumber() =
        let buf = System.Text.StringBuilder()
        let rec loop() =
            match this.Peek() with
            | Some c when isAsciiDigit c || c = '.' ->
                // TODO: validity check, NaNs, exponent, hex ints & floats
                buf.Append(c) |> ignore
                this.Advance()
                loop()
            | _ -> ()
        loop()
        this.YieldToken(TokNumber(buf.ToString()))

    member private this.ScanString() =
        assert(this.Read() = '"')
        let buf = System.Text.StringBuilder()
        let rec loop() =
            match this.Peek() with
            // TODO: escapes, errors
            | Some '"' -> this.Advance()
            | Some c ->
                buf.Append(c) |> ignore
                this.Advance()
                loop()
            | None -> ()
        loop()
        this.YieldToken(TokString(buf.ToString()))

    member private this.ScanChar() =
        assert(this.Read() = '\'')
        // TODO: escapes
        match this.Peek() with
        | Some c ->
            this.Advance()
            match this.Peek() with
            | Some '\'' ->
                this.Advance()
                this.YieldToken(TokChar c)
            | _ -> this.Error("Expected single quotation mark (').")
        | None -> this.ErrorEof()

    member private this.ScanPunctuator() =
        let c0 = defaultArg (this.Peek()) '\u0000'
        let c1 = defaultArg (this.Peek(1)) '\u0000'
        let c2 = defaultArg (this.Peek(2)) '\u0000'
        let value, n =
            match c0, c1, c2 with
            | '!', '=', _ -> TokNeq, 2
            | '!', _, _ -> TokExcl, 1
            | '%', '=', _ -> TokModEq, 2
            | '%', _, _ -> TokMod, 1
            | '&', '&', _ -> TokAndAnd, 2
            | '&', '=', _ -> TokAndEq, 2
            | '&', _, _ -> TokAnd, 1
            | '(', _, _ -> TokLParen, 1
            | ')', _, _ -> TokRParen, 1
            | '*', '=', _ -> TokStarEq, 2
            | '*', _, _ -> TokStar, 1
            | '+', '+', _ -> TokPlusPlus, 2
            | '+', '=', _ -> TokPlusEq, 2
            | '+', _, _ -> TokPlus, 1
            | ',', _, _ -> TokComma, 1
            | '-', '-', _ -> TokMinusMinus, 2
            | '-', '=', _ -> TokMinusEq, 2
            | '-', '>', _ -> TokArrow, 2
            | '-', _, _ -> TokMinus, 1
            | '/', '=', _ -> TokSlashEq, 2
            | '/', _, _ -> TokSlash, 1
            | ':', _, _ -> TokColon, 1
            | ';', _, _ -> TokSemicolon, 1
            | '<', '<', '=' -> TokLshEq, 3
            | '<', '<', _ -> TokLsh, 2
            | '<', '=', _ -> TokLE, 2
            | '<', _, _ -> TokLT, 1
            | '=', '=', _ -> TokEqEq, 2
            | '=', _, _ -> TokEq, 1
            | '>', '>', '=' -> TokRshEq, 3
            | '>', '=', _ -> TokGE, 2
            | '>', '>', _ -> TokRsh, 2
            | '>', _, _ -> TokGT, 1
            | '?', _, _ -> TokQues, 1
            | '[', _, _ -> TokLBracket, 1
            | ']', _, _ -> TokRBracket, 1
            | '^', '=', _ -> TokHatEq, 2
            | '^', _, _ -> TokHat, 1
            | '{', _, _ -> TokLBrace, 1
            | '|', '=', _ -> TokOrEq, 2
            | '|', '|', _ -> TokOrOr, 2
            | '|', _, _ -> TokOr, 1
            | '}', _, _ -> TokRBrace, 1
            | '~', _, _ -> TokNot, 1
            | _ -> raise(System.NotImplementedException())
        this.YieldToken(value)
        this.Advance(n)

    member private this.MarkTokenStart() =
        tokenStart <- Some(this.Position)

    member private this.YieldToken(value) =
        let token : Token =
            {
                pos = Option.get tokenStart
                value = value
            }
        tokens.Add(token)
        tokenStart <- None

let private normalizeEols (text : string) =
    let buf = System.Text.StringBuilder()
    let rec loop i =
        let j = text.IndexOf('\r', i)
        if j >= 0 then
            if i <= j - 1 then
                buf.Append(text.[i .. j - 1]) |> ignore
            if not(j + 1 < text.Length && text.[j + 1] = '\n') then
                buf.Append('\n') |> ignore
            loop (j + 1)
        else
            buf.Append(text.[i ..]) |> ignore
    loop 0
    buf.ToString()

let scan path text =
    Scanner(path, normalizeEols text).Run()
