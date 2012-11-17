﻿module Roma.Plang.Scanning

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
    | TokCatch
    | TokContinue
    | TokDefault
    | TokElse
    | TokEnum
    | TokFalse
    | TokFinally
    | TokFor
    | TokFun
    | TokGoto
    | TokIf
    | TokNull
    | TokReadonly
    | TokReturn
    | TokSizeof
    | TokStruct
    | TokSwitch
    | TokThrow
    | TokTrue
    | TokTry
    | TokUnion
    | TokUsing
    | TokVar
    | TokWhile
    // TODO: other keywords

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
    // TODO: other punctuators

type Token =
    {
        pos : SourcePosition
        value : TokenValue
    }

let private isAsciiSpace c =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'

let private punctStart = Set.ofSeq "!%&()*+,-./:;<=>?[]^{|}~"

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
                        this.Advance(3)
                        this.YieldToken(TokEllipsis)
                    | Some c, _ when isAsciiDigit c -> this.ScanNumber()
                    | _ -> this.ScanPunctuator()
                | '/' ->
                    match la with
                    | Some '/' ->
                        this.Advance()
                        this.SkipSingleLineComment()
                    | Some '*' ->
                        this.Advance()
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
            | "catch" -> TokCatch
            | "continue" -> TokContinue
            | "default" -> TokDefault
            | "else" -> TokElse
            | "enum" -> TokEnum
            | "false" -> TokFalse
            | "finally" -> TokFinally
            | "for" -> TokFor
            | "fun" -> TokFun
            | "goto" -> TokGoto
            | "if" -> TokIf
            | "null" -> TokNull
            | "readonly" -> TokReadonly
            | "return" -> TokReturn
            | "sizeof" -> TokSizeof
            | "struct" -> TokStruct
            | "switch" -> TokSwitch
            | "throw" -> TokThrow
            | "true" -> TokTrue
            | "try" -> TokTry
            | "union" -> TokUnion
            | "using" -> TokUsing
            | "var" -> TokVar
            | "while" -> TokWhile
            // TODO: other reserved words
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
            | '.', _, _ -> TokPeriod, 1
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

let scan path =
    let text =
        System.IO.File.ReadAllText(path)
        |> normalizeEols
    Scanner(path, text).Run()