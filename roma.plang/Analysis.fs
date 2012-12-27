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

// check against top-level conflicts
let checkTopLevelConflicts (defs : TopLevelDef list) =
    let entityGroups =
        seq {
            for def in defs ->
                match def with
                | TopEnum enumDef -> enumDef.name, enumDef.pos, "enum"
                | TopStruct structDef -> structDef.name, structDef.pos, "struct"
                | TopFun funDef -> funDef.name, funDef.pos, "function"
                | TopConst constDef -> constDef.name, constDef.pos, "constant"
                | TopVar varDef -> varDef.name, varDef.pos, "variable"
                | TopTypeAlias typeAliasDef -> typeAliasDef.name, typeAliasDef.pos, "type alias"
        }
        |> Seq.groupBy (fun (name, _, _) -> name)

    let diag = Diagnostics()
    for (name, xs) in entityGroups do
        let xs = Array.ofSeq xs
        if xs.Length > 1 then
            for i = 0 to xs.Length - 1 do
                let (_, pos, desc) = xs.[i]
                if i = 0 then
                    diag.Error(pos, sprintf "'%s' declared as %s here." name desc)
                else
                    diag.Error(pos, sprintf "'%s' redeclared as %s here." name desc)
    diag.Report()

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

let private strToConstant (s : string) =
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

let createEnumMap (defs : TopLevelDef list) =
    seq {
        for def in defs do
            match def with
            | TopEnum enumDef ->

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

                let underlyingType =
                    // TODO: eval type expr
                    match enumDef.underlyingType with
                    | PrimitiveType kind when IntegerConstant.IsValidKind kind -> kind
                    | _ ->
                        // TODO: report error
                        failwith "Type is not valid as underlying enum type."

                let index = ref(IntegerConstant(underlyingType, Int128.Zero))
                let values =
                    seq {
                        for name, pos, exprOpt in enumDef.values do
                            let value =
                                match exprOpt with
                                | None -> !index
                                | Some expr ->
                                    let value = evalConstExpr expr
                                    // TODO: report error (overflow)
                                    IntegerConstant(underlyingType, value.Value)
                            yield name, value.Value
                            // TODO: report error (overflow)
                            index := IntegerConstant(underlyingType, value.Value + Int128.One)
                    }
                    |> Map.ofSeq
                yield enumDef.name, (underlyingType, values)

            | _ -> ()
    }
    |> Map.ofSeq
