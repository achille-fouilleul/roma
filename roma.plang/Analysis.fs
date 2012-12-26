module Roma.Plang.Analysis

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

type Constant =
    | SInt8 of int8
    | SInt16 of int16
    | SInt32 of int32
    | SInt64 of int64
    | UInt8 of uint8
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64

let private createZeroConstant typeExpr =
    match typeExpr with
    | PrimitiveType PrimitiveTypeKind.SInt8 -> Constant.SInt8 0y
    | PrimitiveType PrimitiveTypeKind.SInt16 -> Constant.SInt16 0s
    | PrimitiveType PrimitiveTypeKind.SInt32 -> Constant.SInt32 0
    | PrimitiveType PrimitiveTypeKind.SInt64 -> Constant.SInt64 0L
    | PrimitiveType PrimitiveTypeKind.UInt8 -> Constant.UInt8 0uy
    | PrimitiveType PrimitiveTypeKind.UInt16 -> Constant.UInt16 0us
    | PrimitiveType PrimitiveTypeKind.UInt32 -> Constant.UInt32 0u
    | PrimitiveType PrimitiveTypeKind.UInt64 -> Constant.UInt64 0UL
    | _ ->
        // TODO: consider including bool, char{8,16,32}...
        // TODO: report error
        failwith "Type is not valid as underlying enum type."

let private constToSInt64 value =
    // TODO: report error
    match value with
    | Constant.SInt8 x -> int64 x
    | Constant.SInt16 x -> int64 x
    | Constant.SInt32 x -> int64 x
    | Constant.SInt64 x -> int64 x
    | Constant.UInt8 x -> Checked.int64 x
    | Constant.UInt16 x -> Checked.int64 x
    | Constant.UInt32 x -> Checked.int64 x
    | Constant.UInt64 x -> Checked.int64 x

let private constToUInt64 value =
    // TODO: report error
    match value with
    | Constant.SInt8 x -> Checked.uint64 x
    | Constant.SInt16 x -> Checked.uint64 x
    | Constant.SInt32 x -> Checked.uint64 x
    | Constant.SInt64 x -> Checked.uint64 x
    | Constant.UInt8 x -> uint64 x
    | Constant.UInt16 x -> uint64 x
    | Constant.UInt32 x -> uint64 x
    | Constant.UInt64 x -> uint64 x

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
            Constant.SInt32(int32(value))
        | value when value >= 0I && value <= maxUInt32 ->
            Constant.UInt32(uint32(value))
        | value when value >= minSInt64 && value <= maxSInt64 ->
            Constant.SInt64(int64(value))
        | value when value >= 0I && value <= maxUInt64 ->
            Constant.UInt64(uint64(value))
        | _ ->
            failwith "Cannot represent integer literal."
    | _ ->
        raise(System.NotImplementedException()) // TODO

let rec private evalConstExpr expr =
    match expr with
    | NumberExpr s -> strToConstant s
    | _ -> raise(System.NotImplementedException()) // TODO

let private constImplicitConv typeExpr value =
    // TODO: report error
    match typeExpr with
    | PrimitiveType PrimitiveTypeKind.SInt8 ->
        Constant.SInt8(Checked.sbyte(constToSInt64 value))
    | PrimitiveType PrimitiveTypeKind.SInt16 ->
        Constant.SInt16(Checked.int16(constToSInt64 value))
    | PrimitiveType PrimitiveTypeKind.SInt32 ->
        Constant.SInt32(Checked.int32(constToSInt64 value))
    | PrimitiveType PrimitiveTypeKind.SInt64 ->
        Constant.SInt64(Checked.int64(constToSInt64 value))
    | PrimitiveType PrimitiveTypeKind.UInt8 ->
        Constant.UInt8(Checked.byte(constToUInt64 value))
    | PrimitiveType PrimitiveTypeKind.UInt16 ->
        Constant.UInt16(Checked.uint16(constToUInt64 value))
    | PrimitiveType PrimitiveTypeKind.UInt32 ->
        Constant.UInt32(Checked.uint32(constToUInt64 value))
    | PrimitiveType PrimitiveTypeKind.UInt64 ->
        Constant.UInt64(Checked.uint64(constToUInt64 value))
    | _ -> failwith "internal error"

let private incrConstant value =
    // TODO: report error
    match value with
    | Constant.SInt8 n -> Constant.SInt8(Checked.(+) n 1y)
    | Constant.SInt16 n -> Constant.SInt16(Checked.(+) n 1s)
    | Constant.SInt32 n -> Constant.SInt32(Checked.(+) n 1)
    | Constant.SInt64 n -> Constant.SInt64(Checked.(+) n 1L)
    | Constant.UInt8 n -> Constant.UInt8(Checked.(+) n 1uy)
    | Constant.UInt16 n -> Constant.UInt16(Checked.(+) n 1us)
    | Constant.UInt32 n -> Constant.UInt32(Checked.(+) n 1u)
    | Constant.UInt64 n -> Constant.UInt64(Checked.(+) n 1UL)

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

                let index = ref(createZeroConstant enumDef.underlyingType)
                let values =
                    seq {
                        for name, pos, exprOpt in enumDef.values do
                            let value =
                                match exprOpt with
                                | None -> !index
                                | Some expr ->
                                    evalConstExpr expr
                                    |> constImplicitConv enumDef.underlyingType
                            yield name, value
                            index := incrConstant value
                    }
                    |> Map.ofSeq
                yield enumDef.name, values

            | _ -> ()
    }
    |> Map.ofSeq
