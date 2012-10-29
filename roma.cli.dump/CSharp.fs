module Roma.Cli.Dump.CSharp

open System
open Roma.Cli

let private intToStr (n : int) =
    n.ToString(System.Globalization.CultureInfo.InvariantCulture)

let private constantToStr constant =
    match constant with
    | ConstantBool false -> "false"
    | ConstantBool true -> "true"
    | ConstantBytearray bytes -> raise(NotImplementedException()) // TODO
    | ConstantChar value ->
        match char value with
        | '\'' -> "'\\''"
        | '\t' -> "'\\t'"
        | '\n' -> "'\\n'"
        | '\r' -> "'\\r'"
        | c -> sprintf "'%c'" c // TODO: escape special chars
    | ConstantR4 0xffc00000u -> "0.0f / 0.0f" // NaN
    | ConstantR4 0x7f800000u -> "1.0f / 0.0f" // +inf
    | ConstantR4 0xff800000u -> "-1.0f / 0.0f" // -inf
    | ConstantR4 value ->
        sprintf "%gf" (BitConverter.ToSingle(BitConverter.GetBytes(value), 0))
    | ConstantR8 0xfff8000000000000UL -> "0.0 / 0.0" // NaN
    | ConstantR8 0x7ff0000000000000UL -> "1.0 / 0.0" // +inf
    | ConstantR8 0xfff0000000000000UL -> "-1.0 / 0.0" // -inf
    | ConstantR8 value ->
        sprintf "%g" (BitConverter.ToDouble(BitConverter.GetBytes(value), 0))
    | ConstantI1 value -> sprintf "%d" value
    | ConstantU1 value -> sprintf "%u" value
    | ConstantI2 value -> sprintf "%d" value
    | ConstantU2 value -> sprintf "%u" value
    | ConstantI4 value -> sprintf "%d" value
    | ConstantU4 value -> sprintf "%uU" value
    | ConstantI8 value -> sprintf "%dL" value
    | ConstantU8 value -> sprintf "%dUL" value
    | ConstantString s -> "\"" + s + "\"" // TODO: escape special chars
    | ConstantNullRef -> "null"

type private GenVarMap =
    {
        vars : string[]
        mvars : string[]
    }

    member this.Var(n) = this.vars.[n]

    member this.MVar(n) = this.mvars.[n]

type private ByRefDir =
    | OutOnly
    | InOut

let rec private typeSpecToStr varMap byRefDir typeSpec =
    match typeSpec with
    | TypeSpec.Choice1Of2 typeRef -> typeRefToStr varMap byRefDir typeRef
    | TypeSpec.Choice2Of2 typeSig -> typeSigToStr varMap byRefDir typeSig

and private typeRefToStr varMap byRefDir typeRef =
    match typeRef.scope with
    | Some(TypeRefScope enclosingTypeRef) ->
        (typeSpecToStr varMap byRefDir enclosingTypeRef) + "." + typeRef.typeName
    | _ ->
        if typeRef.typeNamespace.Length <> 0 then
            typeRef.typeNamespace + "." + typeRef.typeName
        else
            typeRef.typeName

and private typeSigToStr (varMap : GenVarMap) byRefDir typeSig =
    match typeSig with
    | Boolean -> "bool"
    | Char -> "char"
    | I1 -> "sbyte"
    | U1 -> "byte"
    | I2 -> "short"
    | U2 -> "ushort"
    | I4 -> "int"
    | U4 -> "uint"
    | I8 -> "long"
    | U8 -> "ulong"
    | R4 -> "float"
    | R8 -> "double"
    | I -> "System.IntPtr"
    | U -> "System.UIntPtr"
    | Array(typeSig, dims) ->
        if not(List.forall (fun dim -> dim = (0, None)) dims) then
            failwith "unsupported array shape"
        typeSigToStr varMap byRefDir typeSig + "[" + (String.replicate (List.length dims - 1) ",") + "]"
    | ByRef typeSig ->
        let typeStr = typeSigToStr varMap None typeSig
        match byRefDir with
        | None -> "ref " + typeStr // TODO: warn
        | Some OutOnly -> "out " + typeStr
        | Some InOut -> "ref " + typeStr
    | Fnptr methodSig ->
        let retTypeStr = typeSigToStr varMap None methodSig.retType
        let paramTypes = Seq.map (typeSigToStr varMap None) methodSig.paramTypes
        "__fnptr(" + retTypeStr + "(" + String.concat ", " paramTypes + ")" // TODO: calling convention
    | GenericInst(typeSig, args) ->
        let name = typeSigToStr varMap byRefDir typeSig
        let expectedSuffix = "`" + intToStr(List.length args)
        let shortName =
            if name.EndsWith(expectedSuffix) then
                name.[0 .. name.Length - expectedSuffix.Length - 1]
            else
                // TODO: warn about missing suffix
                name
        shortName + "<" + (args |> Seq.map (typeSigToStr varMap byRefDir) |> String.concat ", ") + ">"
    | MVar n -> varMap.MVar(n)
    | Object -> "object"
    | Ptr typeSig -> typeSigToStr varMap byRefDir typeSig + "*"
    | String -> "string"
    | SZArray typeSig -> typeSigToStr varMap byRefDir typeSig + "[]"
    | TypedByRef -> "System.TypedReference"
    | Var n -> varMap.Var(n)
    | Void -> "void"
    | ModReq(custMod, typeSig) ->
        let typeStr = typeSigToStr varMap byRefDir typeSig
        match typeRefToStr varMap None custMod with
        | "System.Runtime.CompilerServices.IsByValue" -> "__byvalue " + typeStr // C++/CLI
        | "System.Runtime.CompilerServices.IsImplicitlyDereferenced" -> "__implicitlydereferenced " + typeStr // C++/CLI
        | "System.Runtime.CompilerServices.IsUdtReturn" -> "__udtreturn " + typeStr // C++/CLI
        | "System.Runtime.CompilerServices.IsVolatile" -> "volatile " + typeStr
        | _ ->
            Diagnostics.Debugger.Break()
            raise(NotImplementedException())
    | ModOpt(custMod, typeSig) ->
        let typeStr = typeSigToStr varMap byRefDir typeSig
        match typeRefToStr varMap None custMod with
        | "System.Runtime.CompilerServices.CallConvStdcall" -> "__stdcall " + typeStr // C++/CLI
        | "System.Runtime.CompilerServices.IsConst" -> "__const " + typeStr // C++/CLI
        | "System.Runtime.CompilerServices.IsExplicitlyDereferenced" -> "__interior_ptr<" + typeStr + ">" // C++/CLI
        | "System.Runtime.CompilerServices.IsImplicitlyDereferenced" -> "__implicitlydereferenced " + typeStr // C++/CLI
        | "System.Runtime.CompilerServices.IsLong" -> "__long " + typeStr // C++/CLI
        | _ -> 
            Diagnostics.Debugger.Break()
            raise(NotImplementedException()) // TODO
    // | Pinned of TypeSig -> TODO
    | Class typeRef -> typeRefToStr varMap byRefDir typeRef
    | ValueType typeRef -> typeRefToStr varMap byRefDir typeRef
    | _ ->
        Diagnostics.Debugger.Break()
        raise(NotImplementedException()) // TODO

type private Writer() =
    let mutable level = 0
    let output = Console.Out
    let err = Console.Error

    member this.Enter() =
        level <- level + 1

    member this.Leave() =
        assert(level > 0)
        level <- level - 1

    member this.Print() =
        output.WriteLine()

    member this.Print(s : string) = 
        output.WriteLine((String.replicate level " ") + s)

    member this.Warn(s : string) =
        err.WriteLine(s)

let private attrToStr varMap (attr : CustomAttribute) =
    if attr.methodRef.methodName <> ".ctor" then
        raise(NotSupportedException())
    match attr.methodRef.typeRef with
    | None -> raise(NotSupportedException())
    | Some typeSpec ->
        let name = typeSpecToStr varMap None typeSpec
        let suffix = "Attribute"
        let shortName =
            if name.EndsWith(suffix) then
                name.[0 .. name.Length - suffix.Length - 1]
            else
                name
        shortName // TODO: arguments

let private dumpAttrs (w : Writer) varMap (attrs : seq<CustomAttribute>) =
    for attr in attrs do
        w.Print("[" + attrToStr varMap attr + "]")

let private dumpField (w : Writer) varMap (fld : FieldDef) =
    dumpAttrs w varMap fld.customAttrs
    // TODO: attributes
    let xs = Collections.Generic.List<string>()

    match fld.flags &&& FieldAttributes.FieldAccessMask with
    | FieldAttributes.CompilerControlled -> xs.Add("__compilercontrolled") // TODO: warn; not supported in C#
    | FieldAttributes.Private -> if false then xs.Add("private")
    | FieldAttributes.FamANDAssem -> () // TODO: warn; not supported in C#
    | FieldAttributes.Assembly -> xs.Add("internal")
    | FieldAttributes.Family -> xs.Add("protected")
    | FieldAttributes.FamORAssem -> xs.Add("protected internal")
    | FieldAttributes.Public -> xs.Add("public")
    | _ -> () // not reached

    if (fld.flags &&& FieldAttributes.Static) = FieldAttributes.Static then
        xs.Add(if Option.isSome fld.constant then "const" else "static")

    if (fld.flags &&& FieldAttributes.InitOnly) = FieldAttributes.InitOnly then
        xs.Add("readonly")

    xs.Add(typeSigToStr varMap None fld.typeSig)
    xs.Add(fld.name);
    match fld.constant with
    | None -> ()
    | Some constant ->
        // TODO: warn if non-static
        xs.Add("= " + (constantToStr constant))
    w.Print((String.concat " " xs) + ";")

let private dumpMethod w varMap ownerTypeName (mth : MethodDef) =
    let genMvars = [| for p in mth.genericParams -> p.name |]
    let genVarMap = { varMap with mvars = genMvars }
    dumpAttrs w genVarMap mth.customAttrs

    // TODO: other attributes (e.g. pinvokes, etc.)

    mth.retVal |> Option.iter (
        fun (par : ParamDef) ->
            for attr in par.customAttrs do
                w.Print("[return: " + attrToStr genVarMap attr + "]")
    )

    let xs = Collections.Generic.List<string>()

    match mth.flags &&& MethodAttributes.MemberAccessMask with
    | MethodAttributes.CompilerControlled -> xs.Add("__compilercontrolled") // TODO: warn; not supported in C#
    | MethodAttributes.Private -> if false then xs.Add("private")
    | MethodAttributes.FamANDAssem -> () // TODO: warn; not supported in C#
    | MethodAttributes.Assem -> xs.Add("internal")
    | MethodAttributes.Family -> xs.Add("protected")
    | MethodAttributes.FamORAssem -> xs.Add("protected internal")
    | MethodAttributes.Public -> xs.Add("public")
    | _ -> () // not reached

    if (mth.flags &&& MethodAttributes.Static) = MethodAttributes.Static then
        xs.Add("static")

    if (mth.flags &&& MethodAttributes.Virtual) = MethodAttributes.Virtual then
        if (mth.flags &&& MethodAttributes.NewSlot) = MethodAttributes.NewSlot then
            xs.Add("virtual")
        else
            xs.Add("override")
    if (mth.flags &&& MethodAttributes.Abstract) = MethodAttributes.Abstract then
        // TODO: warn if not virtual
        xs.Add("abstract")

    let name =
        if genMvars.Length <> 0 then
            mth.name + "<" + (String.concat ", " genMvars) + ">"
        else
            mth.name
    // TODO: gen param constraints (where %s : %s ...)
    // TODO: retval attributes
    // TODO: vararg methods (__arglist)
    // TODO: C# params keyword (ParamArrayAttribute)
    // TODO: C# unsafe keyword if pointers involved
    let retTypeStr = typeSigToStr genVarMap None mth.signature.retType
    let pars = seq {
        for typeSig, parOpt in Seq.zip mth.signature.paramTypes mth.parameters ->
            let byRefDir =
                Option.bind (fun (par : ParamDef) ->
                    let dirIn = (par.flags &&& ParamAttributes.In) = ParamAttributes.In
                    let dirOut = (par.flags &&& ParamAttributes.Out) = ParamAttributes.Out
                    match dirIn, dirOut with
                    | true, true -> Some InOut
                    | false, true -> Some OutOnly
                    | _ -> None
                ) parOpt
            let typeStr = typeSigToStr genVarMap byRefDir typeSig
            seq {
                match parOpt with
                | None -> yield typeStr
                | Some par ->
                    for attr in par.customAttrs do
                        yield "[" + attrToStr genVarMap attr + "]"
                    yield typeStr
                    yield par.name
            }
            |> String.concat " "
    }
    let paramsStr =  "(" + String.concat ", " pars + ")"
    match mth.name with
    | ".ctor"
    | ".cctor" ->
        // TODO: warn if return type is not void
        // TODO: check for specialname & rtspecialname flags
        xs.Add(ownerTypeName + paramsStr + ";")
        w.Print(String.concat " " xs)
    | "Finalize" when true (* TODO: non-generic, return type = void, no parameters... *) ->
        w.Print("~" + ownerTypeName)
    | _ -> // TODO: operators
        xs.Add(retTypeStr)
        xs.Add(name + paramsStr + ";")
        w.Print(String.concat " " xs)

type private TypeKind =
    | TKInterface
    | TKEnum
    | TKStruct
    | TKDelegate
    | TKClass

let rec private dumpType (w : Writer) (typeDef : TypeDef) (nestingStack : TypeDef list) =
    // TODO: built-in attributes
    let header = Collections.Generic.List<string>()
    match typeDef.flags &&& TypeAttributes.VisibilityMask with
    | TypeAttributes.NotPublic -> () // TODO: expect not nested
    | TypeAttributes.Public -> header.Add("public") // TODO: expect not nested
    | TypeAttributes.NestedPublic -> header.Add("public") // TODO: expect nested
    | TypeAttributes.NestedPrivate -> () // TODO: expect nested
    | TypeAttributes.NestedFamily -> header.Add("protected") // TODO: expect nested
    | TypeAttributes.NestedAssembly -> header.Add("internal") // TODO: expect nested
    | TypeAttributes.NestedFamANDAssem -> () // TODO: not available in C#; expect nested
    | TypeAttributes.NestedFamORAssem -> header.Add("protected internal") // TODO: expect nested
    | _ -> () // never reached
    let isInterface = (typeDef.flags &&& TypeAttributes.Interface) = TypeAttributes.Interface
    let isSealed = (typeDef.flags &&& TypeAttributes.Sealed) = TypeAttributes.Sealed
    let isAbstract = (typeDef.flags &&& TypeAttributes.Abstract) = TypeAttributes.Abstract
    let typeKind =
        if isInterface then
            if not(not isSealed && isAbstract) then
                w.Warn("enum not non-sealed and abstract as it should be")
            header.Add("interface")
            TKInterface
        else
            let handleClass() =
                match isSealed, isAbstract with
                | false, false -> ()
                | true, false -> header.Add("sealed")
                | false, true -> header.Add("abstract")
                | true, true -> header.Add("static")
                header.Add("class")
                TKClass
            match typeDef.baseType with
            | Some(TypeSpec.Choice1Of2 typeRef) ->
                match typeRef.typeNamespace, typeRef.typeName with
                | "System", "Enum" ->
                    if not(isSealed && not isAbstract) then
                        w.Warn("enum not sealed and non-abstract as it should be")
                    header.Add("enum")
                    TKEnum
                | "System", "ValueType" when (typeDef.typeNamespace, typeDef.typeName) <> ("System", "Enum") ->
                    if not(isSealed && not isAbstract) then
                        w.Warn("struct not sealed and non-abstract as it should be")
                    header.Add("struct")
                    TKStruct
                | "System", "MulticastDelegate" ->
                    if not(isSealed && not isAbstract) then
                        w.Warn("delegate not sealed and non-abstract as it should be")
                    header.Add("delegate")
                    TKDelegate
                | _ -> handleClass()
            | _ ->
                handleClass()

    let genVars = [| for p in typeDef.genericParams -> p.name |]
    let varMap : GenVarMap = { vars = genVars; mvars = null }

    dumpAttrs w varMap typeDef.customAttrs

    match nestingStack with
    | [] -> ()
    | enclosingType :: _ ->
        if typeDef.genericParams.Length < enclosingType.genericParams.Length then
            w.Warn("expected nested type to have at least as many generic parameters as its enclosing type")

    let simpleName =
        match typeDef.genericParams with
        | [||] -> typeDef.typeName
        | ps ->
            let n = 
                match nestingStack with
                | [] -> ps.Length
                | enclosingType :: _ -> ps.Length - enclosingType.genericParams.Length
            if n > 0 then
                let expectedSuffix = "`" + intToStr n
                if typeDef.typeName.EndsWith(expectedSuffix) then
                    typeDef.typeName.[0 .. typeDef.typeName.Length - expectedSuffix.Length - 1]
                else
                    w.Warn("generic type name missing `n suffix")
                    typeDef.typeName
            else
                typeDef.typeName

    let name =
        if typeDef.genericParams.Length = 0 then
            simpleName
        else
            simpleName + "<" + (String.concat ", " genVars) + ">"
    header.Add(name)

    let inheritanceList = [
        match typeKind with
        | TKDelegate | TKStruct -> ()
        | TKInterface ->
            for intf in typeDef.interfaces do
                yield typeSpecToStr varMap None intf
        | TKEnum -> // TODO
            let f = typeDef.fields |> Seq.find (fun field -> field.name = "value__" && (field.flags &&& FieldAttributes.SpecialName) = FieldAttributes.SpecialName)
            if f.typeSig <> TypeSig.I4 then
                yield typeSigToStr varMap None f.typeSig
        | TKClass ->
            match typeDef.baseType with
            | None -> ()
            | Some typeSpec ->
                let name = typeSpecToStr varMap None typeSpec
                if name <> "System.Object" then
                    yield name
            for intf in typeDef.interfaces do
                yield typeSpecToStr varMap None intf
    ]

    if not(List.isEmpty inheritanceList) then
        header.Add(": " + (String.concat ", " inheritanceList))

    // TODO: generic param constraints (where %s : %s ...)

    // TODO: special processing for interfaces, enums, delegates

    w.Print((String.concat " " header) + " {")
    w.Enter()
    for nestedType in typeDef.nestedTypes do
        dumpType w nestedType (typeDef :: nestingStack)
    for fld in typeDef.fields do
        dumpField w varMap fld
    for mth in typeDef.methods do
        dumpMethod w varMap simpleName mth
    // TODO: properties, events
    w.Leave()
    w.Print("}")

let dump (m : Module) =
    let w = Writer()
    let genVarMap : GenVarMap = { vars = Array.empty; mvars = Array.empty }
    for attr in m.customAttrs do
        w.Print("[module: " + attrToStr genVarMap attr + "]")
    m.assembly |> Option.iter (fun asm ->
        for attr in asm.customAttrs do
            w.Print("[assembly: " + attrToStr genVarMap attr + "]")
    )
    for typeDef in m.typeDefs do
        let nens = typeDef.typeNamespace.Length <> 0
        if nens then
            w.Print(sprintf "namespace %s {" typeDef.typeNamespace)
            w.Enter()
        dumpType w typeDef []
        if nens then
            w.Leave()
            w.Print("}")
        w.Print()
    // TODO: global fields & methods
