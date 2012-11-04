module Roma.Cli.Dump.CSharp

open System
open Roma.Cli
open Internal.StrUtil
open Internal.CSharp.TypeToStr
open Internal.CSharp.CustomAttrToStr

let private dumpAttrs (w : Writer) resolveTypeRef varMap (attrs : seq<CustomAttribute>) =
    for attr in attrs do
        w.Print("[" + attrToStr resolveTypeRef varMap attr + "]")

let private dumpProp (w : Writer) resolveTypeRef varMap (prop : Property) =
    dumpAttrs w resolveTypeRef varMap prop.customAttrs

    // TODO: attributes
    // TODO: handle PropertyAttributes.HasDefault
    // TODO: visibility

    let xs = Collections.Generic.List<string>()

    if not prop.hasThis then
        xs.Add("static")

    xs.Add(typeSigToStr resolveTypeRef varMap None prop.retType)

    if prop.paramTypes.Length <> 0 then
        let ps = Seq.map (typeSigToStr resolveTypeRef varMap None) prop.paramTypes
        xs.Add("this[" + String.concat ", " ps + "]") // TODO: param names
    else
        xs.Add(prop.name)

    xs.Add("{")

    for sem, mth in prop.methods do
        // TODO: attributes, visibility, virtuality, etc. of referenced methods
        match sem with
        | sem when sem.HasFlag(MethodSemanticsAttribute.Getter) -> xs.Add("get;")
        | sem when sem.HasFlag(MethodSemanticsAttribute.Setter) -> xs.Add("set;")
        | _ -> failwith "Invalid property semantics."

    xs.Add("}")

    w.Print(String.concat " " xs)

let private dumpEvent (w : Writer) resolveTypeRef varMap (evt : Event) =
    dumpAttrs w resolveTypeRef varMap evt.customAttrs

    let xs = Collections.Generic.List<string>()

    // TODO: visibility, static

    xs.Add("event")

    evt.typeRef |> Option.iter (fun typeRef -> xs.Add(typeSpecToStr resolveTypeRef varMap None typeRef))

    xs.Add(evt.name + ";")

    w.Print(String.concat " " xs)

let private dumpField (w : Writer) resolveTypeRef varMap (fld : FieldDef) =
    dumpAttrs w resolveTypeRef varMap fld.customAttrs
    // TODO: attributes

    let xs = Collections.Generic.List<string>()

    match fld.flags &&& FieldAttributes.FieldAccessMask with
    | FieldAttributes.CompilerControlled -> xs.Add("__compilercontrolled") // TODO: warn; not supported in C#
    | FieldAttributes.Private -> if false then xs.Add("private")
    | FieldAttributes.FamANDAssem -> xs.Add("__famandassem") // TODO: warn; not supported in C#
    | FieldAttributes.Assembly -> xs.Add("internal")
    | FieldAttributes.Family -> xs.Add("protected")
    | FieldAttributes.FamORAssem -> xs.Add("protected internal")
    | FieldAttributes.Public -> xs.Add("public")
    | _ -> () // not reached

    if (fld.flags &&& FieldAttributes.Static) = FieldAttributes.Static then
        xs.Add(if Option.isSome fld.constant then "const" else "static")

    if (fld.flags &&& FieldAttributes.InitOnly) = FieldAttributes.InitOnly then
        xs.Add("readonly")

    xs.Add(typeSigToStr resolveTypeRef varMap None fld.typeSig)
    xs.Add(fld.name);
    match fld.constant with
    | None -> ()
    | Some constant ->
        // TODO: warn if non-static
        xs.Add("= " + constantToCSharpStr constant)
    w.Print((String.concat " " xs) + ";")

let private makeWhereClause resolveTypeRef varMap (genPar : GenericParam) =
    let constraints = Collections.Generic.List<_>()
    if genPar.flags.HasFlag(GenericParamAttributes.ReferenceTypeConstraint) then
        constraints.Add("class")
    if genPar.flags.HasFlag(GenericParamAttributes.NotNullableValueTypeConstraint) then
        constraints.Add("struct")
    for typeSpec in genPar.constraints do
        constraints.Add(typeSpecToStr resolveTypeRef varMap None typeSpec)
    if genPar.flags.HasFlag(GenericParamAttributes.DefaultConstructorConstraint) then
        constraints.Add("new()")
    if constraints.Count <> 0 then
        "where " + genPar.name + " : " + String.concat ", " constraints
    else
        null

let private makeGenericParamsStr (gps : seq<GenericParam>) =
    let xs =
        seq {
            for gp in gps ->
                // TODO: variance (in: contravariant / out: covariant)
                match gp.flags &&& GenericParamAttributes.VarianceMask with
                | GenericParamAttributes.Covariant -> "out " + gp.name
                | GenericParamAttributes.Contravariant -> "in " + gp.name
                | _ -> gp.name
        }
    "<" + String.concat ", " xs + ">"

let private dumpMethod w resolveTypeRef varMap ownerTypeName (mth : MethodDef) =
    let genMvars = [| for p in mth.genericParams -> p.name |]
    let genVarMap = { varMap with mvars = genMvars }
    dumpAttrs w resolveTypeRef genVarMap mth.customAttrs

    // TODO: other attributes (e.g. pinvokes, etc.)

    mth.retVal |> Option.iter (
        fun (par : ParamDef) ->
            for attr in par.customAttrs do
                w.Print("[return: " + attrToStr resolveTypeRef genVarMap attr + "]")
    )

    let xs = Collections.Generic.List<string>()

    match mth.flags &&& MethodAttributes.MemberAccessMask with
    | MethodAttributes.CompilerControlled -> xs.Add("__compilercontrolled") // TODO: warn; not supported in C#
    | MethodAttributes.Private -> if false then xs.Add("private")
    | MethodAttributes.FamANDAssem -> xs.Add("__famandassem") // TODO: warn; not supported in C#
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
        if mth.genericParams.Length <> 0 then
            mth.name + makeGenericParamsStr mth.genericParams
        else
            mth.name
    // TODO: retval attributes
    // TODO: vararg methods (__arglist)
    // TODO: C# params keyword (ParamArrayAttribute)
    // TODO: C# unsafe keyword if pointers involved
    let retTypeStr = typeSigToStr resolveTypeRef genVarMap None mth.signature.retType
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
            let typeStr = typeSigToStr resolveTypeRef genVarMap byRefDir typeSig
            seq {
                match parOpt with
                | None -> yield typeStr
                | Some par ->
                    for attr in par.customAttrs do
                        yield "[" + attrToStr resolveTypeRef genVarMap attr + "]"
                    yield typeStr
                    yield par.name
                    match par.constant with
                    | None -> ()
                    | Some constant ->
                        yield "= " + constantToCSharpStr constant
            }
            |> String.concat " "
    }
    let paramsStr =  "(" + String.concat ", " pars + ")"
    match mth.name with
    | ".ctor"
    | ".cctor" ->
        // TODO: warn if return type is not void
        // TODO: check for specialname & rtspecialname flags
        xs.Add(ownerTypeName + paramsStr)
        w.Print(String.concat " " xs + ";")
    | "Finalize" when true (* TODO: non-generic, return type = void, no parameters... *) ->
        w.Print("~" + ownerTypeName + "();")
    | _ -> // TODO: operators
        xs.Add(retTypeStr)
        xs.Add(name + paramsStr)
        for genPar in mth.genericParams do
            let whereClause = makeWhereClause resolveTypeRef genVarMap genPar
            if whereClause <> null then
                xs.Add(whereClause)
        w.Print(String.concat " " xs + ";")
    // TODO: method body

type private TypeKind =
    | TKInterface
    | TKEnum
    | TKStruct
    | TKDelegate
    | TKClass

let rec private dumpType (w : Writer) resolveTypeRef (typeDef : TypeDef) (nestingStack : TypeDef list) =
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
                // TODO: better check (i.e. not only name-based)
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

    dumpAttrs w resolveTypeRef varMap typeDef.customAttrs

    match nestingStack with
    | [] -> ()
    | enclosingType :: _ ->
        if typeDef.genericParams.Length < enclosingType.genericParams.Length then
            w.Warn("expected nested type to have at least as many generic parameters as its enclosing type")

    let simpleName, name, newGenPars =
        let i =
            match nestingStack with
            | [] -> 0
            | enclosingTypeDef :: _ -> enclosingTypeDef.genericParams.Length
        let n = typeDef.genericParams.Length

        let simpleName =
            let m = n - i
            if m > 0 then
                let expectedSuffix = "`" + intToStr m
                if typeDef.typeName.EndsWith(expectedSuffix) then
                    typeDef.typeName.[.. typeDef.typeName.Length - expectedSuffix.Length - 1]
                else
                    w.Warn("generic type name missing `n suffix")
                    typeDef.typeName
            else
                typeDef.typeName

        if i < n then
            let newGenPars = typeDef.genericParams.[i .. n - 1]
            let name = simpleName + makeGenericParamsStr newGenPars
            simpleName, name, newGenPars
        else
            simpleName, simpleName, [||]
    header.Add(name)

    let inheritanceList = [
        match typeKind with
        | TKDelegate | TKStruct -> ()
        | TKInterface ->
            for intf in typeDef.interfaces do
                yield typeSpecToStr resolveTypeRef varMap None intf
        | TKEnum ->
            match enumUnderlyingType typeDef with
            | I4 -> ()
            | typeSig ->
                yield typeSigToStr resolveTypeRef varMap None typeSig
        | TKClass ->
            match typeDef.baseType with
            | None -> ()
            | Some typeSpec ->
                // TODO: better check (i.e. not only name-based)
                let name = typeSpecToStr resolveTypeRef varMap None typeSpec
                if name <> "System.Object" then
                    yield name
            for intf in typeDef.interfaces do
                yield typeSpecToStr resolveTypeRef varMap None intf
    ]

    if not(List.isEmpty inheritanceList) then
        header.Add(": " + (String.concat ", " inheritanceList))

    for genPar in newGenPars do
        let whereClause = makeWhereClause resolveTypeRef varMap genPar
        if whereClause <> null then
            header.Add(whereClause)

    // TODO: special processing for interfaces, enums, delegates

    match typeKind with
    | TKInterface | TKStruct | TKClass ->
        w.Print(String.concat " " header + " {")
        w.Enter()
        for nestedType in typeDef.nestedTypes do
            dumpType w resolveTypeRef nestedType (typeDef :: nestingStack)
        for prop in typeDef.properties do
            dumpProp w resolveTypeRef varMap prop
        for evt in typeDef.events do
            dumpEvent w resolveTypeRef varMap evt
        for fld in typeDef.fields do
            dumpField w resolveTypeRef varMap fld
        for mth in typeDef.methods do
            dumpMethod w resolveTypeRef varMap simpleName mth
        w.Leave()
        w.Print("}")

    | TKEnum ->
        // TODO: warn if not standard-compliant
        w.Print(String.concat " " header + " {")
        w.Enter()
        let rec loop fields =
            let last =
                match fields with
                | [ fld ] -> true
                | _ -> false
            match fields with
            | (fld : FieldDef) :: tl ->
                dumpAttrs w resolveTypeRef varMap fld.customAttrs
                match fld.constant with
                | None -> ()
                | Some constant ->
                    let s = fld.name + " = " + constantToCSharpStr constant
                    if last then
                        w.Print(s)
                    else
                        w.Print(s + ",")
                loop tl
            | [] -> ()
        loop typeDef.fields
        w.Leave()
        w.Print("}")

    | TKDelegate ->
        // TODO: warn if not standard-compliant
        w.Print(String.concat " " header + ";")

let private findAssembly assemblyRef refs =
    try
        let _, m =
            refs
            |> Seq.find (
                fun (path, m) ->
                    match m.assembly with
                    | Some a when a.name = assemblyRef.Name -> true // TODO: better check
                    | _ -> false
            )
        m
    with :? Collections.Generic.KeyNotFoundException ->
        failwithf "Could not resolve assembly '%s'" assemblyRef.Name

let private findTypeDef (m : Module) (typeRef : TypeRef) =
    // TODO: performance: use a Map
    try
        m.typeDefs
        |> Seq.find (
            fun typeDef ->
                (typeDef.typeNamespace, typeDef.typeName) = (typeRef.typeNamespace, typeRef.typeName)
        )
    with :? Collections.Generic.KeyNotFoundException ->
        let typeName =
            match typeRef.typeNamespace with
            | "" -> typeRef.typeName
            | _ -> typeRef.typeNamespace + "." + typeRef.typeName
        match m.assembly with
        | Some a -> failwith "Could not resolve type '%s' in assembly '%s'." typeName a.name
        | None -> failwith "Could not resolve type '%s' in module '%s'." typeName m.moduleName

let dump refs (m : Module) =
    let w = Writer()

    let rec resolveTypeRef typeRef =
        // TODO: performance: use Maps
        match typeRef.scope with
        | None -> findTypeDef m typeRef
        | Some(TypeRefScope(TypeSpec.Choice1Of2 enclosingTypeRef)) ->
            let enclosingTypeDef = resolveTypeRef enclosingTypeRef
            enclosingTypeDef.nestedTypes
            |> Seq.find (fun typeDef -> typeDef.typeName = typeRef.typeName) // TODO: error handling
        | Some(AssemblyRefScope assemblyRef) ->
            let m = findAssembly assemblyRef refs
            findTypeDef m typeRef
        | Some(_) ->
            Diagnostics.Debugger.Break()
            raise(NotImplementedException()) // TODO

    let genVarMap : GenVarMap = { vars = Array.empty; mvars = Array.empty }
    for attr in m.customAttrs do
        w.Print("[module: " + attrToStr resolveTypeRef genVarMap attr + "]")
    m.assembly |> Option.iter (fun asm ->
        for attr in asm.customAttrs do
            w.Print("[assembly: " + attrToStr resolveTypeRef genVarMap attr + "]")
    )
    for typeDef in m.typeDefs do
        let nens = typeDef.typeNamespace.Length <> 0
        if nens then
            w.Print(sprintf "namespace %s {" typeDef.typeNamespace)
            w.Enter()
        dumpType w resolveTypeRef typeDef []
        if nens then
            w.Leave()
            w.Print("}")
        w.Print()
    // TODO: global fields & methods
    