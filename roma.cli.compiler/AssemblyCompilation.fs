module Roma.Cli.Compiler.AssemblyCompilation

open System
open System.IO
open Roma.Cli
open Roma.Cli.Compiler
open Roma.Compiler

let private int128OfConstant c =
    match c with
    | ConstantBool false -> Int128.Zero
    | ConstantBool true -> Int128.One
    | ConstantChar x -> Int128(uint32 x)
    | ConstantI1 x -> Int128(int32 x)
    | ConstantU1 x -> Int128(uint32 x)
    | ConstantI2 x -> Int128(int32 x)
    | ConstantU2 x -> Int128(uint32 x)
    | ConstantI4 x -> Int128(x)
    | ConstantU4 x -> Int128(x)
    | ConstantI8 x -> Int128(x)
    | ConstantU8 x -> Int128(x)
    | _ -> failwith "Invalid constant type for enum."

let compileAssemblies addrSize outputPath (asms : Roma.Cli.Compiler.Assembly list) =
    let typeLayoutMan = TypeLayoutManager(addrSize)
    let debugInfo = DwarfDebugInfo(addrSize)
    let compileUnit = debugInfo.CompileUnit
    let incompleteTypes = System.Collections.Generic.List<_>()
    let incompleteMethods = System.Collections.Generic.List<_>()

    let rec lookup (info : CompositeTypeInfo) =
        let getTypeEntry (typeRef : TypeRef) (tc : ITypeContainer) name =
            match tc.TryFindType(name) with
            | Some te -> te
            | None ->
                let typeDef = typeRef.TypeDef
                match typeRef with
                | IsEnum kind ->
                    let ete = tc.CreateEnumType(name)
                    let ut = compileUnit.GetPrimitiveType(kind)
                    ete.SetUnderlyingType(ut)
                    ete.SetByteSize(ut.ByteSize)
                    for fld in typeDef.fields do
                        match fld.constant with
                        | Some constant when fld.IsStatic ->
                            ete.AddValue(fld.name, int128OfConstant constant)
                        | _ -> ()
                    ete :> TypeEntry
                | _ ->
                    let t = mkTypeInfo typeRef info.genArgs
                    let cte =
                        if typeDef.IsInterface then
                            tc.CreateInterfaceType(name) :> CompositeTypeBase
                        else
                            if typeRef.IsValueType then
                                tc.CreateStructType(name) :> CompositeTypeBase
                            else
                                tc.CreateClassType(name) :> CompositeTypeBase
                    incompleteTypes.Add((t, cte))
                    cte :> TypeEntry

        let rec loop =
            function
            | TopLevelTypeRef(m, ns, name) as typeRef ->
                // FIXME: will not work correctly if two or more modules have the same ScopeName
                let m' = compileUnit.GetModule(m.ScopeName) :> INamespace
                let ns' =
                    if String.IsNullOrEmpty(ns) then
                        m'
                    else
                        m'.GetNamespace(ns)
                getTypeEntry typeRef ns' name
            | NestedTypeRef(enclosingTypeRef, name) as typeRef ->
                getTypeEntry typeRef (loop enclosingTypeRef :> obj :?> ITypeContainer) name

        loop info.typeRef

    and translateType =
        function
        | VoidType -> failwith "Cannot translate void type."
        | PrimitiveType kind -> compileUnit.GetPrimitiveType(kind) :> TypeEntry
        | EnumType(kind, typeRef) -> mkTypeInfo typeRef [] |> lookup
        | CompositeType info -> lookup info
        | ArrayType info -> compileUnit.GetManagedArrayType(translateType info.elemType, info.shape) :> TypeEntry
        | GCRefType t -> compileUnit.GetManagedPointerType(translateType t) :> TypeEntry
        | ByRefType t -> compileUnit.GetReferenceType(Some(translateType t)) :> TypeEntry
        | PointerType t ->
            match t with
            | VoidType -> None
            | _ -> Some(translateType t)
            |> compileUnit.GetPointerType
            :> TypeEntry
        | VolatileModifier t -> compileUnit.GetVolatileType(translateType t) :> TypeEntry
        | PinnedModifier t -> translateType t // TODO

    let setupMethod (m : Roma.Cli.Compiler.Module) (mth : MethodDef) genArgs (sub : Subprogram) =
        let translateTypeSig typeSig =
            m.TranslateToType(typeSig, genArgs)

        match translateTypeSig mth.signature.retType with
        | VoidType -> None
        | retType -> Some(translateType retType)
        |> sub.SetReturnType

        for tsig, p in Seq.zip mth.signature.paramTypes mth.parameters do
            let paramType = tsig |> translateTypeSig |> translateType
            let name = p |> Option.map (fun p -> p.name)
            sub.AddParameter(paramType, name) |> ignore

        if mth.signature.callConv.callKind = CallKind.Vararg then
            sub.AddUnspecifiedParameters()

        mth.body |> Option.iter (
            fun body ->

                incompleteMethods.Add((body, sub))

                for loc in body.locals do
                    translateTypeSig loc |> ignore
                for ec in body.excClauses do
                    match ec with
                    | Catch(_, ts) ->
                        m.TranslateTypeSpecToType(ts, genArgs) |> ignore // TODO
                    | _ -> ()
                for _, instr in body.instrs do
                    match instr with
                    | Cpobj typeSpec
                    | Ldobj typeSpec
                    | Castclass typeSpec
                    | Isinst typeSpec
                    | Unbox typeSpec
                    | Stobj typeSpec
                    | Box typeSpec
                    | Newarr typeSpec
                    | Ldelema typeSpec
                    | Ldelem typeSpec
                    | Stelem typeSpec
                    | Unbox_any typeSpec
                    | Refanyval typeSpec
                    | Mkrefany typeSpec
                    | Ldtoken_type typeSpec
                    | Initobj typeSpec
                    | Constrained typeSpec
                    | Sizeof typeSpec ->
                        m.TranslateTypeSpecToType(typeSpec, genArgs) |> ignore // TODO
                    | Jmp methodSpec
                    | Call methodSpec
                    | Callvirt methodSpec
                    | Newobj methodSpec
                    | Ldtoken_method methodSpec ->
                        methodSpec.methodRef.typeRef |> Option.iter (
                            fun ts ->
                                m.TranslateTypeSpecToType(ts, genArgs) |> ignore // TODO
                        )
                    | Ldfld fieldRef
                    | Ldflda fieldRef
                    | Stfld fieldRef
                    | Ldsfld fieldRef
                    | Ldsflda fieldRef
                    | Stsfld fieldRef
                    | Ldtoken_field fieldRef ->
                        fieldRef.typeRef |> Option.iter (
                            fun ts ->
                                m.TranslateTypeSpecToTypeInfo(ts, genArgs) |> ignore // TODO
                        )
                    | _ -> ()
        )

    for asm in asms do
        for m in asm.Modules do
            let rec visit (types : seq<TypeRef>) =
                for typeRef in types do
                    let typeDef = typeRef.TypeDef
                    if not typeDef.IsGeneric then
                        let t = mkTypeInfo typeRef []
                        lookup t |> ignore // create type
                    visit typeRef.NestedTypes
            visit m.Types
            for mth in m.GlobalMethods do
                if not mth.IsGeneric then
                    if not mth.IsStatic then
                        failwith "Non-static global method"
                    let sub = compileUnit.GetModule(m.ScopeName).AddSubprogram(mth.name)
                    setupMethod m mth ([], []) sub

    let complete (t : CompositeTypeInfo) (cte : CompositeTypeBase) =
        let m = t.Module
        let genArgs = (t.genArgs, [])
        let translateTypeSig typeSig =
            m.TranslateToType(typeSig, genArgs)

        let td = t.TypeDef

        for p, a in Seq.zip td.genericParams t.genArgs do
            cte.AddTypeParameter(p.name, translateType a)

        if not td.IsInterface then
            let layout = typeLayoutMan.GetCompositeTypeLayout(t)
            cte.SetByteSize(layout.size)
            for fldLayout in layout.fields do
                match fldLayout.field with
                | VtablePtr | SyncBlock -> () // TODO?
                | UserField fld ->
                    let mem = cte.AddMember(fld.name)
                    mem.SetType(translateTypeSig fld.typeSig |> translateType)
                    mem.SetDataMemberLocation(int fldLayout.offset)

        for mth in td.methods do
            if not mth.IsGeneric then
                let sub = cte.AddSubprogram(mth.name)
                if mth.IsVirtual then
                    sub.SetVirtual(mth.IsAbstract)

                if not mth.IsStatic then
                    let paramType =
                        match cte :> obj with
                        | :? StructTypeEntry ->
                            compileUnit.GetReferenceType(Some(cte :> TypeEntry)) :> TypeEntry
                        | _ ->
                            compileUnit.GetManagedPointerType(cte) :> TypeEntry
                    let param = sub.AddParameter(paramType, None)
                    param.SetArtificial()
                    sub.SetObjectPointer(param)

                setupMethod m mth genArgs sub

    let rec loop() =
        let newTypes = Seq.toArray incompleteTypes
        if Array.isEmpty newTypes then
            for body, sub in incompleteMethods do
                () // TODO
        else
            incompleteTypes.Clear()
            for t, cte in newTypes do
                complete t cte
            loop()
    loop()

    printfn "%d methods" incompleteMethods.Count

    let lines = debugInfo.Serialize()

    if outputPath = "-" then
        let buffer = System.Text.StringBuilder()
        for line in lines do
            buffer.AppendLine(line) |> ignore
        Console.Out.Write(buffer.ToString())
    else
        File.WriteAllLines(outputPath, lines)

