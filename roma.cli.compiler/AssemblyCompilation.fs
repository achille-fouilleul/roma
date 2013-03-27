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
        | ArrayType(t, shape) -> compileUnit.GetManagedArrayType(translateType t, shape) :> TypeEntry
        | GCRefType t -> compileUnit.GetManagedPointerType(translateType t) :> TypeEntry
        | ByRefType t -> compileUnit.GetReferenceType(Some(translateType t)) :> TypeEntry
        | PointerType t ->
            match t with
            | VoidType -> None
            | _ -> Some(translateType t)
            |> compileUnit.GetPointerType
            :> TypeEntry
        | VolatileModifier t -> compileUnit.GetVolatileType(translateType t) :> TypeEntry

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

    let complete (t : CompositeTypeInfo) (cte : CompositeTypeBase) =
        let m = t.Module
        let genArgs = (t.genArgs, [])
        let translateTypeSig typeSig =
            translateType (m.TranslateToType(typeSig, genArgs))

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
                    mem.SetType(translateTypeSig fld.typeSig)
                    mem.SetDataMemberLocation(int fldLayout.offset)

        for mth in t.Methods do
            if not mth.IsGeneric then
                let def = mth.methodDef
                let sub = cte.AddSubprogram(mth.methodDef.name)
                let flags = def.flags

                if (flags &&& MethodAttributes.Virtual) = MethodAttributes.Virtual then
                    sub.SetVirtual((flags &&& MethodAttributes.Abstract) = MethodAttributes.Abstract)

                match mth.ReturnType with
                | VoidType -> None
                | retType -> Some(translateType retType)
                |> sub.SetReturnType

                if def.signature.callConv.hasThis then
                    let paramType =
                        match cte :> obj with
                        | :? StructTypeEntry ->
                            compileUnit.GetReferenceType(Some(cte :> TypeEntry)) :> TypeEntry
                        | _ ->
                            compileUnit.GetManagedPointerType(cte) :> TypeEntry
                    let param = sub.AddParameter(paramType, None)
                    param.SetArtificial()
                    sub.SetObjectPointer(param)

                for paramType, p in Seq.zip mth.ParamTypes mth.methodDef.parameters do
                    let name = p |> Option.map (fun p -> p.name)
                    sub.AddParameter(translateType paramType, name) |> ignore

                if def.signature.callConv.callKind = CallKind.Vararg then
                    sub.AddUnspecifiedParameters()

    let rec loop() =
        let newTypes = Seq.toArray incompleteTypes
        incompleteTypes.Clear()
        if not(Array.isEmpty newTypes) then
            printfn "%s: %d incomplete types" asms.Head.Name.name newTypes.Length
            for t, cte in newTypes do
                complete t cte
            loop()
    loop()

    let lines = debugInfo.Serialize()

    if outputPath = "-" then
        let buffer = System.Text.StringBuilder()
        for line in lines do
            buffer.AppendLine(line) |> ignore
        Console.Out.Write(buffer.ToString())
    else
        File.WriteAllLines(outputPath, lines)

