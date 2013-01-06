module Roma.Cli.Compiler.Main

open Roma.Cli
open Roma.Compiler
open Roma.Compiler.Dwarf

let getNamespace fqn ns =
    if System.String.IsNullOrEmpty(fqn) then
        ns
    else
        let rec loop (ns : INamespace) (path : string) =
            let p = path.IndexOf('.')
            if p >= 0 then
                let hd = path.[.. p - 1]
                let tl = path.[p + 1 ..]
                loop (ns.GetNamespace(hd)) tl
            else
                ns.GetNamespace(path)
        loop ns fqn

let compile (m : Module) (compileUnit : CompileUnit) =
    for typeDef in m.typeDefs do
        let rec addType (scope : ITypeContainer) (typeDef : TypeDef) =

            let addMembers (entry : MemberContainer) =
                for mth in typeDef.methods do
                    entry.AddMember(mth.name) |> ignore
                for fld in typeDef.fields do
                    entry.AddMember(fld.name) |> ignore
                match (entry :> obj) with
                | :? ITypeContainer as enclosingType ->
                    for nestedType in typeDef.nestedTypes do
                        addType enclosingType nestedType
                | _ -> ()

            let addValues (entry : EnumTypeEntry) =
                for fld in typeDef.fields do
                    if (fld.flags &&& FieldAttributes.Static) = FieldAttributes.Static then
                        match fld.constant with
                        | Some c ->
                            let value =
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
                            entry.AddValue(fld.name, value)
                        | None -> () // TODO: emit diagnostic
                    else
                        // TODO: check that there is exactly one such field
                        let kind =
                            match fld.typeSig with
                            | Boolean -> PrimitiveTypeKind.Bool
                            | Char -> PrimitiveTypeKind.Char16
                            | I1 -> PrimitiveTypeKind.SInt8
                            | U1 -> PrimitiveTypeKind.UInt8
                            | I2 -> PrimitiveTypeKind.SInt16
                            | U2 -> PrimitiveTypeKind.UInt16
                            | I4 -> PrimitiveTypeKind.SInt32
                            | U4 -> PrimitiveTypeKind.UInt32
                            | I8 -> PrimitiveTypeKind.SInt64
                            | U8 -> PrimitiveTypeKind.UInt64
                            | I -> PrimitiveTypeKind.SIntPtr
                            | U -> PrimitiveTypeKind.UIntPtr
                            | _ -> failwith "Invalid underlying type for enum."
                        entry.SetUnderlyingType(compileUnit.GetPrimitiveType(kind))

            let name = typeDef.typeName
            if (typeDef.flags &&& TypeAttributes.ClassSemanticsMask) = TypeAttributes.Interface then
                scope.CreateInterfaceType(name) |> addMembers
            else
                // FIXME: check of base type is name-based
                if (typeDef.typeNamespace, typeDef.typeName) = ("System", "Enum") then
                    scope.CreateClassType(name) |> addMembers
                else
                    match typeDef.baseType with
                    | Some(TypeSpec.Choice1Of2 { typeNamespace = "System"; typeName = "Enum" }) ->
                        scope.CreateEnumType(name) |> addValues
                    | Some(TypeSpec.Choice1Of2 { typeNamespace = "System"; typeName = "ValueType" }) ->
                        scope.CreateStructType(name) |> addMembers
                    | _ -> scope.CreateClassType(name) |> addMembers

        let ns = getNamespace typeDef.typeNamespace compileUnit
        addType (ns :> ITypeContainer) typeDef

    // TODO

