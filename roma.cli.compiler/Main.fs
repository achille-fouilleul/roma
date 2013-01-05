module Roma.Cli.Compiler.Main

open Roma.Cli
open Roma.Compiler
open Roma.Compiler.Dwarf

let compile (m : Module) (compileUnit : CompileUnit) =
    let getNamespace fqn =
        let globalNs = compileUnit :> INamespace
        if System.String.IsNullOrEmpty(fqn) then
            globalNs
        else
            let rec loop (ns : INamespace) (path : string) =
                let p = path.IndexOf('.')
                if p >= 0 then
                    let hd = path.[.. p - 1]
                    let tl = path.[p + 1 ..]
                    loop (ns.GetNamespace(hd)) tl
                else
                    ns.GetNamespace(path)
            loop globalNs fqn

    for typeDef in m.typeDefs do
        let rec addType (scope : ITypeContainer) typeDef =
            let entry =
                let name = typeDef.typeName
                if (typeDef.flags &&& TypeAttributes.ClassSemanticsMask) = TypeAttributes.Interface then
                    scope.CreateInterfaceType(name) :> TypeEntry
                else
                    // TODO: use CreateStructType or CreateEnumType
                    scope.CreateClassType(name) :> TypeEntry
            match (entry :> obj) with
            | :? ITypeContainer as enclosingType ->
                for nestedType in typeDef.nestedTypes do
                    addType enclosingType nestedType
            | _ -> ()
            // TODO
        let ns = getNamespace typeDef.typeNamespace
        addType (ns :> ITypeContainer) typeDef

    // TODO

