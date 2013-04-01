namespace Roma.Cli.Compiler

open System
open System.Security.Cryptography
open System.IO
open Roma.Cli

[<AutoOpen>]
module private HexUtils =
    let toHexDigit x =
        if x < 10uy then '0' + char x else 'a' + char(x - 10uy)

    let toHexString (blob : byte array) =
        let chars =
            [|
                for x in blob do
                    yield toHexDigit(x >>> 4)
                    yield toHexDigit(x &&& 0xfuy)
            |]
        System.String(chars)

type PublicKeyOrToken =
    | PublicKey of byte array
    | PublicKeyToken of byte array

    static member FromAssemblyRef(assemblyRef : AssemblyRef) =
        let blob = assemblyRef.publicKeyOrToken
        if blob <> null && blob.Length <> 0 then
            if (assemblyRef.flags &&& AssemblyFlags.PublicKey) = AssemblyFlags.PublicKey then
                Some (PublicKey blob)
            else
                Some (PublicKeyToken blob)
        else
            // TODO: check that PublicKey flag is off
            None

    member this.Token =
        match this with
        | PublicKey key ->
            use sha1 = SHA1Managed.Create()
            let hash = sha1.ComputeHash(key)
            hash.[(Array.length hash - 8) ..]
            |> Array.rev
        | PublicKeyToken token -> token

    static member Matches ref def =
        match ref with
        | None -> true
        | Some (ref : PublicKeyOrToken) ->
            match def with
            | None -> false
            | Some (def : PublicKeyOrToken) ->
                match (ref, def) with
                | (PublicKey refKey, PublicKey defKey) -> refKey = defKey
                | _ -> ref.Token = def.Token

type AssemblyName =
    {
        name : string
        version : Roma.Cli.Version
        culture : string
        publicKeyOrToken : PublicKeyOrToken option
    }

    static member FromAssemblyManifest(manifest : Assembly) =
        let name : AssemblyName =
            {
                name = manifest.name
                version = manifest.version
                culture = manifest.culture
                publicKeyOrToken =
                    if manifest.publicKey <> null then
                        Some (PublicKey manifest.publicKey)
                    else
                        None
            }
        name

    static member FromAssemblyRef(assemblyRef : AssemblyRef) =
        let name : AssemblyName =
            {
                name = assemblyRef.name
                version = assemblyRef.version
                culture = assemblyRef.culture
                publicKeyOrToken = PublicKeyOrToken.FromAssemblyRef(assemblyRef)
            }
        name

    member this.Matches(candidate) =
        match candidate with
        | _ when this.name <> candidate.name -> false
        (*
        FIXME:
        http://msdn.microsoft.com/en-us/library/system.reflection.assemblyversionattribute.aspx
        "Version checking only occurs with strong-named assemblies."
        *)
        | _ when this.version <> (0us, 0us, 0us, 0us) && this.version <> candidate.version -> false
        | _ when this.culture <> null && this.culture <> candidate.culture -> false
        | _ when not(PublicKeyOrToken.Matches this.publicKeyOrToken candidate.publicKeyOrToken) -> false
        | _ -> true

(*
TODO: review assembly resolution algorithm
- context 1 ("load context"): GAC or private assembly store. (Assembly.Load())
- context 2 ("load-from context"): user-provided path or URL. (Assembly.LoadFrom(), ExecuteAssembly(), ...)
- context 3 ("reflection-only context"): no execution. (Assembly.ReflectionOnlyLoad(), ReflectionOnlyLoadFrom())
- no context: user-provided byte array.
*)
type AssemblyResolver(sysLibPath, dirs, gac1Dirs, gac2Dirs) as this =

    let resolve (asmName : AssemblyName) =
        let checkDir dir =
            let path = Path.Combine(dir, asmName.name + ".dll")
            if File.Exists(path) then
                let asm = Assembly(this, path)
                if asmName.Matches(asm.Name) then
                    Some asm
                else
                    None
            else
                None

        seq {
            yield List.tryPick checkDir dirs

            match asmName.publicKeyOrToken with
            | None -> raise(NotImplementedException())
            | Some keyOrToken ->
                let maj, min, rev, bld = asmName.version
                let token = toHexString keyOrToken.Token

                for gacDir in gac1Dirs do
                    let gacSubDir = Path.Combine(gacDir, asmName.name)
                    let dir = Path.Combine(gacSubDir, sprintf "%d.%d.%d.%d__%s" maj min rev bld token)
                    yield checkDir dir

                for gacDir in gac2Dirs do 
                    let gacSubDir = Path.Combine(gacDir, asmName.name)
                    let dir = Path.Combine(gacSubDir, sprintf "v4.0_%d.%d.%d.%d__%s" maj min rev bld token)
                    yield checkDir dir

            failwithf "Could not resolve assembly %A." asmName
        }
        |> Seq.pick id

    let sysLib = Assembly(this, sysLibPath)

    let mutable loadedAsms = [ sysLib ]

    member this.SystemLibrary = sysLib

    member this.LoadedAssemblies = loadedAsms

    member this.Load(asmName : AssemblyName) =
        match loadedAsms |> List.tryFind (fun asm -> asmName.Matches(asm.Name)) with
        | Some asm -> asm
        | None ->
            let asm = resolve asmName
            loadedAsms <- asm :: loadedAsms
            asm

    member this.Load(asmRef) =
        this.Load(AssemblyName.FromAssemblyRef(asmRef))

    member this.LoadFrom(path) =
        match loadedAsms |> List.tryFind (fun asm -> asm.ManifestModule.Path = path) with
        | Some asm -> asm
        | None ->
            let asm' = Assembly(this, path)
            let h = asm'.ManifestModule.Hash
            match loadedAsms |> List.tryFind (fun asm -> asm.ManifestModule.Hash = h) with
            | Some asm -> asm
            | None ->
                loadedAsms <- asm' :: loadedAsms
                asm'

and Assembly internal (asmResolver : AssemblyResolver, path) as this =
    do printfn "%s" path
    let manifestModule = Module(this, path)
    let manifest =
        match manifestModule.AssemblyManifest with
        | Some asm -> asm
        | None -> failwith "Manifest missing."
    let moduleMap =
        let dir = Path.GetDirectoryName(path)
        seq {
            for name in manifestModule.ModuleRefs do
                let path = Path.Combine(dir, name)
                if File.Exists(path) then
                    let pe = PEImageReader(path)
                    if Option.isSome pe.CliHeader then
                        yield name, Module(this, path)
        }
        |> Map.ofSeq

    interface IComparable with
        member this.CompareTo(other) =
            if obj.ReferenceEquals(this, other) then
                0
            else
                compare this.ManifestModule.Hash (other :?> Assembly).ManifestModule.Hash

    override this.GetHashCode() =
        hash this.ManifestModule.Hash

    override this.Equals(other) =
        obj.ReferenceEquals(this, other)

    member this.AssemblyResolver = asmResolver
    member this.ManifestModule : Module = manifestModule
    member this.Name = AssemblyName.FromAssemblyManifest(manifest)
    member this.Modules =
        seq {
            yield manifestModule
            for _, m in Map.toSeq moduleMap -> m
        }

    member this.GetModule(name) = moduleMap.[name]

and Module internal (asm, path) =
    let m = ModuleLoading.loadModule path
    let imageHash = Roma.Cli.PEImageReader(path).StrongNameHash
    let typeMap =
        seq {
            for typeDef in m.typeDefs ->
                (typeDef.typeNamespace, typeDef.typeName), typeDef
        }
        |> Map.ofSeq
    let exportedTypeMap =
        seq {
            for et in m.exportedTypes ->
                (et.typeNamespace, et.typeName), et
        }
        |> Map.ofSeq

    interface IComparable with
        member this.CompareTo(other) =
            if obj.ReferenceEquals(this, other) then
                0
            else
                let otherMod = other :?> Module
                match compare this.Assembly otherMod.Assembly with
                | 0 -> compare this.Path otherMod.Path
                | x -> x

    override this.GetHashCode() =
        hash [ asm.GetHashCode(); path.GetHashCode() ]

    override this.Equals(other) =
        obj.ReferenceEquals(this, other)

    member this.Assembly = asm
    member this.ScopeName = m.moduleName
    member this.ModuleRefs = m.moduleRefs
    member this.Path = path
    member this.Hash = imageHash
    member this.AssemblyManifest = m.assembly
    member this.TypeMap = typeMap
    member this.ExportedTypeMap = exportedTypeMap
    member this.GlobalMethods = m.methods
    member this.GlobalFields = m.fields

and Type =
    | VoidType
    | PrimitiveType of Roma.Compiler.PrimitiveTypeKind
    | EnumType of Roma.Compiler.PrimitiveTypeKind * TypeRef
    | CompositeType of CompositeTypeInfo
    | ArrayType of ArrayTypeInfo
    | GCRefType of Type
    | ByRefType of Type
    | PointerType of Type
    | VolatileModifier of Type
    | PinnedModifier of Type
    // TODO: pointer-to-function
    // TODO: pinned, custom modifiers

and CompositeTypeInfo =
    {
        typeRef : TypeRef
        genArgs : Type list
    }

and ArrayTypeInfo =
    {
        elemType : Type
        shape : (int * int option) list
    }

and TypeRef =
    | TopLevelTypeRef of Module * string * string // owner module, type namespace, type name
    | NestedTypeRef of TypeRef * string // enclosing type, type name
