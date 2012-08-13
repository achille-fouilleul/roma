namespace Roma.Cli

type CustomAttribute = {
    methodRef : MethodRef
    value : byte[]
}

type Implementation =
    | Implementation_File of string
    | Implementation_AssemblyRef of AssemblyRefRow

type ManifestResource = {
    name : string
    flags : ManifestResourceAttributes
    offset : uint32
    implementation : Implementation option
    customAttrs : CustomAttribute list
}

type DeclSecurity = {
    action : uint16
    permissionSet : byte[]
}

type Assembly = {
    hashAlgId : AssemblyHashAlgorithm
    version : Version
    flags : AssemblyFlags
    publicKey : byte[]
    name : string
    culture : string
    declSec : DeclSecurity list
    customAttrs : CustomAttribute list
}

type AssemblyRef = {
    version : Version
    flags : AssemblyFlags
    publicKeyOrToken : byte[]
    name : string
    culture : string
    hashValue : byte[]
    customAttrs : CustomAttribute list
}

type ParamDef = {
    flags : ParamAttributes
    name : string
    marshal : Marshal option
    constant : Constant option
    customAttrs : CustomAttribute list
}

type GenericParam = {
    name : string
    flags : GenericParamAttributes
    constraints : TypeSpec list
    customAttrs : CustomAttribute list
}

type ExceptionClauseRanges = {
    tryOffset : int
    tryLength : int
    handlerOffset : int
    handlerLength : int
}

type ExceptionClause =
    | Catch of ExceptionClauseRanges * TypeSpec
    | Filter of ExceptionClauseRanges * int
    | Finally of ExceptionClauseRanges
    | Fault of ExceptionClauseRanges

type MethodBody = {
    maxStack : int
    locals : TypeSig list
    initLocals : bool
    excClauses : ExceptionClause list
    instrs : byte[] // TODO: (int * Instruction) list
}

type MethodDef = {
    name : string
    implFlags : MethodImplAttributes
    flags : MethodAttributes
    genericParams : GenericParam[]
    isEntryPoint : bool
    signature : MethodSig
    pinvokeimpl : (PInvokeAttributes * string * string) option
    retVal : ParamDef option
    parameters : ParamDef option array
    body : MethodBody option
    declSec : DeclSecurity list
    customAttrs : CustomAttribute list
}

type FieldDef = {
    name : string
    flags : FieldAttributes
    typeSig : TypeSig
    offset : uint32 option
    marshal : Marshal option
    rva : uint32 option
    constant : Constant option
    customAttrs : CustomAttribute list
}

type Property = {
    flags : PropertyAttributes
    name : string
    hasThis : bool
    retType : TypeSig
    paramTypes : TypeSig[]
    methods : (MethodSemanticsAttribute * MethodRef) list
    customAttrs : CustomAttribute list
}

type Event = {
    flags : EventAttributes
    name : string
    typeRef : TypeSpec option
    methods : (MethodSemanticsAttribute * MethodRef) list
    customAttrs : CustomAttribute list
}

type TypeDef = {
    typeName : string
    typeNamespace : string
    flags : TypeAttributes
    packingSize : int option
    classSize : int option
    genericParams : GenericParam[]
    baseType : TypeSpec option
    interfaces : TypeSpec list
    nestedTypes : TypeDef list
    methods : MethodDef list
    fields : FieldDef list
    overrides : (MethodRef * MethodRef) list
    properties : Property list
    events : Event list
    declSec : DeclSecurity list
    customAttrs : CustomAttribute list
}

type FileRef = {
    name : string
    isEntryPoint : bool
    containsMetadata : bool
    hash : byte[]
    customAttrs : CustomAttribute list
}

type Module = {
    moduleName : string
    moduleGuid : Guid
    moduleRefs : string list
    assembly : Assembly option
    mresources : ManifestResource list
    assemblyRefs : AssemblyRef list
    fileRefs : FileRef list
    methods : MethodDef list
    fields : FieldDef list
    typeDefs : TypeDef list
    customAttrs : CustomAttribute list
    // TODO: exported types
}
