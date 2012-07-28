namespace Roma.Cli

open System

[<Flags>]
type AssemblyFlags =
    | PublicKey = 0x0001u
    | Retargetable = 0x0100u
    | DisableJITcompileOptimizer = 0x4000u
    | EnableJITcompileTracking = 0x8000u

[<Flags>]
type TypeAttributes =
    // visibility attributes
    | VisibilityMask = 0x7u
    | NotPublic = 0x0u
    | Public = 0x1u
    | NestedPublic = 0x2u
    | NestedPrivate = 0x3u
    | NestedFamily = 0x4u
    | NestedAssembly = 0x5u
    | NestedFamANDAssem = 0x6u
    | NestedFamORAssem = 0x7u

    // class layout attributes
    | LayoutMask = 0x18u
    | AutoLayout = 0x0u
    | SequentialLayout = 0x8u
    | ExplicitLayout = 0x10u

    // class semantics attributes
    | ClassSemanticsMask = 0x20u
    | Class = 0x0u
    | Interface = 0x20u

    // special semantics in addition to class semantics
    | Abstract = 0x80u
    | Sealed = 0x100u
    | SpecialName = 0x400u

    // implementation attributes
    | Import = 0x1000u
    | Serializable = 0x2000u

    // string formatting attributes
    | StringFormatMask = 0x30000u
    | AnsiClass = 0x0u
    | UnicodeClass = 0x10000u
    | AutoClass = 0x20000u
    | CustomFormatClass = 0x30000u
    | CustomStringFormatClass = 0xc0000u

    // class initialization attributes
    | BeforeFieldInit = 0x100000u

    // additional flags
    | RTSpecialName = 0x800u
    | HasSecurity = 0x40000u
    | IsTypeForwarder = 0x200000u

[<Flags>]
type FieldAttributes =
    | FieldAccessMask = 0x7us
    | CompilerControlled = 0x0us
    | Private = 0x1us
    | FamANDAssem = 0x2us
    | Assembly = 0x3us
    | Family = 0x4us
    | FamORAssem = 0x5us
    | Public = 0x6us
    | Static = 0x10us
    | InitOnly = 0x20us

    | Literal = 0x40us
    | NotSerialized = 0x80us
    | SpecialName = 0x200us

    // interop attributes
    | PInvokeImpl = 0x2000us

    // additional flags
    | RTSpecialName = 0x400us
    | HasFieldMarshal = 0x1000us
    | HasDefault = 0x8000us
    | HasFieldRVA = 0x100us

[<Flags>]
type MethodImplAttributes =
    | CodeTypeMask = 0x3us
    | IL = 0x0us
    | Native = 0x1us
    | OPTIL = 0x2us
    | Runtime = 0x3us

    | ManagedMask = 0x4us
    | Unmanaged = 0x4us
    | Managed = 0x0us

    // implementation info and interop
    | ForwardRef = 0x10us
    | PreserveSig = 0x80us
    | InternalCall = 0x1000us
    | Synchronized = 0x20us
    | NoInlining = 0x8us
    | NoOptimization = 0x40us

[<Flags>]
type MethodAttributes =
    | MemberAccessMask = 0x7us
    | CompilerControlled = 0x0us
    | Private = 0x1us
    | FamANDAssem = 0x2us
    | Assem = 0x3us
    | Family = 0x4us
    | FamORAssem = 0x5us
    | Public = 0x6us

    | Static = 0x10us
    | Final = 0x20us
    | Virtual = 0x40us
    | HideBySig = 0x80us

    | VtableLayoutMask = 0x100us
    | ReuseSlot = 0x0us
    | NewSlot = 0x100us

    | Strict = 0x200us
    | Abstract = 0x400us
    | SpecialName = 0x800us

    // interop attributes
    | PInvokeImpl = 0x2000us
    | UnmanagedExport = 0x8us

    // additional flags
    | RTSpecialName = 0x1000us
    | HasSecurity = 0x4000us
    | RequireSecObject = 0x8000us

[<Flags>]
type ParamAttributes =
    | In = 0x1us
    | Out = 0x2us
    | Optional = 0x10us
    | HasDefault = 0x1000us
    | HasFieldMarshal = 0x2000us

[<Flags>]
type EventAttributes =
    | SpecialName = 0x200us
    | RTSpecialName = 0x400us

[<Flags>]
type PropertyAttributes =
    | SpecialName = 0x200us
    | RTSpecialName = 0x400us
    | HasDefault = 0x1000us

[<Flags>]
type MethodSemanticsAttribute =
    | Setter = 0x1us
    | Getter = 0x2us
    | Other = 0x4us
    | AddOn = 0x8us
    | RemoveOn = 0x10us
    | Fire = 0x20us

[<Flags>]
type PInvokeAttributes =
    | NoMangle = 0x1us

    // character set
    | CharSetMask = 0x6us
    | CharSetNotSpec = 0x0us
    | CharSetAnsi = 0x2us
    | CharSetUnicode = 0x4us
    | CharSetAuto = 0x6us
    | SupportsLastError = 0x40us

    // calling convention
    | CallConvMask = 0x700us
    | CallConvPlatformapi = 0x100us
    | CallConvCdecl = 0x200us
    | CallConvStdcall = 0x300us
    | CallConvThiscall = 0x400us
    | CallConvFastcall = 0x500us

type AssemblyHashAlgorithm =
    | None = 0x0u
    | MD5 = 0x8003u
    | SHA1 = 0x8004u

[<Flags>]
type FileAttributes =
    | ContainsMetaData = 0x0u
    | ContainsNoMetaData = 0x1u

[<Flags>]
type ManifestResourceAttributes =
    | VisibilityMask = 0x7u
    | Public = 0x1u
    | Private = 0x2u

[<Flags>]
type GenericParamAttributes =
    | VarianceMask = 0x3us
    | None = 0x0us
    | Covariant = 0x1us
    | Contravariant = 0x2us

    | SpecialConstraintMask = 0x1cus
    | ReferenceTypeConstraint = 0x4us
    | NotNullableValueTypeConstraint = 0x8us
    | DefaultConstructorConstraint = 0x10us
