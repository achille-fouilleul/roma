namespace Roma.Cli

type Version = uint16 * uint16 * uint16 * uint16

type CallKind =
    | Default
    | Cdecl
    | Stdcall
    | Thiscall
    | Fastcall
    | Vararg

type CallConv = {
    hasThis : bool
    explicitThis : bool
    callKind : CallKind
}

type TypeResolutionScope =
    | TypeRefScope of TypeSpec
    | ModuleRefScope of string
    | AssemblyRefScope of AssemblyRefRow

and TypeRef = {
    scope : TypeResolutionScope option
    typeNamespace : string
    typeName : string
}

and TypeSig =
    | Boolean
    | Char
    | I1
    | U1
    | I2
    | U2
    | I4
    | U4
    | I8
    | U8
    | R4
    | R8
    | I
    | U
    | Array of TypeSig * ((int * int option) list)
    | ByRef of TypeSig
    | Fnptr of MethodSig
    | GenericInst of TypeSig * (TypeSig list)
    | MVar of int
    | Object
    | Ptr of TypeSig
    | String
    | SZArray of TypeSig
    | TypedByRef
    | Var of int
    | Void
    | ModReq of TypeRef * TypeSig
    | ModOpt of TypeRef * TypeSig
    | Pinned of TypeSig
    | Class of TypeRef
    | ValueType of TypeRef

and TypeSpec = Choice<TypeRef, TypeSig>

and MethodSig = {
    callConv : CallConv
    retType : TypeSig
    paramTypes : TypeSig[]
    varargParamTypes : TypeSig[] option
    genParamCount : int
}

type MethodRef = {
    typeRef : TypeSpec option
    methodName : string
    signature : MethodSig
}

type MethodSpec = {
    methodRef : MethodRef
    args : TypeSig list
}

type FieldRef = {
    typeRef : TypeSpec option
    fieldName : string
    signature : TypeSig
}

type MemberRef = Choice<MethodRef, FieldRef>

type Constant =
    | ConstantBool of bool
    | ConstantBytearray of byte[]
    | ConstantChar of uint16
    | ConstantR4 of uint32
    | ConstantR8 of uint64
    | ConstantI1 of int8
    | ConstantU1 of uint8
    | ConstantI2 of int16
    | ConstantU2 of uint16
    | ConstantI4 of int32
    | ConstantU4 of uint32
    | ConstantI8 of int64
    | ConstantU8 of uint64
    | ConstantString of string
    | ConstantNullRef

type Marshal =
    | NativeBool
    | NativeI1
    | NativeU1
    | NativeI2
    | NativeU2
    | NativeI4
    | NativeU4
    | NativeI8
    | NativeU8
    | NativeR4
    | NativeR8
    | NativeBstr // non-standard
    | NativeLpstr
    | NativeLpwstr
    | NativeLptstr // non-standard
    | NativeFixedSysString of int // non-standard
    | NativeIUnknown // non-standard
    | NativeIDispatch // non-standard
    | NativeStruct // non-standard
    | NativeInterface // non-standard
    | NativeSafeArray // non-standard
    | NativeFixedArray of int // non-standard
    | NativeI
    | NativeU
    | NativeByvalstr // non-standard
    | NativeVariantBool // non-standard
    | NativeFunc
    | NativeAsAny // non-standard
    | NativeArray of Marshal option * int option * int option // element type, param index, size
    | NativeLpstruct // non-standard
    | NativeCustom of string * string // non-standard
    | NativeError // non-standard
    | NativeHString // non-standard
