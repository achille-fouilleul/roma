namespace Roma.Cli

type IModuleLoader =
    abstract member GetLocalVarSig : Token -> TypeSig list
    abstract member GetUserString : uint32 -> string
    abstract member GetTypeRef : Token -> TypeSpec
    abstract member GetMemberRef : Token -> TypeSpec option * string * byte[]
    abstract member GetMethodSpec : Token -> MethodSpec
    abstract member GetMethodSig : Token -> MethodSig
    abstract member GetFieldRef : Token -> FieldRef