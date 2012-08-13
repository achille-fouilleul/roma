namespace Roma.Cli

type IModuleLoader =
    abstract member GetTypeRef : Token -> TypeSpec
    abstract member GetLocalVarSig : Token option -> TypeSig list

