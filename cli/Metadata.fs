namespace Roma.Cli

type ExceptionClauseRanges = {
    tryOffset : int
    tryLength : int
    handlerOffset : int
    handlerLength : int
}

type ExceptionClause =
    | ExceptionCatch of (ExceptionClauseRanges * Token (* TODO: TypeRef *))
    | ExceptionFilter of (ExceptionClauseRanges * int)
    | ExceptionFinally of ExceptionClauseRanges
    | ExceptionFault of ExceptionClauseRanges

type MethodBody = {
    maxStack : int
    locals :  byte[] // TODO: TypeSig[]
    initLocals : bool
    excClauses : ExceptionClause list
    instrs : byte [] // TODO: (int * Instruction) list
}
