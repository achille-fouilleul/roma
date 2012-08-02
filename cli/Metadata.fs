namespace Roma.Cli

type ExceptionClauseRanges = {
    tryOffset : int
    tryLength : int
    handlerOffset : int
    handlerLength : int
}

type ExceptionClause =
    | Catch of ExceptionClauseRanges * Token // TODO: TypeRef
    | Filter of ExceptionClauseRanges * int
    | Finally of ExceptionClauseRanges
    | Fault of ExceptionClauseRanges

type MethodBody = {
    maxStack : int
    localVarSigToken : Token option // TODO: StandAloneSig
    initLocals : bool
    excClauses : ExceptionClause list
    instrs : byte[] // TODO: (int * Instruction) list
}
