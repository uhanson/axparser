module Ax where
    import Text.ParserCombinators.Parsec
    import Text.ParserCombinators.Parsec.Token as P
    import Text.ParserCombinators.Parsec.Language (emptyDef)

    xppStyle = emptyDef {
                   commentStart    = "/*",
                   commentEnd      = "*/",
                   commentLine     = "//",
                   nestedComments  = False,
                   identLetter     = alphaNum <|> char '_',
                   caseSensitive   = False,
                   reservedNames   = ["abstract", "anytype", "as", "asc", "at", "avg", "break", "breakpoint", "by", "byref", "case", "catch",
                                      "changeCompany", "class", "client", "container", "continue", "count", "crossCompany", "date", "default",
                                      "delegate", "delete_from", "desc", "display", "div", "do", "edit", "else", "eventHandler", "exists",
                                      "extends", "false", "final", "firstFast", "firstOnly", "firstOnly10", "firstOnly100", "firstOnly1000",
                                      "flush", "for", "forceLiterals", "forceNestedLoop", "forcePlaceholders", "forceSelectOrder", "forUpdate",
                                      "from", "group", "if", "implements", "insert_recordset", "int", "int64", "interface", "is", "join", "like",
                                      "maxof", "minof", "mod", "next", "new", "noFetch", "notExists", "null", "optimisticLock", "order", "outer", "pause",
                                      "pessimisticLock", "print", "private", "protected", "public", "real", "repeatableRead", "retry", "return",
                                      "reverse", "select", "server", "setting", "static", "str", "sum", "super", "switch", "tableLock", "this",
                                      "throw", "true", "try", "ttsAbort", "ttsBegin", "ttsCommit", "update_recordset", "validTimeState", "void",
                                      "where", "while", "window"],
                   reservedOpNames = ["!", "!=", "#", "&", "&&", "(", ")", "*", "^", "|", "||", "~", "+", "++", "+=", ",", "-", "--", "-=", ".",
                                      "/", "\\", "@", ":", "::", ";", "<", "<<", "<=", "=", "==", ">", ">=", ">>", "?", "[", "]", "{", "}"]
               }

    xpp = makeTokenParser xppStyle

    whiteSpace = P.whiteSpace xpp
    lexeme     = P.lexeme xpp
    symbol     = P.symbol xpp
    natural    = P.natural xpp
    parens     = P.parens xpp
    semi       = P.semi xpp
    identifier = P.identifier xpp
    reserved   = P.reserved xpp
    reservedOp = P.reservedOp xpp