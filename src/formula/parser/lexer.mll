{
  open Parser;;
  exception Eof
}

rule lexer = parse
    [' ' '\t' '\n' '\r'] {lexer lexbuf}
  | "//"[^'\n']* {lexer lexbuf}
  | "/*"((['*'][^'/'])|[^'*'])*"*/" {lexer lexbuf}
  | "tt" {Ttt }
  | "ff" {Tff }
  | "==>" {TImplies }
  | "<==" {TInvImplies }
  | "<==>" {TIff }
  | "<~~>" {TXor }
  | "#"['A'-'Z' 'a'-'z' '0'-'9']+ {TFormulaIdent (Lexing.lexeme lexbuf)}
  | "$"['A'-'Z' 'a'-'z' '0'-'9']+ {TArithmVar (Lexing.lexeme lexbuf)}
  | '_' {TArithmIndex }
  | ":=" {TBeginDef }
  | [';'] {TEndDef }
  | ['?'] {TQuestionMark }
  | "^*" {TStar }
  | [','] {TDelim }
  | "->" {TMatchWith }
  | ['\\'] {TMatchOption }
  | ['&']['&'] {TCondAnd }
  | ['|']['|'] {TCondOr }
  | ['!']['!'] {TCondNot }
  | "=" {TCondEq }
  | "!=" {TCondUneq }
  | ">=" {TCondGreaterEq }
  | "<=" {TCondLessEq }
  | "mu" {TMu }
  | "nu" {TNu }
  | ("()") {TNext }
  | ("<>") {TFinally }
  | ("[]") {TGenerally }
  | ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* {TProp (Lexing.lexeme lexbuf)}
  | ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9']* {TVar (Lexing.lexeme lexbuf)}
  | ['('] {TBracketOpen }
  | [')'] {TBracketClose }
  | ['!'] {TNeg }
  | ['~'] {TNeg }
  | ['&'] {TAnd }
  | ['|'] {TOr }
  | ['<'] {TDiamondOpen }
  | ['>'] {TDiamondClose }
  | ['['] {TBoxOpen }
  | [']'] {TBoxClose }
  | ['.'] {TPoint }
  | ['0'-'9']+ {TArithmConst (int_of_string(Lexing.lexeme lexbuf))}
  | ['+'] {TArithmSum}
  | ['-'] {TArithmNeg}
  | ['*'] {TArithmProd}
  | ['/'] {TArithmDiv}
  | ['%'] {TArithmMod}
  | eof {TEOL}