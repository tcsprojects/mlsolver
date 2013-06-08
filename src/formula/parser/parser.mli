type token =
  | Ttt
  | Tff
  | TImplies
  | TInvImplies
  | TIff
  | TXor
  | TFormulaIdent of (string)
  | TArithmVar of (string)
  | TArithmIndex
  | TBeginDef
  | TEndDef
  | TDelim
  | TMatchWith
  | TMatchOption
  | TQuestionMark
  | TCondAnd
  | TCondOr
  | TCondNot
  | TCondEq
  | TCondUneq
  | TCondGreaterEq
  | TCondLessEq
  | TMu
  | TNu
  | TNext
  | TFinally
  | TGenerally
  | TProp of (string)
  | TVar of (string)
  | TBracketOpen
  | TBracketClose
  | TNeg
  | TAnd
  | TOr
  | TDiamondOpen
  | TStar
  | TDiamondClose
  | TBoxOpen
  | TBoxClose
  | TPoint
  | TArithmConst of (int)
  | TArithmSum
  | TArithmNeg
  | TArithmProd
  | TArithmDiv
  | TArithmMod
  | TEOL

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Metaformula.environment
