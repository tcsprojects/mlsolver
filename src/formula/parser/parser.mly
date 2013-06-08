%{
open Metaformula;;

let parse_error s =
  print_endline "Parse error";
  print_endline s;
  flush stdout;;

%}

%token Ttt
%token Tff
%token TImplies
%token TInvImplies
%token TIff
%token TXor
%token <string> TFormulaIdent
%token <string> TArithmVar
%token TArithmIndex
%token TBeginDef
%token TEndDef
%token TDelim
%token TMatchWith
%token TMatchOption
%token TQuestionMark
%token TCondAnd
%token TCondOr
%token TCondNot
%token TCondEq
%token TCondUneq
%token TCondGreaterEq
%token TCondLessEq
%token TMu
%token TNu
%token TNext
%token TFinally
%token TGenerally
%token <string> TProp
%token <string> TVar
%token TBracketOpen
%token TBracketClose
%token TNeg
%token TAnd
%token TOr
%token TDiamondOpen
%token TStar
%token TDiamondClose
%token TBoxOpen
%token TBoxClose
%token TPoint
%token <int> TArithmConst
%token TArithmSum
%token TArithmNeg
%token TArithmProd
%token TArithmDiv
%token TArithmMod
%token TEOL

%right TPoint
%right TIff
%right TImplies TInvImplies
%right TOr TXor TEndDef
%right TAnd
%left TDiamondOpen TDiamondClose TBoxOpen TBoxClose
%left TNext TFinally TGenerally
%right TNeg TMu TNu
%right TBracketOpen
%left TBracketClose
%right TProp TVar

%left TQuestionMark TStar

%right TCondOr
%right TCondAnd
%right TCondNot
%right TArithmNeg
%right TArithmSum
%right TArithmProd
%right TArithmDiv
%right TArithmMod


%type <Metaformula.environment> program
%start program

%%
program :
  nprogram TEOL {$1}
;

nprogram :
  definition {[$1]}
| definition nprogram {$1::$2}
;

definition :
  TFormulaIdent TBeginDef expression TEndDef {($1, [], [(CondTT, $3)])}
| TFormulaIdent TBeginDef ncondexpression TEndDef {($1, [], $3)}
| TFormulaIdent TBracketOpen nformalparam TBracketClose TBeginDef expression TEndDef {($1, $3, [(CondTT, $6)])}
| TFormulaIdent TBracketOpen nformalparam TBracketClose TBeginDef ncondexpression TEndDef {($1, $3, $6)}
;

nformalparam :
  formalparam {[$1]}
| formalparam TDelim nformalparam {$1::$3}
;

formalparam :
  TFormulaIdent {PFormulaVar $1}
| TArithmVar {PArithmVar $1}
;

ncondexpression :
  TMatchOption condition TMatchWith expression {[($2, $4)]}
| TMatchOption condition TMatchWith expression ncondexpression {($2, $4) :: $5}

condition :
  TBracketOpen condition TBracketClose {$2}
| Ttt {CondTT}
| Tff {CondFF}
| condition TCondAnd condition {CondAnd($1, $3)}
| condition TCondOr condition {CondOr($1, $3)}
| TCondNot condition {CondNot $2}
| arithmexpr TCondEq arithmexpr {CondEq($1, $3)}
| arithmexpr TCondUneq arithmexpr {CondNot(CondEq($1, $3))}
| arithmexpr TDiamondClose arithmexpr {CondGreater($1, $3)}
| arithmexpr TDiamondOpen arithmexpr {CondGreater($3, $1)}
| arithmexpr TCondGreaterEq arithmexpr {CondOr(CondGreater($1, $3), CondEq($1, $3))}
| arithmexpr TCondLessEq arithmexpr {CondOr(CondGreater($3, $1), CondEq($3, $1))}
;

expression :
  TBracketOpen expression TBracketClose {$2}
| expression TAnd expression {FAnd ($1, $3)}
| expression TOr expression {FOr ($1, $3)}
| expression TImplies expression {FOr (FNeg ($1), $3)}
| expression TInvImplies expression {FOr ($1, FNeg ($3))}
| expression TIff expression {FOr(FAnd ($1, $3), FAnd (FNeg ($1), FNeg ($3)))}
| expression TXor expression {FOr(FAnd ($1, FNeg ($3)), FAnd (FNeg ($1), $3))}
| TNeg expression {FNeg ($2)}
| Ttt {Ftt}
| Tff {Fff}
| TDiamondOpen expression TDiamondClose expression {FLabelledOp ("<>", $2, $4)}
| TBoxOpen expression TBoxClose expression {FLabelledOp ("[]", $2, $4)}
| TNext expression {FUnaryOp ("()", $2)}
| TFinally expression {FUnaryOp ("<>", $2)}
| TGenerally expression {FUnaryOp ("[]", $2)}
| TProp expression {FUnaryOp ($1, $2)}
| TVar expression {FUnaryOp ($1, $2)}
| expression TQuestionMark {FUnaryOp ("?", $1)}
| expression TStar {FUnaryOp ("*", $1)}
| expression TPoint expression {FBinaryOp(".", $1, $3)}
| expression TProp expression {FBinaryOp($2, $1, $3)}
| expression TVar expression {FBinaryOp($2, $1, $3)}
| TMu TVar TPoint expression {FQuantor ("mu", IDefault $2, $4)}
| TMu TVar TArithmIndex arithmexpr TPoint expression {FQuantor ("mu", IIndexed($2, $4), $6)}
| TNu TVar TPoint expression {FQuantor ("nu", IDefault $2, $4)}
| TNu TVar TArithmIndex arithmexpr TPoint expression {FQuantor ("nu", IIndexed($2, $4), $6)}
| TProp TArithmIndex arithmexpr {FProp (IIndexed ($1, $3))}
| TProp {FProp (IDefault $1)}
| TVar TArithmIndex arithmexpr {FVariable (IIndexed ($1, $3))}
| TVar {FVariable (IDefault $1)}
| TFormulaIdent {FFormulaIdent ($1, [])}
| TFormulaIdent TBracketOpen nactualparam TBracketClose {FFormulaIdent ($1, $3)}
;

nactualparam :
  actualparam {[$1]}
| actualparam TDelim nactualparam {$1::$3}
;

actualparam :
  expression {PFormula $1}
| arithmexpr {PArithmExpr $1}
;

arithmexpr :
  TBracketOpen arithmexpr TBracketClose {$2}
| TArithmConst {AConst $1}
| arithmexpr TArithmNeg arithmexpr {ASum($1, ANeg $3)}
| TArithmNeg arithmexpr {ANeg $2}
| arithmexpr TArithmSum arithmexpr {ASum($1, $3)}
| arithmexpr TArithmProd arithmexpr {AProd($1, $3)}
| arithmexpr TArithmDiv arithmexpr {ADiv($1, $3)}
| arithmexpr TArithmMod arithmexpr {AMod($1, $3)}
| TArithmVar {AArithmVar $1}
;

%%