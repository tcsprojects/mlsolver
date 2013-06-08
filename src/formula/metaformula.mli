type arithm_expr =
    AConst of int
  | ASum of arithm_expr * arithm_expr
  | ANeg of arithm_expr
  | AProd of arithm_expr * arithm_expr
  | ADiv of arithm_expr * arithm_expr
  | AMod of arithm_expr * arithm_expr
  | AArithmVar of string
type indentifier = IDefault of string | IIndexed of string * arithm_expr
type formal_param = PFormulaVar of string | PArithmVar of string
type actual_param = PFormula of formula_expr | PArithmExpr of arithm_expr
and formula_expr =
    FProp of indentifier
  | FVariable of indentifier
  | Ftt
  | Fff
  | FNeg of formula_expr
  | FOr of formula_expr * formula_expr
  | FAnd of formula_expr * formula_expr
  | FQuantor of string * indentifier * formula_expr
  | FUnaryOp of string * formula_expr
  | FBinaryOp of string * formula_expr * formula_expr
  | FLabelledOp of string * formula_expr * formula_expr
  | FFormulaIdent of string * actual_param list
type condition =
    CondTT
  | CondFF
  | CondNot of condition
  | CondAnd of condition * condition
  | CondOr of condition * condition
  | CondEq of arithm_expr * arithm_expr
  | CondGreater of arithm_expr * arithm_expr
type cond_expression = condition * formula_expr
type formula_def = string * formal_param list * cond_expression list
type environment = formula_def list
(*
val eval_arithm_expr : arithm_expr -> int
val eval_condition : condition -> bool
val select_cond_expression : (condition * 'a) list -> 'a
val apply_arithm_subst :
  arithm_expr -> (formal_param * actual_param) list -> arithm_expr
val apply_arithm_identifier_subst :
  indentifier -> (formal_param * actual_param) list -> indentifier
val apply_condition_subst :
  condition -> (formal_param * actual_param) list -> condition
val apply_formula_subst :
  formula_expr -> (formal_param * actual_param) list -> formula_expr
val apply_cond_expression_subst :
  (condition * formula_expr) list ->
  (formal_param * actual_param) list -> (condition * formula_expr) list
val resolve_formula :
  string ->
  actual_param list ->
  (string * formal_param list * 'a) list ->
  (string * formal_param list * 'a) list *
  (formal_param * actual_param) list * 'a
val eval_identifier : indentifier -> indentifier
*)
val eval_formula :
  formula_expr ->
  (string * formal_param list * (condition * formula_expr) list) list ->
  formula_expr
val identifier_to_str : indentifier -> string
val as_identifier_to_str : formula_expr -> string
