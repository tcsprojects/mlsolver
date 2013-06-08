type arithm_expr = AConst of int
                | ASum of arithm_expr * arithm_expr
                | ANeg of arithm_expr
                | AProd of arithm_expr * arithm_expr
                | ADiv of arithm_expr * arithm_expr
                | AMod of arithm_expr * arithm_expr
                | AArithmVar of string;;
                
type indentifier = IDefault of string
                 | IIndexed of string * arithm_expr;;

type formal_param = PFormulaVar of string
                 | PArithmVar of string;;

type actual_param = PFormula of formula_expr
                 | PArithmExpr of arithm_expr

and formula_expr = FProp of indentifier
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
                | FFormulaIdent of string * (actual_param list);;

type condition = CondTT
               | CondFF
               | CondNot of condition
               | CondAnd of condition * condition
               | CondOr of condition * condition
               | CondEq of arithm_expr * arithm_expr
               | CondGreater of arithm_expr * arithm_expr;;

type cond_expression = condition * formula_expr;;

type formula_def = string * (formal_param list) * (cond_expression list);;

type environment = formula_def list;;

let rec eval_arithm_expr expr =
  match expr with
    AConst i     -> i
  | ASum (l, r)  -> (eval_arithm_expr l) + (eval_arithm_expr r)
  | ANeg e       -> -(eval_arithm_expr e)
  | AProd (l, r) -> (eval_arithm_expr l) * (eval_arithm_expr r)
  | ADiv (l, r)  -> (eval_arithm_expr l) / (eval_arithm_expr r)
  | AMod (l, r)  -> (eval_arithm_expr l) mod (eval_arithm_expr r)
  | AArithmVar v -> failwith ("Unbound arithmetic variable: " ^ v);;

let rec eval_condition cond =
  match cond with
    CondTT -> true
  | CondFF -> false
  | CondNot f -> not (eval_condition f)
  | CondAnd (f1, f2) -> (eval_condition f1) && (eval_condition f2)
  | CondOr (f1, f2) -> (eval_condition f1) || (eval_condition f2)
  | CondEq (a1, a2) -> (eval_arithm_expr a1) = (eval_arithm_expr a2)
  | CondGreater (a1, a2) -> (eval_arithm_expr a1) > (eval_arithm_expr a2);;

let rec select_cond_expression cond_expression =
  match cond_expression with
    [] -> failwith("No condition applies.")
  | (cond, expr)::rest -> if (eval_condition cond)
                          then expr
                          else select_cond_expression rest;;

let rec apply_arithm_subst f subst =
  match f with
    AConst i     -> AConst i
  | ASum (l, r)  -> ASum(apply_arithm_subst l subst, apply_arithm_subst r subst)
  | ANeg e       -> ANeg(apply_arithm_subst e subst)
  | AProd (l, r) -> AProd(apply_arithm_subst l subst, apply_arithm_subst r subst)
  | ADiv (l, r)  -> ADiv(apply_arithm_subst l subst, apply_arithm_subst r subst)
  | AMod (l, r)  -> AMod(apply_arithm_subst l subst, apply_arithm_subst r subst)
  | AArithmVar v -> if List.mem_assoc (PArithmVar v) subst
                    then match (List.assoc (PArithmVar v) subst) with
                           PArithmExpr e -> e
                         | _ -> failwith ("Corrupt substitution list")
                    else AArithmVar v;;

let apply_arithm_identifier_subst f subst =
	match f with
		IDefault s -> IDefault s
	|   IIndexed (s, a) -> IIndexed (s, apply_arithm_subst a subst);;

let rec apply_condition_subst f subst =
  match f with
  | CondNot f' -> CondNot (apply_condition_subst f' subst)
  | CondAnd (f1, f2) -> CondAnd (apply_condition_subst f1 subst, apply_condition_subst f2 subst)
  | CondOr (f1, f2) -> CondOr (apply_condition_subst f1 subst, apply_condition_subst f2 subst)
  | CondEq (a1, a2) -> CondEq (apply_arithm_subst a1 subst, apply_arithm_subst a2 subst)
  | CondGreater (a1, a2) -> CondGreater (apply_arithm_subst a1 subst, apply_arithm_subst a2 subst)
  | _ -> f;;

let rec apply_formula_subst f subst =
  let applyParamSubst p =
    match p with
      PFormula f -> PFormula (apply_formula_subst f subst)
    | PArithmExpr f -> PArithmExpr (apply_arithm_subst f subst)
  in
  match f with
    FNeg f' -> FNeg (apply_formula_subst f' subst)
  | FOr (f1, f2) -> FOr (apply_formula_subst f1 subst, apply_formula_subst f2 subst)
  | FAnd (f1, f2) -> FAnd (apply_formula_subst f1 subst, apply_formula_subst f2 subst)
  | FQuantor (k, x, f') -> FQuantor (k, apply_arithm_identifier_subst x subst, apply_formula_subst f' subst)
  | FUnaryOp (o, f') -> FUnaryOp (o, apply_formula_subst f' subst)
  | FBinaryOp (o, f1, f2) -> FBinaryOp (o, apply_formula_subst f1 subst, apply_formula_subst f2 subst)
  | FLabelledOp (o, l, f') -> FLabelledOp (o, apply_formula_subst l subst, apply_formula_subst f' subst)
  | FProp s -> FProp (apply_arithm_identifier_subst s subst)
  | FVariable s -> FVariable (apply_arithm_identifier_subst s subst)
  | FFormulaIdent (ident, params) ->
      if (params != [])
      then FFormulaIdent (ident, List.map applyParamSubst params)
      else if List.mem_assoc (PFormulaVar ident) subst
           then match (List.assoc (PFormulaVar ident) subst) with
                  PFormula e -> e
                | _ -> failwith ("Corrupt substitution list")
           else FFormulaIdent (ident, [])
  | _ -> f;;

let apply_cond_expression_subst f subst =
  List.map (fun (cond, expr) -> (apply_condition_subst cond subst, apply_formula_subst expr subst)) f;;

let resolve_formula ident params env =
  let rec params_match p =
    match p with
      [] -> true
    | (PFormulaVar v, PFormula e)::rest -> params_match rest
    | (PArithmVar v, PArithmExpr e)::rest -> params_match rest
    | _ -> false
  in
  let rec resolve env =
    match env with
      [] -> failwith ("Identifier '" ^ ident ^ "' is undefined.")
    | (ident', params', expr)::rest ->
        if (ident' = ident) &&
           (List.length params = List.length params') &&
           (params_match (List.combine params' params))
        then (List.rev ((ident', params', expr)::rest), List.combine params' params, expr)
        else resolve rest
  in
    resolve (List.rev env);;
    
let eval_identifier = function
	IDefault s -> IDefault s
|	IIndexed (s, a) -> IDefault (s ^ "_" ^ (string_of_int (eval_arithm_expr a)));;

let rec eval_formula fexpr env =
  let rec eval_param param env =
    match param with
      PFormula f -> PFormula (eval_formula f env)
    | PArithmExpr e -> PArithmExpr (AConst (eval_arithm_expr e))
  in
  match fexpr with
    FProp s -> FProp (eval_identifier s)
  | FVariable s -> FVariable (eval_identifier s)
  | FNeg f -> FNeg (eval_formula f env)
  | FOr (f1, f2) -> FOr (eval_formula f1 env, eval_formula f2 env)
  | FAnd (f1, f2) -> FAnd (eval_formula f1 env, eval_formula f2 env)
  | FQuantor (x, y, f) -> FQuantor (x, eval_identifier y, eval_formula f env)
  | FUnaryOp (x, f) -> FUnaryOp (x, eval_formula f env)
  | FBinaryOp (x, f1, f2) -> FBinaryOp (x, eval_formula f1 env, eval_formula f2 env)
  | FLabelledOp (x, y, f) -> FLabelledOp (x, eval_formula y env, eval_formula f env)
  | FFormulaIdent (ident, params) ->
      let params' = List.map (fun e -> eval_param e env) params in
      let (env', subst, f) = resolve_formula ident params' env in
      let f' = select_cond_expression (apply_cond_expression_subst f subst) in
        eval_formula f' env'
  | _ -> fexpr;;

let identifier_to_str = function
	IDefault s -> s
|   _ -> failwith "identifier is still indexed";;

let as_identifier_to_str = function
	FProp i -> identifier_to_str i
|	FVariable i -> identifier_to_str i
|	_ -> failwith "expression is a formula not an identifier";;
