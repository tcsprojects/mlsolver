type ltmc_formula = 
	FProp of string
  | FVariable of string
  | FTT
  | FFF
  | FNeg of ltmc_formula
  | FAnd of ltmc_formula * ltmc_formula
  | FOr of ltmc_formula * ltmc_formula
  | FNext of ltmc_formula
  | FMu of string * ltmc_formula
  | FNu of string * ltmc_formula
  
val is_closed : ltmc_formula -> bool
val is_guarded : ltmc_formula -> bool
val is_guarded_wrt : ltmc_formula -> string -> bool
val is_uniquely_bound : ltmc_formula -> bool
val make_uniquely_bound : ltmc_formula -> ltmc_formula

val eval_metaformula : Metaformula.formula_expr -> ltmc_formula

val formula_length: ltmc_formula -> int

val format_formula : ltmc_formula -> string

val formula_to_positive : ltmc_formula -> ltmc_formula

val is_positive : ltmc_formula -> bool

val guarded_transform : ltmc_formula -> ltmc_formula

type decomposed_ltmc_formula_part =
    FIntAtom of bool (* player *)
  | FIntVariable of int (* variable reference *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntNext of int (* formula *)
  | FIntProp of bool * int (* negation * proposition reference *)

type ltmc_proposition_data = int * int (* prop, positive formula, negative formula *)

type ltmc_variable_data = bool * int * bool * int (* player, fixed point priority * formula *)

type decomposed_ltmc_formula =
	int *
	decomposed_ltmc_formula_part array *
	(string * ltmc_proposition_data) array *
	(string * ltmc_variable_data) array

val normal_form_formula_to_decomposed_formula :
  ltmc_formula -> decomposed_ltmc_formula

val sort_decomposed_formula :
  decomposed_ltmc_formula ->
  (decomposed_ltmc_formula ->
   decomposed_ltmc_formula_part ->
   decomposed_ltmc_formula_part -> int) ->
  decomposed_ltmc_formula

val decomposed_formula_subformula_cardinality: decomposed_ltmc_formula -> int

val get_formula_depth:
   decomposed_ltmc_formula ->
   decomposed_ltmc_formula_part -> int

val decomposed_formula_to_formula :
  decomposed_ltmc_formula -> int -> ltmc_formula

val format_decomposed_formula :
  decomposed_ltmc_formula -> int -> string