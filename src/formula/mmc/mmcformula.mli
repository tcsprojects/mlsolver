type mmc_formula = 
	FProp of string
  | FVariable of string
  | FTT
  | FFF
  | FNeg of mmc_formula
  | FAnd of mmc_formula * mmc_formula
  | FOr of mmc_formula * mmc_formula
  | FDiamond of mmc_formula
  | FBox of mmc_formula
  | FMu of string * mmc_formula
  | FNu of string * mmc_formula
  
val is_closed : mmc_formula -> bool
val is_guarded : mmc_formula -> bool
val is_guarded_wrt : mmc_formula -> string -> bool
val is_uniquely_bound : mmc_formula -> bool
val make_uniquely_bound : mmc_formula -> mmc_formula

val eval_metaformula : Metaformula.formula_expr -> mmc_formula

val formula_length: mmc_formula -> int

val formula_height: mmc_formula -> int

(* Total, Guarded, Unguarded *)
val formula_variable_occs: mmc_formula -> int * int * int


val format_formula : mmc_formula -> string

val format_formula_as_tree : mmc_formula -> string


val formula_to_positive : mmc_formula -> mmc_formula

val is_positive : mmc_formula -> bool

val guarded_transform : mmc_formula -> mmc_formula

val guarded_kupferman_vardi_wolper_transform : mmc_formula -> mmc_formula

type decomposed_mmc_formula_part =
    FIntAtom of bool (* player *)
  | FIntVariable of int (* variable reference *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntModality of bool * int (* player * formula *)
  | FIntProp of bool * int (* negation * proposition reference *)

type mmc_proposition_data = int * int (* prop, positive formula, negative formula *)

type mmc_variable_data = bool * int * bool * int (* player, fixed point priority * guarded * formula *)

type decomposed_mmc_formula =
	int *
	decomposed_mmc_formula_part array *
	(string * mmc_proposition_data) array *
	(string * mmc_variable_data) array

val normal_form_formula_to_decomposed_formula :
  mmc_formula -> decomposed_mmc_formula

val sort_decomposed_formula :
  decomposed_mmc_formula ->
  (decomposed_mmc_formula ->
   decomposed_mmc_formula_part ->
   decomposed_mmc_formula_part -> int) ->
  decomposed_mmc_formula
  
val decomposed_formula_subformula_cardinality: decomposed_mmc_formula -> int

val get_formula_depth:
   decomposed_mmc_formula ->
   decomposed_mmc_formula_part -> int

val decomposed_formula_to_formula :
  decomposed_mmc_formula -> int -> mmc_formula

val format_decomposed_formula :
  decomposed_mmc_formula -> int -> string