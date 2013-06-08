type labelled_mmc_formula =
    FProp of string
  | FVariable of string
  | FTT
  | FFF
  | FNeg of labelled_mmc_formula
  | FAnd of labelled_mmc_formula * labelled_mmc_formula
  | FOr of labelled_mmc_formula * labelled_mmc_formula
  | FDiamond of string * labelled_mmc_formula
  | FBox of string * labelled_mmc_formula
  | FMu of string * labelled_mmc_formula
  | FNu of string * labelled_mmc_formula
  
val is_closed : labelled_mmc_formula -> bool
val is_guarded : labelled_mmc_formula -> bool
val is_guarded_wrt : labelled_mmc_formula -> string -> bool
val is_uniquely_bound : labelled_mmc_formula -> bool
val make_uniquely_bound : labelled_mmc_formula -> labelled_mmc_formula

val eval_metaformula : Metaformula.formula_expr -> labelled_mmc_formula

val formula_length: labelled_mmc_formula -> int
  
val formula_height: labelled_mmc_formula -> int

(* Total, Guarded, Unguarded *)
val formula_variable_occs: labelled_mmc_formula -> int * int * int

val format_formula : labelled_mmc_formula -> string

val format_formula_as_tree : labelled_mmc_formula -> string

val formula_to_positive : labelled_mmc_formula -> labelled_mmc_formula

val is_positive : labelled_mmc_formula -> bool

val guarded_transform : labelled_mmc_formula -> labelled_mmc_formula

type decomposed_labelled_mmc_formula_part =
    FIntAtom of bool (* player *)
  | FIntVariable of int (* variable reference *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntModality of bool * int * int (* player * label reference * formula *)
  | FIntProp of bool * int (* negation * proposition reference *)

type mmc_proposition_data = int * int (* prop, positive formula, negative formula *)

type mmc_variable_data = bool * int * bool * int (* player, fixed point priority * formula *)

type mmc_label_data = string
  
type decomposed_labelled_mmc_formula =
	int *
	decomposed_labelled_mmc_formula_part array *
	(string * mmc_proposition_data) array *
	(string * mmc_variable_data) array *
	mmc_label_data array

val normal_form_formula_to_decomposed_formula :
  labelled_mmc_formula -> decomposed_labelled_mmc_formula

val get_formula_depth:
   decomposed_labelled_mmc_formula ->
   decomposed_labelled_mmc_formula_part -> int

val sort_decomposed_formula :
  decomposed_labelled_mmc_formula ->
  (decomposed_labelled_mmc_formula ->
   decomposed_labelled_mmc_formula_part ->
   decomposed_labelled_mmc_formula_part -> int) ->
  decomposed_labelled_mmc_formula

val decomposed_formula_subformula_cardinality: decomposed_labelled_mmc_formula -> int

val decomposed_formula_to_formula :
  decomposed_labelled_mmc_formula -> int -> labelled_mmc_formula

val format_decomposed_formula :
  decomposed_labelled_mmc_formula -> int -> string