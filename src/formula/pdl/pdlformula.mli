type pdl_formula = 
	FProp of string
  | FTT
  | FFF
  | FNeg of pdl_formula
  | FAnd of pdl_formula * pdl_formula
  | FOr of pdl_formula * pdl_formula
  | FDiamond of pdl_program * pdl_formula
  | FBox of pdl_program * pdl_formula
and pdl_program =
    FAtom of string
  | FConcat of pdl_program * pdl_program
  | FChoice of pdl_program * pdl_program
  | FStar of pdl_program
  | FQuestion of pdl_formula

val eval_metaformula : Metaformula.formula_expr -> pdl_formula
val eval_program : Metaformula.formula_expr -> pdl_program

val formula_length: pdl_formula -> int
val program_length: pdl_program -> int

val format_formula : pdl_formula -> string
val format_program : pdl_program -> string

val formula_to_positive : pdl_formula -> pdl_formula
val program_to_positive : pdl_program -> pdl_program

val is_positive : pdl_formula -> bool
val is_positive_program : pdl_program -> bool


type decomposed_pdl_formula_part =
    FIntAtom of bool (* player *)
  | FIntProp of bool * int (* negation * proposition reference *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntModality of bool * int * int (* player * program * formula *)
  
type decomposed_pdl_program_part =
	FIntPAtom of int (* program atom reference *)
  | FIntQuestion of int
  | FIntConcat of int * int
  | FIntChoice of int * int
  | FIntStar of int

type decomposed_pdl_formula =
	int *
	decomposed_pdl_formula_part array *
	(int array) array * (* contains additional links from formula to formulas *)
	decomposed_pdl_program_part array *
	(string * (int * int)) array * (* prop, positive formula, negative formula *)
	string array (* program atoms *)


val normal_form_formula_to_decomposed_formula :
  pdl_formula -> (pdl_formula -> pdl_formula array) -> decomposed_pdl_formula


val sort_decomposed_formula :
  decomposed_pdl_formula ->
  (decomposed_pdl_formula ->
   decomposed_pdl_formula_part ->
   decomposed_pdl_formula_part -> int) ->
  decomposed_pdl_formula
  
val get_formula_depth:
   decomposed_pdl_formula ->
   decomposed_pdl_formula_part -> int

val decomposed_formula_to_formula :
  decomposed_pdl_formula -> int -> pdl_formula
val decomposed_program_to_program :
  decomposed_pdl_formula -> int -> pdl_program

val format_decomposed_formula :
  decomposed_pdl_formula -> int -> string

val format_decomposed_program :
  decomposed_pdl_formula -> int -> string 
  
val pdl_formula_link_map: pdl_formula -> pdl_formula array