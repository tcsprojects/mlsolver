type ctlstar_formula = 
	FProp of string
  | FTT
  | FFF
  | FNeg of ctlstar_formula
  | FAnd of ctlstar_formula * ctlstar_formula
  | FOr of ctlstar_formula * ctlstar_formula
  | FNext of ctlstar_formula
  | FExists of ctlstar_formula
  | FForall of ctlstar_formula
  | FRelease of ctlstar_formula *ctlstar_formula
  | FUntil of ctlstar_formula * ctlstar_formula

val eval_metaformula : Metaformula.formula_expr -> ctlstar_formula

val is_ctl_formula: ctlstar_formula -> bool

val formula_length: ctlstar_formula -> int

val format_formula : ctlstar_formula -> string

val formula_to_positive : ctlstar_formula -> ctlstar_formula

val is_positive : ctlstar_formula -> bool


type decomposed_ctlstar_formula_part =
    FIntAtom of bool (* player *)
  | FIntProp of bool * int (* negation * proposition reference *)
  | FIntNext of int (* formula *)
  | FIntPath of bool * int (* player * formula *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntFixpoint of bool * int * int (* player * left formula * right formula *)

type decomposed_ctlstar_formula =
	int *
	decomposed_ctlstar_formula_part array *
	(int array) array * (* contains additional links from formula to formulas *)
	(string * (int * int)) array (* prop, positive formula, negative formula *)

val normal_form_formula_to_decomposed_formula :
  ctlstar_formula -> (ctlstar_formula -> ctlstar_formula array) -> decomposed_ctlstar_formula

val sort_decomposed_formula :
  decomposed_ctlstar_formula ->
  (decomposed_ctlstar_formula ->
   decomposed_ctlstar_formula_part ->
   decomposed_ctlstar_formula_part -> int) ->
  decomposed_ctlstar_formula

val get_formula_depth:
   decomposed_ctlstar_formula ->
   decomposed_ctlstar_formula_part -> int

val decomposed_formula_to_formula :
  decomposed_ctlstar_formula -> int -> ctlstar_formula

val format_decomposed_formula :
  decomposed_ctlstar_formula -> int -> string
  
type block_type = EBlock | ABlock

type block = block_type * 
             int list *     (* non-modal formulas *)
             int list       (* modal formulas *)

val format_block: decomposed_ctlstar_formula -> block -> string

val add_to_block: decomposed_ctlstar_formula -> block -> int list -> block

val ctlstar_formula_link_map: ctlstar_formula -> ctlstar_formula array
