open Tcsmetaformula;;
open Tcsmessage;;
open Tcsautomata;;
open Tcsgames;;

type validity_answer =
	FormulaValid
|	FormulaFalsifiable
|	FormulaFalsifiableBy of ((string -> unit) -> unit)

type validity_procedure = 
	formula_expr ->
	string array -> (* options; ann for annotations; comp for compact games *)
	(MessageChannel.message_channel * (* general information *)
	 MessageChannel.message_channel * (* formula information *)
	 MessageChannel.message_channel) -> (* construction information *)
	(int initpg * (* the game *)
	 (unit -> unit) * (* final stats output function *)
	 (int initpg_solution -> int initpg_strategy -> validity_answer)) (* answer function *)

val register_validity_procedure: validity_procedure -> string -> string -> unit

val mem_validity_procedure: string -> bool

val find_validity_procedure: string -> validity_procedure * string

val enum_validity_procedures: (validity_procedure -> string -> string -> unit) -> unit

val fold_validity_procedures: (validity_procedure -> string -> string -> 'a -> 'a) -> 'a -> 'a