open Tcsmetaformula;;
open Tcsmessage;;
open Tcsgames;;

type modelchecking_answer =
	ModelSatisfiesFormula
|	ModelFalsifiesFormula

type modelchecking_procedure = 
	formula_expr ->
	in_channel ->
	string array -> (* options *)
	(MessageChannel.message_channel * (* general information *)
	 MessageChannel.message_channel * (* formula information *)
	 MessageChannel.message_channel) -> (* construction information *)
	(int initpg * (* the game *)
	 (unit -> unit) * (* final stats output function *)
	 (int initpg_solution -> modelchecking_answer)) (* answer function *)

val register_modelchecking_procedure: modelchecking_procedure -> string -> string -> unit

val mem_modelchecking_procedure: string -> bool

val find_modelchecking_procedure: string -> modelchecking_procedure * string

val enum_modelchecking_procedures: (modelchecking_procedure -> string -> string -> unit) -> unit

val fold_modelchecking_procedures: (modelchecking_procedure -> string -> string -> 'a -> 'a) -> 'a -> 'a