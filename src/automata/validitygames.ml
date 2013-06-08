open Tcsset;;
open Metaformula;;
open Tcsmessage;;
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

let proceduremap = ref TreeMap.empty_def;;

let register_validity_procedure procedure identifier description =
	if TreeMap.mem identifier !proceduremap
	then failwith ("Procedure `" ^ identifier ^ "' already registered!\n")
	else proceduremap := TreeMap.add identifier (procedure, description) !proceduremap;;

let mem_validity_procedure identifier = TreeMap.mem identifier !proceduremap;;

let find_validity_procedure identifier = TreeMap.find identifier !proceduremap;;

let enum_validity_procedures it = TreeMap.iter (fun i (f, d) -> it f i d) !proceduremap;;

let fold_validity_procedures fo b = TreeMap.fold (fun i (f, d) x -> fo f i d x) !proceduremap b;;	