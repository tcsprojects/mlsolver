open Tcsset;;
open Metaformula;;
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
	 
let proceduremap = ref TreeMap.empty_def;;

let register_modelchecking_procedure procedure identifier description =
	if TreeMap.mem identifier !proceduremap
	then failwith ("Procedure `" ^ identifier ^ "' already registered!\n")
	else proceduremap := TreeMap.add identifier (procedure, description) !proceduremap;;

let mem_modelchecking_procedure identifier = TreeMap.mem identifier !proceduremap;;

let find_modelchecking_procedure identifier = TreeMap.find identifier !proceduremap;;

let enum_modelchecking_procedures it = TreeMap.iter (fun i (f, d) -> it f i d) !proceduremap;;

let fold_modelchecking_procedures fo b = TreeMap.fold (fun i (f, d) x -> fo f i d x) !proceduremap b;;	