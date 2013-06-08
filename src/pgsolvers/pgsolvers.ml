open Tcsgames;;
open Tcsset;;

type solver_func = (string array -> (explicit_pg -> explicit_pg_solution * explicit_pg_strategy))

let solvermap = ref TreeMap.empty_def;;

let register_solver solver_func identifier abbreviation description =
	if TreeMap.mem identifier !solvermap
	then failwith ("Solver `" ^ identifier ^ "' already registered!\n")
	else solvermap := TreeMap.add identifier (solver_func, abbreviation, description) !solvermap;;
	
let mem_solver identifier = TreeMap.mem identifier !solvermap;;

let find_solver identifier = TreeMap.find identifier !solvermap;;

let enum_solvers it = TreeMap.iter (fun i (f, a, d) -> it f i a d) !solvermap;;

let fold_solvers fo b = TreeMap.fold (fun i (f, a, d) x -> fo f i a d x) !solvermap b;;


type partial_solver_func = (string array -> (int initpg -> int initpg_solution * int initpg_strategy))

let partial_solvermap = ref TreeMap.empty_def;;

let register_partial_solver partial_solver_func identifier abbreviation description =
	if TreeMap.mem identifier !partial_solvermap
	then failwith ("partial solver `" ^ identifier ^ "' already registered!\n")
	else partial_solvermap := TreeMap.add identifier (partial_solver_func, abbreviation, description) !partial_solvermap;;
	
let mem_partial_solver identifier = TreeMap.mem identifier !partial_solvermap;;

let find_partial_solver identifier = TreeMap.find identifier !partial_solvermap;;

let enum_partial_solvers it = TreeMap.iter (fun i (f, a, d) -> it f i a d) !partial_solvermap;;

let fold_partial_solvers fo b = TreeMap.fold (fun i (f, a, d) x -> fo f i a d x) !partial_solvermap b;;