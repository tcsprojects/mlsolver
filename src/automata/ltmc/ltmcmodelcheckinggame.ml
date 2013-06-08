open Tcsautomata;;
open Tcsgames;;
open Tcstransitionsys;;
open Tcsset;;
open Tcslist;;
open Tcsarray;;
open Tcstiming;;
open Tcsmessage;;
open Ltmcformula;;
open Modelcheckinggames;;

type state =
    Atom of int
  | Tuple of (int * int)
  
type modelchecking_game = state initpg

let compute_stringmap_identification source target =
	let target_map = ref (TreeMap.empty_def) in
	Array.iteri (fun i s -> target_map := TreeMap.add s i !target_map) target;
	Array.init (Array.length source) (fun i ->
		try
			TreeMap.find source.(i) !target_map
		with Not_found -> -1
	);;

let get_modelchecking_game (((r, fmls, props, vars) as fml): decomposed_ltmc_formula)
                           ((init_state, (ltsprops, states)): explicit_initts) =

	let props_to_lts = compute_stringmap_identification (Array.map fst props) ltsprops in
						   
	let initial_state =	Tuple (r, init_state) in
	
	let delta = function
		Atom i -> [Atom i]
	|	Tuple (formula, state) ->
			match fmls.(formula) with
				FIntAtom b -> [Atom (if b then 0 else 1)]
			|	FIntVariable v -> [Tuple ((let (_, _, _, f) = snd vars.(v) in f), state)]
			|	FIntBranch (_, l, r) -> [Tuple (l, state); Tuple (r, state)]
			|	FIntNext f ->
					let (_, trans, _, _) = states.(state) in
					if (Array.length trans > 1) then failwith "Out-degree of transition system > 1";
					List.map (fun tar -> Tuple (f, tar)) (Array.to_list trans)
			|	FIntProp (positive, p) ->
					let p' = props_to_lts.(p) in
					if p' = -1 then [Atom (if positive then 1 else 0)]
					else let (ps, _, _, _) = states.(state) in
					     let i = ref 0 in
						 let l = Array.length ps in
						 let found = ref false in
						 while (not !found) && (!i < l) do
							found := ps.(!i) = p';
							incr i
						 done;
						 [Atom (if ((not !found) || positive) && (!found || (not positive)) then 0 else 1)]
	in
	
	let omega = function
		Atom i -> i
	|	Tuple (s, _) ->
			match fmls.(s) with
				FIntVariable v -> let (_, pr, _, _) = snd vars.(v) in pr
			|	_ -> 0
	in
	
	let exist_pos = function
		Tuple (s, _) ->
			(
			match fmls.(s) with
			|	FIntBranch (false, _, _) -> false
			|	_ -> true
			)
	|	_ -> true
	in
	
	let format_formula = format_decomposed_formula fml in
	
	let format_state state =
		match states.(state) with
			(_, _, None, _) -> string_of_int state
		|	(_, _, Some s, _) -> s
	in
	
	let format_state = function
		Atom i -> "Atom " ^ string_of_int i
    |   Tuple (formula, state) -> "Tuple (" ^ format_formula formula ^ ", " ^ format_state state ^ ")"
	in
		
	((initial_state, delta, omega, exist_pos, format_state),
	 (None, None, Some compare));;
	 
	 
let identify_step_state (_, fmls, _, _) = function
	Atom _ -> true
|	Tuple (formula, _) ->
		match fmls.(formula) with
			FIntNext _ -> true
		|	_ -> false;;

		
let modelchecking_proc formula in_chan options (info_chan, formula_chan, constr_chan) =
	let game_in_annot = ArrayUtils.mem "ann" options in
	let compact_game = ArrayUtils.mem "comp" options in
	let info_msg = MessageChannel.send_message info_chan in
	let formula_msg = MessageChannel.send_message formula_chan in
	let constr_msg = MessageChannel.send_message constr_chan in
	
	info_msg (fun _ -> "Modelchecking Procedure For Linear-Time Mu Calculus\n");
	
	formula_msg (fun _ -> "Transforming given formula...");
    let t = SimpleTiming.init true in
	let formula' = eval_metaformula formula in
    let goalformula = make_uniquely_bound (formula_to_positive formula') in

	formula_msg (fun _ -> SimpleTiming.format t ^ "\n");
	formula_msg (fun _ -> "Transformed formula: " ^ format_formula goalformula ^ "\n");
	formula_msg (fun _ -> "Formula Length: " ^ string_of_int (formula_length goalformula) ^ "\n");
	
	constr_msg (fun _ -> "Parsing transition system...");
	let t = SimpleTiming.init true in
	let ts = Tcstransitionsysparser.parse_explicit_initts in_chan in
	constr_msg (fun _ -> SimpleTiming.format t ^ "\n");

	constr_msg (fun _ -> "Decompose formula...");
    let t = SimpleTiming.init true in
    let decformula = normal_form_formula_to_decomposed_formula goalformula in
	constr_msg (fun _ -> SimpleTiming.format t ^ "\n");
	
	let states_game = ref 0 in

	constr_msg (fun _ -> "Initializing game...");
	let game = get_modelchecking_game decformula ts in
	let game = if compact_game then get_compact_initpg game (identify_step_state decformula) else get_escaped_initpg game 0 in
    let (game_cached, state_to_int, int_to_state) = get_int_cached_initpg game (fun _ i -> states_game := i) in
	constr_msg (fun _ -> "finished.\n");
	
    let ((init, b, c, d, fo), e) = game_cached in

	let fo' = if game_in_annot
	          then (fun s -> fo s)
			  else (fun _ -> "")
	in

	let game_cached' = ((init, b, c, d, fo'), e) in
	
	let show_stats _ =
		info_msg (fun _ -> "Game has " ^ string_of_int !states_game ^ " states.\n")
	in
	
   (game_cached', show_stats, (fun sol -> if sol init = Some true then ModelSatisfiesFormula else ModelFalsifiesFormula));;
   

Modelcheckinggames.register_modelchecking_procedure
	modelchecking_proc
	"ltmc"
	"Modelchecking Procedure For Linear-Time Mu Calculus";;
