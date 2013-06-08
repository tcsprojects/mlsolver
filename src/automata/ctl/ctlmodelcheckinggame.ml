open Tcsautomata;;
open Tcsgames;;
open Tcstransitionsys;;

open Tcsset;;
open Tcslist;;
open Tcsarray;;
open Tcstiming;;
open Tcsmessage;;
open Modelcheckinggames;;
open Ctlstarformula;;
			  
type state = Tracking of block * int
           | Failed
	       | Success

let compute_stringmap_identification source target =
	let target_map = ref (TreeMap.empty_def) in
	Array.iteri (fun i s -> target_map := TreeMap.add s i !target_map) target;
	Array.init (Array.length source) (fun i ->
		try
			TreeMap.find source.(i) !target_map
		with Not_found -> -1
	);;

let get_modelchecking_game (((r, fmls, lnks, props) as dcp): decomposed_ctlstar_formula)
						   ((init_state, (ltsprops, states)): explicit_initts) =

	let props_to_lts = compute_stringmap_identification (Array.map fst props) ltsprops in

	let initial_state = Tracking ((EBlock, [r], []), init_state) in
	
	let omega = function
		Tracking ((_, [], _), _) -> 1
	|	Tracking ((_, f::_, _), _) -> (
			match fmls.(f) with
				FIntFixpoint (false, _, _) -> 2
			|	_ -> 1
		)
	|	Success -> 0
	| _ -> 1
	in
						   
	let format_state = function
		Failed -> "<FAILED>"
	|	Success -> "<SUCCESS>"
	|	Tracking (bl, i) ->
				"(" ^ format_block dcp bl ^ ", " ^ string_of_int i ^ ")"
	in

	let rec delta s =
		let add_to_block = add_to_block dcp in
		match s with
			Tracking (bl, state) -> (
				match bl with
					(kind, f::rf, m) -> (
						match fmls.(f) with
							FIntAtom b ->
								if b && (kind = ABlock) then [Success]
								else if (not b) && (kind = EBlock) then [Failed]
								else let bl' = (kind, rf, m) in
								     [Tracking (bl', state)]
						|	FIntProp (positive, p) ->
								let p' = props_to_lts.(p) in
								if p' = -1 then [if positive then Failed else Success]
								else let (ps, _, _, _) = states.(state) in
									 let i = ref 0 in
									 let l = Array.length ps in
									 let found = ref false in
									 while (not !found) && (!i < l) do
										found := ps.(!i) = p';
										incr i
									 done;
									 let is_good = (!found && positive) || not (!found || positive) in
								 	 if is_good && (kind = ABlock) then [Success]
								 	 else if (not is_good) && (kind = EBlock) then [Failed]
								 	 else let bl' = (kind, rf, m) in
										  [Tracking (bl', state)]
						|	FIntBranch (b, l, r) ->
								if (b && (kind = ABlock)) || ((not b) && (kind = EBlock)) then (
									let bl' = add_to_block (kind, rf, m) [l; r] in
									[Tracking (bl', state)]
								)
								else (
									let bll = add_to_block (kind, rf, m) [l] in
									let blr = add_to_block (kind, rf, m) [r] in
									[Tracking (bll, state);
									 Tracking (blr, state)]
								)
						|	FIntPath (b, g) ->
								let bl' = add_to_block ((if b then EBlock else ABlock), [], []) [g] in
								[Tracking (bl', state);
								 Tracking ((kind, rf, m), state)]
						|	FIntFixpoint (b, l, r) -> (
								let only_r_left = (kind = EBlock && b) || (kind = ABlock && (not b)) in
								let bll = add_to_block (kind, rf, m) (if only_r_left then [r] else [l;r]) in
								let blr = add_to_block (kind, rf, m) (if only_r_left then [l;lnks.(f).(0)] else [r;lnks.(f).(0)]) in
								[Tracking (bll, state);
								 Tracking (blr, state)]
							)
						|	_ -> failwith "impossible"
				)
				|	(EBlock, [], []) -> [Success]
				|	(ABlock, [], []) -> [Failed]
				|	(kind, [], m) -> (
					let (_, trans, _, _) = states.(state) in
					let bl' = add_to_block (kind, [], []) m in
					List.map (fun tar -> Tracking (bl', tar)) (Array.to_list trans)
				)
			)
		|	_ -> [s]
	in
	
	let exist_pos = function
		Tracking (bl, _) -> (
			match bl with
				(kind, f::rf, _) -> (
					match fmls.(f) with
						FIntBranch (b, _, _) -> (kind = EBlock) && b
					|	FIntPath (_, _) -> kind = ABlock
					|	FIntFixpoint _ -> kind = EBlock
					|	_ -> false
				)
			|	(kind, [], []) -> false
			|	(ABlock, [], _) -> false
			|	(EBlock, [], _) -> true
		)
	|	_ -> false
	in
	
	((initial_state, delta, omega, exist_pos, format_state),
	 (None, None, Some compare));;
	 
let identify_step_state = function
	Failed -> true
|	Success -> true
|	Tracking ((_, [], _::_), _) -> true
|	_ -> false	
	 
	 
let modelchecking_proc formula in_chan options (info_chan, formula_chan, constr_chan) =
	let game_in_annot = ArrayUtils.mem "ann" options in
	let compact_game = ArrayUtils.mem "comp" options in
	let info_msg = MessageChannel.send_message info_chan in
	let formula_msg = MessageChannel.send_message formula_chan in
	let constr_msg = MessageChannel.send_message constr_chan in
	
	info_msg (fun _ -> "Modelchecking Procedure For CTL\n");
	
	formula_msg (fun _ -> "Transforming given formula...");
    let t = SimpleTiming.init true in
	let formula' = eval_metaformula formula in
	if not (is_ctl_formula formula') then failwith("Formula is not able to be interpreted as CTL");
	
    let goalformula = formula_to_positive (FExists formula') in

	formula_msg (fun _ -> SimpleTiming.format t ^ "\n");
	formula_msg (fun _ -> "Transformed formula: " ^ format_formula goalformula ^ "\n");
	formula_msg (fun _ -> "Formula Length: " ^ string_of_int (formula_length goalformula) ^ "\n");
	
	constr_msg (fun _ -> "Parsing transition system...");
	let t = SimpleTiming.init true in
	let ts = Tcstransitionsysparser.parse_explicit_initts in_chan in
	constr_msg (fun _ -> SimpleTiming.format t ^ "\n");

	constr_msg (fun _ -> "Decompose formula...");
    let t = SimpleTiming.init true in
    let decformula = normal_form_formula_to_decomposed_formula goalformula ctlstar_formula_link_map in
	constr_msg (fun _ -> SimpleTiming.format t ^ "\n");
	
	constr_msg (fun _ -> "Initializing game...");
	
	let states_game = ref 0 in
		
	let game = get_modelchecking_game decformula ts in
	let game = if compact_game then get_compact_initpg game identify_step_state else get_escaped_initpg game 0 in
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
	"ctl"
	"Modelchecking Procedure For CTL";;