open Tcsautomata;;
open Tcsautohelper;;
open Tcsautotransform;;
open Tcstransitionsys;;
open Tcsgames;;
open Tcsbasedata;;
open Tcsset;;
open Tcslist;;
open Tcsarray;;
open Tcstiming;;
open Tcsmessage;;
open Modelcheckinggames;;
open Tcsctlstarformula;;
open Ctlstarthreadnba;;

type ('a, 'b) nba_state = NbaFailed
                        | NbaWaiting
                        | TrackingE of 'a
                        | TrackingA of 'b

let get_nba (((r, fmls, lnks, props) as dcp): decomposed_ctlstar_formula)
			etracker
			atracker =
	let e_states = NMA.states etracker in
	let a_states = NMA.states atracker in
	let e_format = Domain.format e_states in
	let a_format = Domain.format a_states in
	let e_compare = Domain.compare e_states in
	let a_compare = Domain.compare a_states in
	let e_initial = NMA.initial etracker in
	let a_initial = NMA.initial atracker in
	let e_delta = fun x y -> Iterators.to_list (NMA.delta etracker x y) in
	let a_delta = fun x y -> Iterators.to_list (NMA.delta atracker x y) in
	let e_final = NMA.accept etracker in
	let a_final = NMA.accept atracker in

	let fmt_state = function
		NbaWaiting -> "<NbaWaiting>"
	|	NbaFailed -> "<NbaFailed>"
	|	TrackingE s -> e_format s
	|	TrackingA s -> a_format s
	in

	let cmp_state x y =
		let idx = function NbaFailed -> 1 | NbaWaiting -> 2 | TrackingE _ -> 3 | TrackingA _ -> 4 in
		let c = compare (idx x) (idx y) in
		if c != 0 then c else (
			match (x, y) with
				(TrackingE tx, TrackingE ty) -> e_compare tx ty
			|	(TrackingA tx, TrackingA ty) -> a_compare tx ty
			|	_ -> 0
		)
	in

	let fmt_alpha = ctlstar_thread_nba_format_rule dcp in

	let cmp_alpha = Comparators.default in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = NbaWaiting in

	let delta s r =
		let new_path =
			match r with
				FormulaBranch (f, true) -> (
					match fmls.(f) with
						FIntPath (b, _) -> Some b
					|	_ -> None
				)
			|	_ -> None
		in
		let result = match s with
			NbaFailed -> [NbaFailed]
		|	NbaWaiting -> (
			match new_path with
				None -> [NbaWaiting]
			|	Some b -> [NbaWaiting; (if b then TrackingE e_initial else TrackingA a_initial)]
		)
		|	TrackingE t -> (
			match new_path with
				None -> List.map (fun x -> TrackingE x) (e_delta t r)
			|	Some _ -> [NbaFailed]
		)
		|	TrackingA t -> (
			match new_path with
				None -> List.map (fun x -> TrackingA x) (a_delta t r)
			|	Some _ -> [NbaFailed]
		)
		in
			Iterators.of_list result
	in

	let accept = function
		TrackingE s -> e_final s
	|	TrackingA s -> a_final s
	|	_ -> false
	in

	NBA.build states alphabet initial delta accept;;
	
	
let get_nba_state_size e_size a_size =
	2 + e_size + a_size
 
type 'a state = Tracking of block * 'a * int
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
		                   dpa
						   ((init_state, (ltsprops, states)): explicit_initts) =
						   
	let dpa_initial = DMA.initial dpa in
	let dpa_delta x y = DMA.delta dpa x y in
	let dpa_omega = DMA.accept dpa in
	let dpa_format = Domain.format (DMA.states dpa) in

	let props_to_lts = compute_stringmap_identification (Array.map fst props) ltsprops in

	let initial_state = Tracking ((EBlock, [r], []), dpa_initial, init_state) in
	
	let omega = function
		Tracking ((ABlock, _, _), s, _) -> dpa_omega s
	|	Tracking ((EBlock, _, _), s, _) -> 1 + dpa_omega s
	|	Success -> 0
	| _ -> 1
	in
						   
	let format_state = function
		Failed -> "<FAILED>"
	|	Success -> "<SUCCESS>"
	|	Tracking (bl, s, i) ->
				"(" ^ format_block dcp bl ^ ", " ^ dpa_format s ^ ", " ^ string_of_int i ^ ")"
	in

	let rec delta s =
		let add_to_block = add_to_block dcp in
		match s with
			Tracking (bl, t, state) -> (
				match bl with
					(kind, f::rf, m) -> (
						match fmls.(f) with
							FIntAtom b ->
								if b && (kind = ABlock) then [Success]
								else if (not b) && (kind = EBlock) then [Failed]
								else let bl' = (kind, rf, m) in
								     [Tracking (bl', dpa_delta t (FormulaDelete f), state)]
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
										  [Tracking (bl', dpa_delta t (FormulaDelete f), state)]
						|	FIntBranch (b, l, r) ->
								if (b && (kind = ABlock)) || ((not b) && (kind = EBlock)) then (
									let bl' = add_to_block (kind, rf, m) [l; r] in
									[Tracking (bl', dpa_delta t (FormulaFollow f), state)]
								)
								else (
									let bll = add_to_block (kind, rf, m) [l] in
									let blr = add_to_block (kind, rf, m) [r] in
									[Tracking (bll, dpa_delta t (FormulaBranch (f, true)), state);
									 Tracking (blr, dpa_delta t (FormulaBranch (f, false)), state)]
								)
						|	FIntPath (b, g) ->
								let bl' = add_to_block ((if b then EBlock else ABlock), [], []) [g] in
								[Tracking (bl', dpa_delta t (FormulaBranch (f, true)), state);
								 Tracking ((kind, rf, m), dpa_delta t (FormulaDelete f), state)]
						|	FIntFixpoint (b, l, r) -> (
								let only_r_left = (kind = EBlock && b) || (kind = ABlock && (not b)) in
								let bll = add_to_block (kind, rf, m) (if only_r_left then [r] else [l;r]) in
								let blr = add_to_block (kind, rf, m) (if only_r_left then [l;lnks.(f).(0)] else [r;lnks.(f).(0)]) in
								[Tracking (bll, dpa_delta t (FormulaBranch (f, true)), state);
								 Tracking (blr, dpa_delta t (FormulaBranch (f, false)), state)]
							)
						|	_ -> failwith "impossible"
				)
				|	(EBlock, [], []) -> [Success]
				|	(ABlock, [], []) -> [Failed]
				|	(kind, [], m) -> (
					let (_, trans, _, _) = states.(state) in
					let bl' = add_to_block (kind, [], []) m in
					List.map (fun tar -> Tracking (bl', dpa_delta t FormulaNext, tar)) (Array.to_list trans)
				)
			)
		|	_ -> [s]
	in
	
	let exist_pos = function
		Tracking (bl, _, _) -> (
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
|	Tracking ((_, [], _::_), _, _) -> true
|	_ -> false	
	 
	 
let modelchecking_proc formula in_chan options (info_chan, formula_chan, constr_chan) =
	let game_in_annot = ArrayUtils.mem "ann" options in
	let compact_game = ArrayUtils.mem "comp" options in
	let info_msg = MessageChannel.send_message info_chan in
	let formula_msg = MessageChannel.send_message formula_chan in
	let constr_msg = MessageChannel.send_message constr_chan in
	
	info_msg (fun _ -> "Modelchecking Procedure For CTL*\n");
	
	formula_msg (fun _ -> "Transforming given formula...");
    let t = SimpleTiming.init true in
	let formula' = eval_metaformula formula in
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

	let nba_a = ctlstar_thread_nba_a_block decformula in
	let nba_a_state_cache = NMAStateCache.make2 nba_a in
	let nba_a_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_a_state_cache) in
	let nba_a_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_a_delta_cache) in
	let nba_a_cached = NMAAcceptCache.automaton nba_a_accept_cache in		
			
	let nba_e = NBA.complementAcceptance (ctlstar_thread_nba_e_block decformula) in
	let nba_e_state_cache = NMAStateCache.make2 nba_e in
	let nba_e_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_e_state_cache) in
	let nba_e_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_e_delta_cache) in
	let nba_e_cached = NMAAcceptCache.automaton nba_e_accept_cache in		
		
	let nba = get_nba decformula nba_e_cached nba_a_cached in
	let nba_state_cache = NMAStateCache.make2 nba in
	let nba_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_state_cache) in
	let nba_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_delta_cache) in
	let nba_cached = NMAAcceptCache.automaton nba_accept_cache in		

	let nba_size = get_nba_state_size (ctlstar_thread_nba_size decformula) (ctlstar_thread_nba_size decformula) in

	let dpa = NBAtoDPA.transform nba_cached nba_size in
	let dpa_state_cache = DMAStateCache.make2 dpa in
	let dpa_delta_cache = DMADeltaCache.make (DMAStateCache.automaton2 dpa_state_cache) in
	let dpa_accept_cache = DMAAcceptCache.make (DMADeltaCache.automaton dpa_delta_cache) in
	let dpa_cached = DMAAcceptCache.automaton dpa_accept_cache in
		
	let states_game = ref 0 in
	let game = get_modelchecking_game decformula dpa_cached ts in
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
		info_msg (fun _ -> "Game has " ^ string_of_int !states_game ^ " states (NBA " ^ string_of_int (NMAStateCache.state_size2 nba_state_cache) ^ ", DPA " ^ string_of_int (DMAStateCache.state_size2 dpa_state_cache) ^ ").\n")
	in
	
   (game_cached', show_stats, (fun sol -> if sol init = Some true then ModelSatisfiesFormula else ModelFalsifiesFormula));;
   

Modelcheckinggames.register_modelchecking_procedure
	modelchecking_proc
	"ctlstar"
	"Modelchecking Procedure For CTL*";;