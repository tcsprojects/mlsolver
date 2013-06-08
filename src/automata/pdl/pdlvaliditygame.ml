open Tcsautomata;;
open Tcsautohelper;;
open Tcsautotransform;;
open Tcstransitionsys;;
open Tcsgames;;
open Tcsset;;
open Tcslist;;
open Tcsarray;;
open Tcsbasedata;;
open Tcstiming;;
open Tcsmessage;;
open Pdlformula;;
open Pdlthreadnba;;
open Validitygames;;


type 'a state =
    TT
  | NT of int list
  | Tuple of (((int * int list) list) *     (* everything other + occ set *)
              (int list) *            		(* direct labelled modalities *)
			  (int list)) *           		(* propositions *)
			  'a;;

type 'a validity_game = 'a state initpg;;

type simplification_result = SimplTT | SimplFF | SimplKeep;;

let rec simplify ((_, fmls, _, _, prop, _) as fml) props i =
	match fmls.(i) with
		FIntAtom b -> if b then SimplTT else SimplFF
	|	FIntProp (b, p) -> if TreeSet.mem ((if b then fst else snd) (snd prop.(p))) props then SimplFF
	                       else if TreeSet.mem ((if b then snd else fst) (snd prop.(p))) props then SimplTT
						   else SimplKeep
	|	FIntBranch (true, l, r) -> (
			match simplify fml props l with
				SimplTT -> SimplTT
			|	SimplFF -> simplify fml props r
			|	SimplKeep -> if simplify fml props r = SimplTT then SimplTT else SimplKeep
		)
	|	FIntBranch (false, l, r) -> (
			match simplify fml props l with
				SimplFF -> SimplFF
			|	SimplTT -> simplify fml props r
			|	SimplKeep -> if simplify fml props r = SimplFF then SimplFF else SimplKeep
		)
	|	_ -> SimplKeep

let get_validity_game (((r, fmls, lnks, prgs, props, prgat) as fml): decomposed_pdl_formula)
                       dba
					   use_literal_propagation =

	let dba_initial_state = DMA.initial dba in
	let dba_delta x y = DMA.delta dba x y in
	let dba_final = DMA.accept dba in
	let state_cmp = Domain.compare (DMA.states dba) in
	let dba_state_format = Domain.format (DMA.states dba) in

	let format_formula = format_decomposed_formula fml in
	
	(* Sorts in new formulas and identifies TT / NT. *)
	let rec finish new_formulas = function
		TT -> TT
	|	NT s -> NT s
	|	Tuple ((a, b, c), d) -> (
		let (newa, newb, newc, is_true) = (ref [], ref [], ref [], ref false) in
		List.iter (fun (f, forbidden) ->
			match fmls.(f) with
				FIntAtom b -> if b then is_true := true
			|	FIntProp (b, p) -> newc := (f, (if b then snd else fst) (snd props.(p)))::!newc
			|	FIntModality (isdia, j, _) -> (
					match prgs.(j) with
						FIntPAtom _ -> newb := f::!newb
					|	_ -> if not (TreeSet.mem f forbidden) then newa := (f, TreeSet.elements forbidden)::!newa
					         else if not isdia then is_true := true
				)
			|	_ -> newa := (f, TreeSet.elements forbidden)::!newa
		) new_formulas;
		if !is_true then TT else (
			let c = ref (TreeSet.of_list_def c) in
			List.iter (fun (f, f') ->
				if (TreeSet.mem f' !c) then is_true := true else c := TreeSet.add f !c
			) !newc;
			if !is_true then TT else (
				let a = TreeSet.elements (TreeSet.union (TreeSet.of_list compare !newa) (TreeSet.of_list compare a)) in
				let b = TreeSet.elements (TreeSet.union (TreeSet.of_list_def !newb) (TreeSet.of_list_def b)) in
				let c = TreeSet.elements !c in
				if (a = []) && (b = [])
				then NT c
				else Tuple ((a, b, c), d)
			)
		)
	)
	in
	
	let initial_state =	finish [(r, TreeSet.empty_def)] (Tuple (([], [], []), dba_initial_state)) in
	
	let exist_pos = function
		Tuple ((x::_, _, _), _) -> false
	|	_ -> true
	in
	
	let rec delta = function
		Tuple ((a, b, c), d) -> (
		match a with
			(f, forb)::a -> (
				let forb = TreeSet.of_list_def forb in
				match fmls.(f) with
				|	FIntBranch (true, f1, f2) ->
						if use_literal_propagation then (
							let propsset = TreeSet.of_list_def c in
							let f1s = simplify fml propsset f1 in
							let f2s = simplify fml propsset f2 in
							if (f1s = SimplFF && f2s = SimplFF) then
								[finish [] (Tuple ((a, b, c), dba_delta d (Pdlthreadnba.Delete f)))]
							else if (f1s = SimplTT || f2s = SimplTT) then
								[TT]
							else if (f1s = SimplFF || f2s = SimplFF) then
								let d = dba_delta d (Pdlthreadnba.Follow f) in
								let d = dba_delta d (Pdlthreadnba.Delete (if f1s = SimplFF then f1 else f2)) in
								[finish [((if f2s = SimplFF then f1 else f2), forb)] (Tuple ((a, b, c), d))]
							else
								[finish [(f1, forb); (f2, forb)] (Tuple ((a, b, c), dba_delta d (Pdlthreadnba.Follow f)))]
						)
						else
							[finish [(f1, forb); (f2, forb)] (Tuple ((a, b, c), dba_delta d (Pdlthreadnba.Follow f)))]
				|	FIntBranch (false, f1, f2) ->
						if use_literal_propagation then (
							let propsset = TreeSet.of_list_def c in
							let f1s = simplify fml propsset f1 in
							let f2s = simplify fml propsset f2 in
							if (f1s = SimplFF || f2s = SimplFF) then
								[finish [] (Tuple ((a, b, c), dba_delta d (Pdlthreadnba.Delete f)))]
							else if (f1s = SimplTT && f2s = SimplTT) then
								[TT]
							else if (f1s = SimplTT || f2s = SimplTT) then
								[finish [((if f2s = SimplTT then f1 else f2), forb)] (Tuple ((a, b, c), dba_delta d (Pdlthreadnba.Branch (f2s = SimplTT, f))))]
							else
								let d1 = dba_delta d (Pdlthreadnba.Branch (true, f)) in
								let d2 = dba_delta d (Pdlthreadnba.Branch (false, f)) in
								[finish [(f1, forb)] (Tuple ((a, b, c), d1)); finish [(f2, forb)] (Tuple ((a, b, c), d2))]
						)
						else
							let d1 = dba_delta d (Pdlthreadnba.Branch (true, f)) in
							let d2 = dba_delta d (Pdlthreadnba.Branch (false, f)) in
							[finish [(f1, forb)] (Tuple ((a, b, c), d1)); finish [(f2, forb)] (Tuple ((a, b, c), d2))]
				|	FIntModality (kind, p, g) -> (
						match prgs.(p) with
							FIntConcat _ ->
								let d = dba_delta d (Pdlthreadnba.Follow f) in
								[finish [(lnks.(f).(0), forb)] (Tuple ((a, b, c), d))]
						|	FIntChoice _ ->
								if kind then
									let d = dba_delta d (Pdlthreadnba.Follow f) in
									[finish [(lnks.(f).(0), forb); (lnks.(f).(1), forb)] (Tuple ((a, b, c), d))]
								else
									let d1 = dba_delta d (Pdlthreadnba.Branch (true, f)) in
									let d2 = dba_delta d (Pdlthreadnba.Branch (false, f)) in
									[finish [(lnks.(f).(0), forb)] (Tuple ((a, b, c), d1));
									 finish [(lnks.(f).(1), forb)] (Tuple ((a, b, c), d2))]
						|	FIntStar p' ->
								let forb' = TreeSet.add f forb in
								if kind then
									let d = dba_delta d (Pdlthreadnba.Follow f) in
									[finish [(g, forb'); (lnks.(f).(0), forb')] (Tuple ((a, b, c), d))]
								else
									let d1 = dba_delta d (Pdlthreadnba.Branch (true, f)) in
									let d2 = dba_delta d (Pdlthreadnba.Branch (false, f)) in
									[finish [(g, forb')] (Tuple ((a, b, c), d1));
									 finish [(lnks.(f).(0), forb')] (Tuple ((a, b, c), d2))]
						|	FIntQuestion h ->
								if kind then
									let d1 = dba_delta d (Pdlthreadnba.Branch (true, f)) in
									let d2 = dba_delta d (Pdlthreadnba.Branch (false, f)) in
									[finish [(h, forb)] (Tuple ((a, b, c), d1));
									 finish [(g, forb)] (Tuple ((a, b, c), d2))]								
								else
									let d = dba_delta d (Pdlthreadnba.Follow f) in
									[finish [(lnks.(f).(0), forb); (g, forb)] (Tuple ((a, b, c), d))]
						|	_ -> failwith "Pdlvaliditygame.delta: Failure 0!"
					)
				|	_ -> failwith "Pdlvaliditygame.delta: Failure 1!"
			)
		|	[] ->
			if b = []
			then failwith "Pdlvaliditygame.delta: Failure 2!"
			else ( 
				let b = List.map (fun f ->
					match fmls.(f) with
						(FIntModality (x,y,z)) -> (f, (x,y,z))
					|	_ -> failwith "Pdlvaliditygame.delta: Failure 3!"
				) b in
				let (diamonds, boxes) = List.partition (fun (_, (b, _, _)) -> b) b in
				if boxes = [] then [NT c]
				else (
					let l = ref 0 in
					let mp = ref TreeMap.empty_def in
					List.iter (fun (_, (_, lab, _)) ->
						if not (TreeMap.mem lab !mp) then (
							mp := TreeMap.add lab !l !mp;
							incr l
						)
					) boxes;
					let part = Array.make !l ([], [], -1) in
					List.iter (fun (f, (b, lab, g)) ->
						try
							let i = TreeMap.find lab !mp in
							let (dia, box, _) = part.(i) in
							part.(i) <- if b then (g::dia, box, lab) else (dia, (f, g)::box, lab)
						with Not_found -> ()
					) b;
					let ret = ref [] in
					Array.iter (fun (dia, box, lab) ->
						List.iter (fun (f, g) ->
							let d = dba_delta d (Pdlthreadnba.Next f) in
							let s = finish (List.map (fun x -> (x, TreeSet.empty_def)) (g::dia)) (Tuple (([], [], []), d)) in
							ret := s::!ret
						) box
					) part;
					!ret
				)
			)
	)
	|	s -> [s]
	in
	

	let omega = function
		TT -> 0
	|	NT _ -> 1
	|	Tuple (_, q) -> if dba_final q then 1 else 0
	in
	
	let format_state = function
		TT -> "TT"
	|	NT s -> "NT (" ^ ListUtils.format format_formula s ^ ")"
    |   Tuple ((a, b, c), f) -> "Tuple (" ^ ListUtils.format format_formula ((List.map fst a)@b@c) ^ ", " ^ dba_state_format f ^ ")"
	in
	
	let comp' cmp s1 s2 = match (s1, s2) with
		(Tuple (a, x), Tuple (b, y)) -> (
			let c = compare a b in
			if c = 0 then cmp x y else c
		)
	|	_ -> compare s1 s2
	in
		
	let comp = Some (comp' state_cmp) in

	((initial_state, delta, omega, exist_pos, format_state),
	 (None, None, comp))
	 

let extract_counter_model (dec_formula: decomposed_pdl_formula)
                          validity_game strategy int_to_state state_to_int =
	let (_, fmls, _, _, props, labels) = dec_formula in
	let ((init, delta, _, ex, fo), _) = validity_game in

	let props_final i = fst props.(i) in
	let labels_final i = labels.(i) in
	
	let rec finalize i =
		match (fst (int_to_state i)) with
			TT -> i
		|	NT _ -> i
		|	Tuple (([], _, _), _) -> i
		|	_ -> finalize (if ex i then List.hd (delta i) else OptionUtils.get_some (strategy i))
	in
				 
	let init_final = finalize init in
	
	let get_props_final i =
		let convert l =
			List.filter (fun i -> i >= 0)
				(List.map (fun f ->
					match fmls.(f) with
						FIntProp (false, p) -> p
					|	_ -> -1
				) l)
		in
		match (fst (int_to_state i)) with
			TT -> []
		|	NT l -> convert l
		|	Tuple ((_, _, l), _) -> convert l
	in
			 
	let get_delta_final i =
		match (int_to_state i) with
			(Tuple (([], b, _), q), r) -> (
				let boxes = ref [] in
				let diamonds = ref [] in
				List.iter (fun f ->
					match fmls.(f) with
						(FIntModality (b,l,_)) -> (
							if b then diamonds := f::!diamonds
							else boxes := (f,l)::!boxes
						)
					|	_ -> failwith "Impossible."
				) b;
				List.map (fun (box, lab) ->
					(lab, finalize (List.hd (delta (state_to_int (Tuple (([], box::!diamonds, []), q), r)))))
 			    ) !boxes
			)
		|	_ -> []
	in
		
	let get_annot_final i = fo i in
	
	let compare_final i j = compare i j in
	
	((init_final, get_props_final, get_delta_final, get_annot_final),
	 (Some compare_final, labels_final, props_final))


let compare_formula (_, _, _, prgs, _, _) f g =
	match (f, g) with
		(FIntBranch (true, _, _), FIntBranch (false, _, _)) -> -1
	|	(FIntBranch (false, _, _), FIntBranch (true, _, _)) -> 1

	|	(FIntBranch (true, _, _), FIntModality _) -> -1
	|	(FIntModality _, FIntBranch (true, _, _)) -> 1

	|	(FIntBranch (false, _, _), FIntModality _) -> 1
	|	(FIntModality _, FIntBranch (false, _, _)) -> -1
	
	|	(FIntModality (kindf, pf, _), FIntModality (kindg, pg, _)) -> (
			match ((kindf, prgs.(pf)), (kindg, prgs.(pg))) with
				((true, FIntQuestion _), (false, _)) -> -1
			|	((false, _), (true, FIntQuestion _)) -> 1

			|	((true, FIntQuestion _), (true, FIntQuestion _)) -> compare f g
			
			|	((true, FIntQuestion _), (true, _)) -> -1
			|	((true, _), (true, FIntQuestion _)) -> 1
			
			|	((false, _), (true, _)) -> -1
			|	((true, _), (false, _)) -> 1
			
			|	_ -> compare f g
		)
	|	_ -> compare f g;;


let validity_proc formula options (info_chan, formula_chan, constr_chan) =
	let proof_game_in_annot = ArrayUtils.mem "ann" options in
	let compact_game = ArrayUtils.mem "comp" options in
	let use_literal_propagation = ArrayUtils.mem "litpro" options in
	let length_sort =
		if ArrayUtils.mem "prefersmall" options then 1
		else if ArrayUtils.mem "preferlarge" options then -1
		else 0
	in
	let info_msg = MessageChannel.send_message info_chan in
	let formula_msg = MessageChannel.send_message formula_chan in
	let constr_msg = MessageChannel.send_message constr_chan in
	
	info_msg (fun _ -> "Decision Procedure For PDL\n");
	
	formula_msg (fun _ -> "Transforming given formula...");
    let t = SimpleTiming.init true in
	let formula' = eval_metaformula formula in
    let goalformula = formula_to_positive formula' in
	formula_msg (fun _ -> SimpleTiming.format t ^ "\n");
	formula_msg (fun _ -> "Transformed formula: " ^ format_formula goalformula ^ "\n");
	formula_msg (fun _ -> "Formula Length: " ^ string_of_int (formula_length goalformula) ^ "\n");
	
	constr_msg (fun _ -> "Decompose formula...");
    let t = SimpleTiming.init true in
    let decformula = normal_form_formula_to_decomposed_formula goalformula pdl_formula_link_map in
	let decformula = 
		if length_sort = 0
		then sort_decomposed_formula decformula compare_formula
		else sort_decomposed_formula decformula (fun d a b -> length_sort * compare (get_formula_depth d a) (get_formula_depth d b))
	in
	constr_msg (fun _ -> SimpleTiming.format t ^ "\n");

	constr_msg (fun _ -> "Initializing automata...");
    let t = SimpleTiming.init true in


	let timing_list = ref [] in
	let new_timing s =
		let t = SimpleTiming.init false in
		timing_list := (s, t)::!timing_list;
		t
	in
	
	let listening = MessageChannel.channel_is_listening constr_chan in

	let ncoba_without_timing = pdl_thread_nba decformula in
	let ncoba = if listening then NMATiming.full_timing ncoba_without_timing (new_timing "ncoba_timing") else ncoba_without_timing in

	let ncoba_state_cache = NMAStateCache.make2 ncoba in
	let ncoba_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 ncoba_state_cache) in
	let ncoba_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton ncoba_delta_cache) in
	let ncoba_cached_without_timing = NMAAcceptCache.automaton ncoba_accept_cache in		
	let ncoba_cached = if listening then NMATiming.full_timing ncoba_cached_without_timing (new_timing "ncoba_cached_timing") else ncoba_cached_without_timing in
	
	let dba_without_timing = NcoBAtoComplementDBA.transform ncoba_cached in
	let dba = if listening then DMATiming.full_timing dba_without_timing (new_timing "dba_timing") else dba_without_timing in

	let dba_state_cache = DMAStateCache.make2 dba in
	let dba_delta_cache = DMADeltaCache.make (DMAStateCache.automaton2 dba_state_cache) in
	let dba_accept_cache = DMAAcceptCache.make (DMADeltaCache.automaton dba_delta_cache) in
	let dba_cached_without_timing = DMAAcceptCache.automaton dba_accept_cache in
	let dba_cached = if listening then DMATiming.full_timing dba_cached_without_timing (new_timing "dba_cached_timing") else dba_cached_without_timing in

	let states_ncoba _ = NMAStateCache.state_size2 ncoba_state_cache in
	let transitions_ncoba _ = NMADeltaCache.edge_size ncoba_delta_cache in
	let states_dba _ = DMAStateCache.state_size2 dba_state_cache in
	let transitions_dba _ = DMADeltaCache.edge_size dba_delta_cache in
	let states_game = ref 0 in
	let info_list = [("ncoba_states", states_ncoba); ("ncoba_transitions", transitions_ncoba);
				     ("dba_states", states_dba); ("dba_transitions", transitions_dba);
				     ("game_states", fun () -> !states_game)] in	

	let game =
		let temp = get_validity_game decformula dba_cached use_literal_propagation in
		let temp = if compact_game then get_compact_initpg_by_player temp false else get_escaped_initpg temp 0 in
		if listening then get_timed_initpg temp (new_timing "game_timing") else temp
	in
	let (game_cached, state_to_int, int_to_state) = (
		let (temp, state_to_int, int_to_state) = get_int_cached_initpg game (fun _ i -> states_game := i) in
		((if listening then get_timed_initpg temp (new_timing "game_cached_timing") else temp),
		 state_to_int, int_to_state)
	)
	in
  	constr_msg (fun _ -> SimpleTiming.format t ^ "\n");
	
    let ((init, b, c, d, fo), e) = game_cached in

	let fo' = if proof_game_in_annot
	          then (fun s -> fo s)
			  else (fun _ -> "")
	in

	let game_cached' = ((init, b, c, d, fo'), e) in

	let show_stats _ =
		if listening then (
			List.iter (fun (s, v) ->
				constr_msg (fun _ -> s ^ ": " ^ string_of_int (v ()) ^ "\n")
			) info_list;
			List.iter (fun (s, t) ->
				constr_msg (fun _ -> s ^ ": " ^ (SimpleTiming.format t) ^ "\n")
			) !timing_list
		);
		info_msg (fun _ -> "Game has " ^ string_of_int !states_game ^ " states (NcoBA " ^ string_of_int (states_ncoba ()) ^ " , DBA " ^ string_of_int (states_dba ()) ^ ").\n")
	in
	
	let counter_mod strategy printer =
		info_msg (fun _ -> "Extracting Transition System.\n");
		constr_msg (fun _ -> "Building Transition System...\n");
		let t = SimpleTiming.init true in
		let lts = extract_counter_model decformula game_cached strategy int_to_state state_to_int in
		let ((_, _, _, fo), _) = lts in
		let states_lts = ref 0 in
		let (lts_cached, lts_state_to_int, lts_int_to_state) = 
			get_int_cached_initlts lts (fun _ i -> states_lts := i) in
		let fmt = if proof_game_in_annot
				  then (fun s -> Some (fo s))
				  else (fun _ -> None)
		in
		let explicit_lts = build_explicit_initlts lts_cached lts_int_to_state fmt (fun i ->
			if (i mod 1000 = 0)
			then constr_msg (fun _ -> "\rBuilding..." ^ string_of_int i)
		) in
		constr_msg (fun _ -> "\rBuilding... finished in " ^ SimpleTiming.format t ^ "\n\n");
		let (_, (_, _, graph)) = explicit_lts in
		info_msg (fun _ -> "Transition System has " ^ string_of_int (Array.length graph) ^ " states.\n");
		print_explicit_initlts explicit_lts printer
	in
	
   (game_cached', show_stats, (fun sol strat -> if sol init = Some true then FormulaValid else FormulaFalsifiableBy (counter_mod strat)));;
	
Validitygames.register_validity_procedure validity_proc "pdl" "Decision Procedure For PDL"
