open Tcsautomata;;
open Tcsautohelper;;
open Tcsautotransform;;
open Tcstransitionsys;;
open Tcsgames;;
open Tcsset;;
open Tcslist;;
open Tcsarray;;
open Tcstiming;;
open Tcsmessage;;
open Tcsbasedata;;
open Tcsctlstarformula;;
open Ctlstartracenba;;
open Ctlstarthreadnba;;
open Ctlstartracenba2;;
open Ctlplustracenba;;
open Validitygamesregistry;;

type 'a state =
    TT
  | NT of int list
  | Tuple of (block list *            (* blocks *)
			  (int list)) *           (* propositions *)
			  'a;;                    (* automaton *)

type 'a validity_game = 'a state initpg;;

type simplification_result = SimplTT | SimplFF | SimplKeep;;

let rec simplify ((_, fmls, _, prop) as fml) props i =
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

	
let get_validity_game (((r, fmls, lnks, props) as fml): decomposed_ctlstar_formula)
                       dpa
					   use_literal_propagation =

	let dpa_initial_state = DMA.initial dpa in
	let dpa_delta x y = DMA.delta dpa x y in
	let dpa_omega = DMA.accept dpa in
	let state_cmp = Domain.compare (DMA.states dpa) in
	(* let dpa_state_format = Domain.format (DMA.states dpa) in *)

	let format_formula = format_decomposed_formula fml in

	let initial_state =	Tuple (([(EBlock, [r], [])], []), dpa_initial_state) in

	let exist_pos = function
		Tuple ((blocks, _), _) -> (blocks <> []) && (let (_, q, r) = List.hd blocks in q = [] && r <> [])
	|	_ -> true
	in

	let finish new_props = function
		TT -> TT
	|	NT s -> NT s
	|	Tuple ((bl, pr), d) -> (
			let pr = ref (TreeSet.of_list_def pr) in
			let is_true = ref false in
			List.iter (fun f ->
				match fmls.(f) with
					FIntProp (b, p) -> (
						let f' = (if b then snd else fst) (snd props.(p)) in
						if TreeSet.mem f' !pr
						then is_true := true
						else pr := TreeSet.add f !pr
					)
				|	_ -> failwith "impossible (game): 1"
			) new_props;
			if !is_true then TT
			else if bl = [] then NT (TreeSet.elements !pr)
			else Tuple ((bl, TreeSet.elements !pr), d)
		)
	in


	let compare_block (t1, q1, m1) (t2, q2, m2) =
		let q1l = List.length q1 in
		let q2l = List.length q2 in
		let m1l = List.length m1 in
		let m2l = List.length m2 in
		let l1 = q1l + m1l in
		let l2 = q2l + m2l in
		if (l1 != l2) && (l1 = 0 || l2 = 0) then
			compare l1 l2
		else if (q1l != q2l) && (q1l = 0 || q2l = 0) then
			compare q2l q1l
		else let c = compare q1 q2 in
		     if c != 0 then c
		     else if t1 != t2 then compare t1 t2
		          else compare m1 m2
	in
	
	let add_blocks blcks lst = TreeSet.elements (TreeSet.of_list compare_block (blcks@lst)) in
	let add_to_block = add_to_block fml in

	let rec delta = function
		Tuple ((blks, prps), d) -> (
			match blks with
				(kind, f::rf, m)::rb -> (
					match fmls.(f) with
					    FIntAtom b ->
					    	if kind = ABlock then
					    		if b then [TT]
					    		else [Tuple ((add_blocks [(kind, rf, m)] rb, prps),
					    		             dpa_delta d (Delete ((kind, f::rf, m), f)))]
					    	else
					    		if b then [Tuple ((add_blocks [(kind, rf, m)] rb, prps),
					    		                  dpa_delta d (Delete ((kind, f::rf, m), f)))]
					    		else [Tuple ((rb, prps),
					    		             dpa_delta d (DeleteBlock (kind, f::rf, m)))]
					|	FIntProp _ ->
							if kind = ABlock then
								[finish [f] (Tuple ((add_blocks [(kind, rf, m)] rb, prps),
								                  dpa_delta d (Delete ((kind, f::rf, m), f))))]
							else
								[finish [f] (Tuple ((rb, prps),
								                  dpa_delta d (DeleteBlock (kind, f::rf, m))));
								 Tuple ((add_blocks [(kind, rf, m)] rb, prps),
								        dpa_delta d (Delete ((kind, f::rf, m), f)))]
					|	FIntBranch (br, le, ri) ->
							if kind = ABlock then (
								if br then
									if use_literal_propagation then (                (* ABlock Or: Checked. *)
										let propsset = TreeSet.of_list_def prps in
										let les = simplify fml propsset le in
										let ris = simplify fml propsset ri in
										if (les = SimplFF && ris = SimplFF) then
											[Tuple ((add_blocks [(kind, rf, m)] rb, prps),
													dpa_delta d (Delete ((kind, f::rf, m), f)))]
										else if (les = SimplTT || ris = SimplTT) then
											[TT]
										else if (les = SimplFF || ris = SimplFF) then
											let d = dpa_delta d (Follow ((kind, f::rf, m), f) )in
											let bl_temp = add_to_block (kind, rf, m) [le;ri] in
											let d = dpa_delta d (Delete (bl_temp, if les = SimplFF then le else ri)) in
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [if ris = SimplFF then le else ri]] rb, prps),
													d)]
										else
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [le;ri]] rb, prps),
													 dpa_delta d (Follow ((kind, f::rf, m), f)))]
									)
									else
										[Tuple ((add_blocks [add_to_block (kind, rf, m) [le;ri]] rb, prps),
												 dpa_delta d (Follow ((kind, f::rf, m), f)))]
								 else
									if use_literal_propagation then (              (* ABlock And: Checked. *)
										let propsset = TreeSet.of_list_def prps in
										let les = simplify fml propsset le in
										let ris = simplify fml propsset ri in
										if (les = SimplFF || ris = SimplFF) then
											[Tuple ((add_blocks [(kind, rf, m)] rb, prps),
													dpa_delta d (Delete ((kind, f::rf, m), f)))]
										else if (les = SimplTT && ris = SimplTT) then
											[TT]
										else if (les = SimplTT || ris = SimplTT) then
											let d = dpa_delta d (Branch ((kind, f::rf, m), f, ris = SimplTT) )in
											let bl_temp = add_to_block (kind, rf, m) [if les = SimplTT then le else ri] in
											let d = dpa_delta d (DeleteBlock bl_temp) in
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [if les = SimplTT then ri else le]] rb, prps),
													d)]
										else
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [le]] rb, prps),
													 dpa_delta d (Branch ((kind, f::rf, m), f, true)));
											 Tuple ((add_blocks [add_to_block (kind, rf, m) [ri]] rb, prps),
													 dpa_delta d (Branch ((kind, f::rf, m), f, false)))]
									)
									else
										[Tuple ((add_blocks [add_to_block (kind, rf, m) [le]] rb, prps),
												 dpa_delta d (Branch ((kind, f::rf, m), f, true)));
										 Tuple ((add_blocks [add_to_block (kind, rf, m) [ri]] rb, prps),
												 dpa_delta d (Branch ((kind, f::rf, m), f, false)))]
							)
							else (
								if br then
									if use_literal_propagation then (        (* EBlock Or: Checked. *)
										let propsset = TreeSet.of_list_def prps in
										let les = simplify fml propsset le in
										let ris = simplify fml propsset ri in
										if (les = SimplFF && ris = SimplFF) then
											[Tuple ((rb, prps), dpa_delta d (DeleteBlock (kind, f::rf, m)))]
										else if (les = SimplTT || ris = SimplTT) then
											[Tuple ((add_blocks [(kind, rf, m)] rb, prps),
					    		                  dpa_delta d (Delete ((kind, f::rf, m), f)))]
										else if (les = SimplFF || ris = SimplFF) then
											let d = dpa_delta d (Follow ((kind, f::rf, m), f) )in
											let bl_good = add_to_block (kind, rf, m) [if les = SimplFF then ri else le] in
											let bl_bad = add_to_block (kind, rf, m) [if ris = SimplFF then ri else le] in
											let d = dpa_delta d (DeleteBlock bl_bad) in
											[ Tuple ((add_blocks [bl_good] rb, prps), d)]
										else
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [le]; add_to_block (kind, rf, m) [ri]] rb, prps),
													 dpa_delta d (Follow ((kind, f::rf, m), f)))]
									)
									else
										[Tuple ((add_blocks [add_to_block (kind, rf, m) [le]; add_to_block (kind, rf, m) [ri]] rb, prps),
												 dpa_delta d (Follow ((kind, f::rf, m), f)))]
								else
									if use_literal_propagation then (         (* EBlock And: Checked. *)
										let propsset = TreeSet.of_list_def prps in
										let les = simplify fml propsset le in
										let ris = simplify fml propsset ri in
										if (les = SimplFF || ris = SimplFF) then
											[Tuple ((rb, prps),
					    		             dpa_delta d (DeleteBlock (kind, f::rf, m)))]
										else if (les = SimplTT && ris = SimplTT) then
											[Tuple ((add_blocks [(kind, rf, m)] rb, prps),
					    		                  dpa_delta d (Delete ((kind, f::rf, m), f)))]
										else if (les = SimplTT || ris = SimplTT) then
											let d = dpa_delta d (Follow ((kind, f::rf, m), f) )in
											let bl_temp = add_to_block (kind, rf, m) [le;ri] in
											let d = dpa_delta d (Delete (bl_temp, if les = SimplTT then le else ri)) in
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [if ris = SimplTT then le else ri]] rb, prps),
													d)]
										else
											[Tuple ((add_blocks [add_to_block (kind, rf, m) [le;ri]] rb, prps),
													 dpa_delta d (Follow ((kind, f::rf, m), f)))]
									)
									else
										[Tuple ((add_blocks [add_to_block (kind, rf, m) [le;ri]] rb, prps),
												 dpa_delta d (Follow ((kind, f::rf, m), f)))]
							)
							(*
							if (kind = ABlock && br) || (kind = EBlock && (not br)) then
								[Tuple ((add_blocks [add_to_block (kind, rf, m) [le;ri]] rb, prps),
								         dpa_delta d (Follow ((kind, f::rf, m), f)))]
							else if kind = EBlock then
								[Tuple ((add_blocks [add_to_block (kind, rf, m) [le]; add_to_block (kind, rf, m) [ri]] rb, prps),
								         dpa_delta d (Follow ((kind, f::rf, m), f)))]
							else
								[Tuple ((add_blocks [add_to_block (kind, rf, m) [le]] rb, prps),
								         dpa_delta d (Branch ((kind, f::rf, m), f, true)));
								 Tuple ((add_blocks [add_to_block (kind, rf, m) [ri]] rb, prps),
								         dpa_delta d (Branch ((kind, f::rf, m), f, false)))]
							*)
					|	FIntPath (pathkind, g) ->
							if kind = ABlock then
								[Tuple ((add_blocks [add_to_block ((if pathkind then EBlock else ABlock), [], []) [g]; (kind, rf, m)] rb, prps),
								        dpa_delta d (Follow ((kind, f::rf, m), f)))]
							else
								[Tuple ((add_blocks [add_to_block ((if pathkind then EBlock else ABlock), [], []) [g]] rb, prps),
								         dpa_delta d (Branch ((kind, f::rf, m), f, true)));
								 Tuple ((add_blocks [(kind, rf, m)] rb, prps),
								         dpa_delta d (Branch ((kind, f::rf, m), f, false)))]
					|	FIntFixpoint (fixkind, le, ri) ->
							if kind = EBlock then
								[Tuple ((add_blocks [add_to_block (kind, rf, m) (if fixkind then [ri] else [le;ri]);
								                     add_to_block (kind, rf, m) (if fixkind then [le;lnks.(f).(0)] else [ri;lnks.(f).(0)])] rb, prps),
								        dpa_delta d (Follow ((kind, f::rf, m), f)))]
							else
								[Tuple ((add_blocks [add_to_block (kind, rf, m) (if fixkind then [le;ri] else [ri])] rb, prps),
								        dpa_delta d (Branch ((kind, f::rf, m), f, true)));
								 Tuple ((add_blocks [add_to_block (kind, rf, m) (if fixkind then [ri;lnks.(f).(0)] else [le;lnks.(f).(0)])] rb, prps),
								        dpa_delta d (Branch ((kind, f::rf, m), f, false)))]
					|	FIntNext _ -> failwith "impossible (game): 2"
				)
			|	(EBlock, [], [])::_ -> [TT]
			|	(ABlock, [], [])::rb -> [Tuple ((rb, prps), dpa_delta d (DeleteBlock (ABlock, [], [])))]
			|	(_, [], _)::_ -> (
					let (eblocks, ablocks) = List.partition (fun (kind, _, _) -> kind = EBlock) blks in
					let map_block (kind, _, l) = add_to_block (kind, [], []) l in
					let eblocks' = TreeSet.of_list compare_block (List.map map_block eblocks) in
					if ablocks = [] then [Tuple ((TreeSet.elements eblocks', []), dpa_delta d Next0)]
					else List.map (fun abl ->
						Tuple ((TreeSet.elements (TreeSet.add (map_block abl) eblocks'), []), dpa_delta d (Next1 abl))
					) ablocks
				)
			|	_ -> [NT prps]
		)
	|	s -> [s]
	in

	let omega = function
		TT -> 0
	|	NT _ -> 1
	|	Tuple (_, q) -> dpa_omega q
	in

	let format_state = function
		TT -> "TT"
	|	NT s -> "NT (" ^ ListUtils.format format_formula s ^ ")"
    |   Tuple ((bl, pr), s) -> "Tuple (" ^ ListUtils.format (format_block fml) bl ^ ", " ^ ListUtils.format format_formula pr ^ (*", " ^ dpa_state_format s ^*) ")"
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
	 (None, None, comp));;


let extract_counter_model (dec_formula: decomposed_ctlstar_formula)
                          validity_game strategy int_to_state state_to_int =
	let (_, fmls, _, props) = dec_formula in
	let ((init, delta, _, ex, fo), _) = validity_game in

	let props_final i = fst props.(i) in

	let rec finalize i =
		match (fst (int_to_state i)) with
			TT -> i
		|	NT _ -> i
		|	Tuple _ ->
				if ex i then i else finalize (OptionUtils.get_some (strategy i))
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
		|	Tuple ((_, l), _) -> convert l
	in

	let get_delta_final i =
		match (fst (int_to_state i)) with
			Tuple _ ->
				List.map finalize (delta i)
		|	_ -> []
	in

	let get_annot_final i = fo i in

	let compare_final i j = compare i j in

	((init_final, get_props_final, get_delta_final, get_annot_final),
	 (Some compare_final, props_final))


let validity_proc formula options (info_chan, formula_chan, constr_chan) =
	let proof_game_in_annot = ArrayUtils.mem "ann" options in
	let compact_game = ArrayUtils.mem "comp" options in
	let use_literal_propagation = ArrayUtils.mem "litpro" options in
	let use_ctlplus = ArrayUtils.mem "ctlplus" options in
	let length_sort =
		if ArrayUtils.mem "prefersmall" options then 1
		else if ArrayUtils.mem "preferlarge" options then -1
		else 0
	in
	let use_tracenba2 = ArrayUtils.mem "nba2" options in
	let info_msg = MessageChannel.send_message info_chan in
	let formula_msg = MessageChannel.send_message formula_chan in
	let constr_msg = MessageChannel.send_message constr_chan in

	info_msg (fun _ -> "Decision Procedure For CTL*\n");

	formula_msg (fun _ -> "Transforming given formula...");
    let t = SimpleTiming.init true in
	let formula' = eval_metaformula formula in
    let goalformula = formula_to_positive (FExists formula') in
	formula_msg (fun _ -> SimpleTiming.format t ^ "\n");
	formula_msg (fun _ -> "Transformed formula: " ^ format_formula goalformula ^ "\n");
	formula_msg (fun _ -> "Formula Length: " ^ string_of_int (formula_length goalformula) ^ "\n");

	constr_msg (fun _ -> "Decompose formula...");
    let t = SimpleTiming.init true in
    let decformula = normal_form_formula_to_decomposed_formula goalformula ctlstar_formula_link_map in

	let decformula = 
		if length_sort = 0
		then decformula
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
	let info_list = ref [] in
	let new_info s f =
		info_list := (s, f)::!info_list
	in

	let listening = MessageChannel.channel_is_listening constr_chan in

	let dpa_cached =
	
		if use_ctlplus then (
			let nba_without_timing = ctlplus_trace_nba decformula in
			let nba = if listening then NMATiming.full_timing nba_without_timing (new_timing "nba_timing") else nba_without_timing in
		
			let nba_state_cache = NMAStateCache.make2 nba in
			let nba_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_state_cache) in
			let nba_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_delta_cache) in
			let nba_cached_without_timing = NMAAcceptCache.automaton nba_accept_cache in		
			let nba_cached = if listening then NMATiming.full_timing nba_cached_without_timing (new_timing "nba_cached_timing") else nba_cached_without_timing in
			new_info "nba_states" (fun _ -> NMAStateCache.state_size2 nba_state_cache);
			new_info "nba_transitions" (fun _ -> NMADeltaCache.edge_size nba_delta_cache);
			
			let dba_without_timing = NcoBAtoComplementDBA.transform nba_cached in
			let dba = if listening then DMATiming.full_timing dba_without_timing (new_timing "dba_timing") else dba_without_timing in
		
			let dba_state_cache = DMAStateCache.make2 dba in
			let dba_delta_cache = DMADeltaCache.make (DMAStateCache.automaton2 dba_state_cache) in
			let dba_accept_cache = DMAAcceptCache.make (DMADeltaCache.automaton dba_delta_cache) in
			let dba_cached_without_timing = DMAAcceptCache.automaton dba_accept_cache in
			let dba_cached = if listening then DMATiming.full_timing dba_cached_without_timing (new_timing "dba_cached_timing") else dba_cached_without_timing in
			new_info "dba_states" (fun _ -> DMAStateCache.state_size2 dba_state_cache);
			new_info "dba_transitions" (fun _ -> DMADeltaCache.edge_size dba_delta_cache);
			
			let dpa = DBA.asDPA2 dba_cached 1 0 in
						
			dpa
		)
		else 
		(
			let (nba_cached, nba_size) =
				if use_tracenba2 then (
					let nba_without_timing = ctlstar_trace_nba2 decformula in
					let nba = if listening then NMATiming.full_timing nba_without_timing (new_timing "nba_timing") else nba_without_timing in
				
					let nba_state_cache = NMAStateCache.make2 nba in
					let nba_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_state_cache) in
					let nba_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_delta_cache) in
					let nba_cached_without_timing = NMAAcceptCache.automaton nba_accept_cache in		
					let nba_cached = if listening then NMATiming.full_timing nba_cached_without_timing (new_timing "nba_cached_timing") else nba_cached_without_timing in
					new_info "nba_states" (fun _ -> NMAStateCache.state_size2 nba_state_cache);
					new_info "nba_transitions" (fun _ -> NMADeltaCache.edge_size nba_delta_cache);

					(nba_cached, ctlstar_trace_nba2_state_size decformula)
				)
				else (
					let nba_a_without_timing = ctlstar_thread_nba_a_block decformula in
					let nba_a = if listening then NMATiming.full_timing nba_a_without_timing (new_timing "nba_a_timing") else nba_a_without_timing in
				
					let nba_a_state_cache = NMAStateCache.make2 nba_a in
					let nba_a_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_a_state_cache) in
					let nba_a_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_a_delta_cache) in
					let nba_a_cached_without_timing = NMAAcceptCache.automaton nba_a_accept_cache in		
					let nba_a_cached = if listening then NMATiming.full_timing nba_a_cached_without_timing (new_timing "nba_a_cached_timing") else nba_a_cached_without_timing in
					new_info "nba_a_states" (fun _ -> NMAStateCache.state_size2 nba_a_state_cache);
					new_info "nba_a_transitions" (fun _ -> NMADeltaCache.edge_size nba_a_delta_cache);

					let nba_e_without_timing = ctlstar_thread_nba_e_block decformula in
					let nba_e = if listening then NMATiming.full_timing nba_e_without_timing (new_timing "nba_e_timing") else nba_e_without_timing in
				
					let nba_e_state_cache = NMAStateCache.make2 nba_e in
					let nba_e_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_e_state_cache) in
					let nba_e_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_e_delta_cache) in
					let nba_e_cached_without_timing = NMAAcceptCache.automaton nba_e_accept_cache in		
					let nba_e_cached = if listening then NMATiming.full_timing nba_e_cached_without_timing (new_timing "nba_e_cached_timing") else nba_e_cached_without_timing in
					new_info "nba_e_states" (fun _ -> NMAStateCache.state_size2 nba_e_state_cache);
					new_info "nba_e_transitions" (fun _ -> NMADeltaCache.edge_size nba_e_delta_cache);

					let dba_e_without_timing = NcoBAtoComplementDBA.transform nba_e_cached in
					let dba_e = if listening then DMATiming.full_timing dba_e_without_timing (new_timing "dba_e_timing") else dba_e_without_timing in
				
					let dba_e_state_cache = DMAStateCache.make2 dba_e in
					let dba_e_delta_cache = DMADeltaCache.make (DMAStateCache.automaton2 dba_e_state_cache) in
					let dba_e_accept_cache = DMAAcceptCache.make (DMADeltaCache.automaton dba_e_delta_cache) in
					let dba_e_cached_without_timing = DMAAcceptCache.automaton dba_e_accept_cache in
					let dba_e_cached = if listening then DMATiming.full_timing dba_e_cached_without_timing (new_timing "dba_e_cached_timing") else dba_e_cached_without_timing in
					new_info "dba_e_states" (fun _ -> DMAStateCache.state_size2 dba_e_state_cache);
					new_info "dba_e_transitions" (fun _ -> DMADeltaCache.edge_size dba_e_delta_cache);

					let nba_without_timing = ctlstar_trace_nba decformula dba_e_cached nba_a_cached in
					let nba = if listening then NMATiming.full_timing nba_without_timing (new_timing "nba_timing") else nba_without_timing in
				
					let nba_state_cache = NMAStateCache.make2 nba in
					let nba_delta_cache = NMADeltaCache.make (NMAStateCache.automaton2 nba_state_cache) in
					let nba_accept_cache = NMAAcceptCache.make (NMADeltaCache.automaton nba_delta_cache) in
					let nba_cached_without_timing = NMAAcceptCache.automaton nba_accept_cache in		
					let nba_cached = if listening then NMATiming.full_timing nba_cached_without_timing (new_timing "nba_cached_timing") else nba_cached_without_timing in
					new_info "nba_states" (fun _ -> NMAStateCache.state_size2 nba_state_cache);
					new_info "nba_transitions" (fun _ -> NMADeltaCache.edge_size nba_delta_cache);

					(nba_cached, ctlstar_trace_nba_state_size decformula (ctlstar_thread_nba_size decformula) (ctlstar_thread_nba_size decformula))
				)
			in
			
			let dpa_without_timing = NBAtoDPA.transform nba_cached nba_size in
			let dpa = if listening then DMATiming.full_timing dpa_without_timing (new_timing "dpa_timing") else dpa_without_timing in
		
			let dpa_state_cache = DMAStateCache.make2 dpa in
			let dpa_delta_cache = DMADeltaCache.make (DMAStateCache.automaton2 dpa_state_cache) in
			let dpa_accept_cache = DMAAcceptCache.make (DMADeltaCache.automaton dpa_delta_cache) in
			let dpa_cached_without_timing = DMAAcceptCache.automaton dpa_accept_cache in
			let dpa_cached = if listening then DMATiming.full_timing dpa_cached_without_timing (new_timing "dpa_cached_timing") else dpa_cached_without_timing in
			
			new_info "dpa_states" (fun _ -> DMAStateCache.state_size2 dpa_state_cache);
			new_info "dpa_transitions" (fun _ -> DMADeltaCache.edge_size dpa_delta_cache);

			dpa_cached
		)
	in

	let game =
		let temp = get_validity_game decformula dpa_cached use_literal_propagation in
		let temp = if compact_game then get_compact_initpg_by_player temp false else get_escaped_initpg temp 0 in
		if listening then get_timed_initpg temp (new_timing "game_timing") else temp
	in
	let states_game = ref 0 in
	new_info "game_states" (fun _ -> !states_game);
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
		List.iter (fun (s, v) ->
			info_msg (fun _ -> s ^ ": " ^ string_of_int (v ()) ^ "\n")
		) (List.rev !info_list);
		if listening then (
			List.iter (fun (s, t) ->
				constr_msg (fun _ -> s ^ ": " ^ (SimpleTiming.format t) ^ "\n")
			) !timing_list
		);
	in

	let counter_mod strategy printer =
		info_msg (fun _ -> "Extracting Transition System.\n");
		constr_msg (fun _ -> "Building Transition System...\n");
		let t = SimpleTiming.init true in
		let ts = extract_counter_model decformula game_cached strategy int_to_state state_to_int in
		let ((_, _, _, fo), _) = ts in
		let states_ts = ref 0 in
		let (ts_cached, ts_state_to_int, ts_int_to_state) =
			get_int_cached_initts ts (fun _ i -> states_ts := i) in
		let fmt = if proof_game_in_annot
				  then (fun s -> Some (fo s))
				  else (fun _ -> None)
		in
		let explicit_ts = build_explicit_initts ts_cached ts_int_to_state fmt (fun i ->
			if (i mod 1000 = 0)
			then constr_msg (fun _ -> "\rBuilding..." ^ string_of_int i)
		) in
		constr_msg (fun _ -> "\rBuilding... finished in " ^ SimpleTiming.format t ^ "\n\n");
		let (_, (_, graph)) = explicit_ts in
		info_msg (fun _ -> "Transition System has " ^ string_of_int (Array.length graph) ^ " states.\n");
		print_explicit_initts explicit_ts printer
	in

   (game_cached', show_stats, (fun sol strat -> if sol init = Some true then FormulaValid else FormulaFalsifiableBy (counter_mod strat)));;



let register _ =
    register_validity_procedure validity_proc "ctlstar" "Decision Procedure For CTL*"
