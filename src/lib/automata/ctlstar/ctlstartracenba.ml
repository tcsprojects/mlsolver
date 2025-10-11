open Tcsset;;
open Tcslist;;
open Tcsautomata;;
open Tcsbasedata;;
open Tcsctlstarformula;;
open Ctlstarthreadnba;;

type rules = Branch of block * int * bool
           | Follow of block * int
           | Next0
           | Next1 of block
           | Delete of block * int
           | DeleteBlock of block

let ctlstar_trace_nba_format_rule dcp = function
		Branch (bl, f, dir) -> (if dir then "L" else "R") ^ "(" ^ format_block dcp bl ^ "," ^ format_decomposed_formula dcp f ^ ")"
	|	Follow (bl, f) -> "F" ^ "(" ^ format_block dcp bl ^ "," ^ format_decomposed_formula dcp f ^ ")"
	|	Next0 -> "N0"
	|	Next1 bl -> "N1(" ^ format_block dcp bl ^ ")"
	|	Delete (bl, f) -> "D(" ^ format_block dcp bl ^ "," ^ format_decomposed_formula dcp f ^ ")"
	|	DeleteBlock bl -> "D(" ^ format_block dcp bl ^ ")"

type ('a, 'b) ctlstar_trace_nba_state = Failed
                    | Waiting
                    | TrackingE of block * 'a
                    | TrackingA of block * 'b

let ctlstar_trace_nba ((r, frs, lnks, props) as dcp) etracker atracker =

	let e_states = DMA.states etracker in
	let a_states = NMA.states atracker in
	let e_format = Domain.format e_states in
	let a_format = Domain.format a_states in
	let e_compare = Domain.compare e_states in
	let a_compare = Domain.compare a_states in
	let e_initial = DMA.initial etracker in
	let a_initial = NMA.initial atracker in
	let e_delta = DMA.delta etracker in
	let a_delta = fun x y -> Iterators.to_list (NMA.delta atracker x y) in
	let e_final = DMA.accept etracker in
	let a_final = NMA.accept atracker in

	let fmt_state = function
		Failed -> "<FAILED>"
	|	Waiting -> "<WAITING>"
	|	TrackingE (b, e_state) -> "TRACKING_E(" ^ format_block dcp b ^ ", " ^ e_format e_state ^ ")"
	|	TrackingA (b, a_state) -> "TRACKING_A(" ^ format_block dcp b ^ ", " ^ a_format a_state ^ ")"
	in

	let cmp_state x y =
		let idx = function Failed -> 1 | Waiting -> 2 | TrackingE _ -> 3 | TrackingA _ -> 4 in
		let c = compare (idx x) (idx y) in
		if c != 0 then c else (
			match (x, y) with
				(TrackingE (tx,sx), TrackingE (ty,sy)) -> Comparators.product Comparators.default e_compare (tx,sx) (ty,sy)
			|	(TrackingA (tx,sx), TrackingA (ty,sy)) -> Comparators.product Comparators.default a_compare (tx,sx) (ty,sy)
			|	_ -> 0
		)
	in
	
	let fmt_alpha = ctlstar_trace_nba_format_rule dcp in

	let cmp_alpha = Comparators.default in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = Waiting in
	
	let delta s r =
		let add_to_block = add_to_block dcp in
		let result = match s with
			Waiting -> (
				let process path_kind g =
					let bl = add_to_block ((if path_kind then EBlock else ABlock), [], []) [g] in
					if path_kind then
						[Waiting; TrackingE (bl, e_initial)]
					else
						[Waiting; TrackingA (bl, a_initial)]
				in
				match r with
					Branch (_, f, true) -> (
						match frs.(f) with
							FIntPath (path_kind, g) -> process path_kind g
						|	_ -> [Waiting]
					)
				|	Follow (_, f) -> (
						match frs.(f) with
							FIntPath (path_kind, g) -> process path_kind g
						|	_ -> [Waiting]
					)
				|	_ -> [Waiting]
			)
		|	Failed -> [Failed]
		|	TrackingE (((_, nonmods, mods) as bl), e_state) -> (
				match r with
					Follow (bl', f) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match frs.(f) with
								FIntBranch (br, le, ri) ->
									if br then
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le], e_delta e_state (FormulaBranch (f, true)));
										 TrackingE (add_to_block (EBlock, nonmods', mods) [ri], e_delta e_state (FormulaBranch (f, false)))]
									else
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le;ri], e_delta e_state (FormulaFollow f))]
							|	FIntFixpoint (fixkind, le, ri) ->
									if fixkind then
										[TrackingE (add_to_block (EBlock, nonmods', mods) [ri], e_delta e_state (FormulaBranch (f, true)));
										 TrackingE (add_to_block (EBlock, nonmods', mods) [le;lnks.(f).(0)], e_delta e_state (FormulaBranch (f, false)))]
									else
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le;ri], e_delta e_state (FormulaBranch (f, true)));
										 TrackingE (add_to_block (EBlock, nonmods', mods) [ri;lnks.(f).(0)], e_delta e_state (FormulaBranch (f, false)))]
							|	_ -> failwith "impossible (trace) 1"
						)
				|	Branch (bl', f, dir) ->
						if bl' <> bl then [s] else (
							match frs.(f) with
								FIntPath (pathkind, z) ->
									if dir then [Failed]
									else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
										 [TrackingE ((EBlock, nonmods', mods), e_delta e_state (FormulaDelete f))]
							|	_ -> failwith "impossible (trace) 2"
						)
				|	Delete (bl', f) ->
						if bl' <> bl then [s]
						else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							 [TrackingE ((EBlock, nonmods', mods), e_delta e_state (FormulaDelete f))]
				|	Next0 ->
						[TrackingE (add_to_block (EBlock, [], []) mods, e_delta e_state FormulaNext)]
				|	Next1 _ ->
						[TrackingE (add_to_block (EBlock, [], []) mods, e_delta e_state FormulaNext)]
				|	DeleteBlock bl' ->
						if bl' <> bl then [s] else [Failed]
			)
		|	TrackingA (((_, nonmods, mods) as bl), a_state) -> (
				match r with
					Follow (bl', f) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match frs.(f) with
								FIntBranch (true, le, ri) ->
									List.map (fun q -> TrackingA (add_to_block (ABlock, nonmods', mods) [le;ri], q)) (a_delta a_state (FormulaFollow f))
							|	FIntPath (pathkind, z) ->
									let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
									List.map (fun q -> TrackingA ((ABlock, nonmods', mods), q)) (a_delta a_state (FormulaDelete f))
							|	_ -> failwith "impossible (trace) 3"
						)
				|	Branch (bl', f, dir) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match frs.(f) with
								FIntBranch (false, le, ri) ->
									List.map (fun q -> TrackingA (add_to_block (ABlock, nonmods', mods) [if dir then le else ri], q))
									         (a_delta a_state (FormulaBranch (f, dir)))
							|	FIntFixpoint (fixkind, le, ri) ->
									if dir then
										List.map (fun q -> TrackingA (add_to_block (ABlock, nonmods', mods) (if fixkind then [le;ri] else [ri]), q))
										         (a_delta a_state (FormulaBranch (f, dir)))
									else
										List.map (fun q -> TrackingA (add_to_block (ABlock, nonmods', mods)
										         (if fixkind then [ri;lnks.(f).(0)] else [le;lnks.(f).(0)]), q))
										         (a_delta a_state (FormulaBranch (f, dir)))
							|	_ -> failwith "impossible (trace) 4"
						)
				|	Delete (bl', f) ->
						if bl' <> bl then [s]
						else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							 List.map (fun q -> TrackingA ((ABlock, nonmods', mods), q)) (a_delta a_state (FormulaDelete f))
				|	Next1 bl' ->
						if bl' <> bl then [Failed]
						else List.map (fun q -> TrackingA (add_to_block (ABlock, [], []) mods, q)) (a_delta a_state FormulaNext)
				|	Next0 -> failwith "impossible (trace) 5"
				|	DeleteBlock bl' ->
						if bl' <> bl then [s] else [Failed]
			)
		in
			Iterators.of_list result
	in
	
	let accept = function
		TrackingE (_, e_state) -> e_final e_state
	|	TrackingA (_, a_state) -> a_final a_state
	| _ -> false
	in

	NBA.build states alphabet initial delta accept;;
	
	
let ctlstar_trace_nba_state_size (_, frs, _, _) e a =
	let value = 2.0 *. (2.0 ** (float (Array.length frs))) *. (float e +. float a) +. 2.0 in
	let max_value = max_int / 8 in
	if (value > float max_value) then max_value else truncate value

	