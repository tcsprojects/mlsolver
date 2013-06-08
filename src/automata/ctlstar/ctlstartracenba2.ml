open Tcsautomata;;
open Tcsbasedata;;
open Tcsset;;
open Tcslist;;
open Ctlstarformula;;
open Ctlstartracenba;;

type ctlstar_trace_nba2_state = Failed
		   | Waiting
           | TrackingE of block * block
		   | TrackingA of block * int

type rules = Ctlstartracenba.rules

let ctlstar_trace_nba2 ((r, fmls, lnks, props) as dcp) =

	let cmp_state = Comparators.default in
	let cmp_alpha = Comparators.default in
	
	let fmt_state = function
		Failed -> "<FAILED>"
	|	Waiting -> "<WAITING>"
	|	TrackingE (bl, bl') -> "TRACKING(" ^ format_block dcp bl ^ ", " ^ format_block dcp bl' ^ ")"
	|	TrackingA (bl, f) -> "TRACKING(" ^ format_block dcp bl ^ ", " ^ format_decomposed_formula dcp f ^ ")"
	in

	let fmt_alpha = Ctlstartracenba.ctlstar_trace_nba_format_rule dcp in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = Waiting in
	
	let delta s r =
		let add_to_block = add_to_block dcp in
		let rec delta' s r = match s with
			Waiting -> (
				match r with
					Branch (_, f, true) -> (
						match fmls.(f) with
							FIntPath (true, g) ->
								[Waiting; let bl = add_to_block (EBlock, [], []) [g] in TrackingE (bl, bl)]
						|	_ -> [Waiting]
					)
				|	Follow (_, f) -> (
						match fmls.(f) with
							FIntPath (true, g) ->
								[Waiting; let bl = add_to_block (EBlock, [], []) [g] in TrackingE (bl, bl)]
						|	_ -> [Waiting]
					)
				|	Branch (bl, f, false) -> (
						match fmls.(f) with
							FIntFixpoint (false, _, _) ->
								Waiting::delta' (TrackingA (bl, f)) r
						|	_ -> [Waiting]
					)
				|	_ -> [Waiting]
			)
		|	Failed -> [Failed]
		|	TrackingA (((_, nonmods, mods) as bl), fp) -> (
				match r with
					Follow (bl', f) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match fmls.(f) with
								FIntBranch (true, le, ri) ->
									[TrackingA (add_to_block (ABlock, nonmods', mods) [le;ri], fp)]
							|	FIntPath (pathkind, z) ->
									let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
									[TrackingA ((ABlock, nonmods', mods), fp)]
							|	_ -> failwith "impossible (trace) 3"
						)
				|	Branch (bl', f, dir) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match fmls.(f) with
								FIntBranch (false, le, ri) ->
									[TrackingA (add_to_block (ABlock, nonmods', mods) [if dir then le else ri], fp)]
							|	FIntFixpoint (fixkind, le, ri) ->
									if f = fp then (
										if dir then [Failed]
										else [TrackingA (add_to_block (ABlock, nonmods', mods) [le;lnks.(f).(0)], lnks.(f).(0))]
									)
									else if dir then
										[TrackingA (add_to_block (ABlock, nonmods', mods) (if fixkind then [le;ri] else [ri]), fp)]
									else
										[TrackingA (add_to_block (ABlock, nonmods', mods) 
										           (if fixkind then [ri;lnks.(f).(0)] else [le;lnks.(f).(0)]), fp)]
							|	_ -> failwith "impossible (trace) 4"
						)
				|	Delete (bl', f) ->
						if bl' <> bl then [s]
						else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							 [TrackingA ((ABlock, nonmods', mods), fp)]
				|	Next1 bl' ->
						if bl' <> bl then [Failed]
						else [TrackingA (add_to_block (ABlock, [], []) mods, (match fmls.(fp) with FIntNext f -> f | _ -> failwith "impossible"))]
				|	Next0 -> failwith "impossible (trace) 5"
				|	DeleteBlock bl' ->
						if bl' <> bl then [s] else [Failed]
			)
		|	TrackingE (((_, nonmods, mods) as bl), (_, nonmodsf, modsf)) -> (
				let (nonmodsf, modsf) = if (nonmodsf = [] && modsf = []) then (nonmods, mods) else (nonmodsf, modsf) in
				match r with
					Follow (bl', f) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							let nonmodsf'' = TreeSet.of_list compare nonmodsf in
							let (nonmodsf', f_in_focus) = if TreeSet.mem f nonmodsf''
							                              then (TreeSet.elements (TreeSet.remove f nonmodsf''), true)
														  else (nonmodsf, false) in
							let add_to_focus g =
								if f_in_focus then add_to_block (EBlock, nonmodsf', modsf) g else (EBlock, nonmodsf', modsf)
							in
							match fmls.(f) with
								FIntBranch (br, le, ri) ->
									if br then
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le], add_to_focus [le]);
										 TrackingE (add_to_block (EBlock, nonmods', mods) [ri], add_to_focus [ri])]
									else
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le;ri], add_to_focus [le;ri])]
							|	FIntFixpoint (fixkind, le, ri) ->
									if fixkind then
										[TrackingE (add_to_block (EBlock, nonmods', mods) [ri], add_to_focus [ri]);
										 TrackingE (add_to_block (EBlock, nonmods', mods) [le;lnks.(f).(0)], add_to_focus [le;lnks.(f).(0)])]
									else
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le;ri], add_to_focus [le;ri]);
										 TrackingE (add_to_block (EBlock, nonmods', mods) [ri;lnks.(f).(0)], add_to_focus [ri])]
							|	_ -> failwith "impossible (trace) 1"
						)
				|	Branch (bl', f, dir) ->
						if bl' <> bl then [s] else (
							match fmls.(f) with
								FIntPath (pathkind, z) ->
									if dir then [Failed]
									else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
									     let nonmodsf' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmodsf))) in
										  [TrackingE ((EBlock, nonmods', mods), (EBlock, nonmodsf', modsf))]
							|	_ -> failwith "impossible (trace) 2"
						)
				|	Delete (bl', f) ->
						if bl' <> bl then [s]
						else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
						     let nonmodsf' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmodsf))) in
							 [TrackingE ((EBlock, nonmods', mods), (EBlock, nonmodsf', modsf))]
				|	Next0 ->
						[TrackingE (add_to_block (EBlock, [], []) mods, add_to_block (EBlock, [], []) mods)]
				|	Next1 _ ->
						[TrackingE (add_to_block (EBlock, [], []) mods, add_to_block (EBlock, [], []) modsf)]
				|	DeleteBlock bl' ->
						if bl' <> bl then [s] else [Failed]
			)
		in
			Iterators.of_list (delta' s r)
	in

	let accept = function
		TrackingE (_, (EBlock, [], [])) -> true
	|	TrackingA (_, _) -> true
	| _ -> false
	in

	NBA.build states alphabet initial delta accept;;

let ctlstar_trace_nba2_state_size (_, frs, _, _) =
	let l = float (Array.length frs) in
	let value = 2.0 +. (2.0 ** l) *. (2.0 ** l +. l) in
	let max_value = max_int / 8 in
	if (value > float max_value) then max_value else truncate value

