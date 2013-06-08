open Tcsbasedata;;
open Tcsautomata;;
open Tcsset;;
open Tcslist;;
open Ctlstarformula;;

type rules = Branch of block * int * bool
           | Follow of block * int
           | Next0
           | Next1 of block
           | Delete of block * int
           | DeleteBlock of block

type ctl_trace_nba_state = Failed
				           | Waiting
				           | Tracking of block * int

let ctl_trace_nba ((r, fmls, lnks, props) as dcp) =

	let cmp_state = Comparators.default in
	let cmp_alpha = Comparators.default in
	
	let fmt_state = function
		Failed -> "<FAILED>"
	|	Waiting -> "<WAITING>"
	|	Tracking (b, formula) -> "TRACKING(" ^ format_block dcp b ^ ", " ^ format_decomposed_formula dcp formula ^ ")"
	in

	let fmt_alpha = function
		Branch (bl, f, dir) -> (if dir then "L" else "R") ^ "(" ^ format_block dcp bl ^ "," ^ format_decomposed_formula dcp f ^ ")"
	|	Follow (bl, f) -> "F" ^ "(" ^ format_block dcp bl ^ "," ^ format_decomposed_formula dcp f ^ ")"
	|	Next0 -> "N0"
	|	Next1 bl -> "N1(" ^ format_block dcp bl ^ ")"
	|	Delete (bl, f) -> "D(" ^ format_block dcp bl ^ "," ^ format_decomposed_formula dcp f ^ ")"
	|	DeleteBlock bl -> "D(" ^ format_block dcp bl ^ ")"
	in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = Waiting in

	let delta s r =
		let add_to_block = add_to_block dcp in
		let result = match s with
			Waiting -> (
				let process path_kind g =
					match fmls.(g) with
						FIntFixpoint (false, _, _) ->
							[Waiting; Tracking (add_to_block ((if path_kind then EBlock else ABlock), [], []) [g], g)]
					|	_ -> [Waiting]
				in
				match r with
					Branch (_, f, true) -> (
						match fmls.(f) with
							FIntPath (path_kind, g) -> process path_kind g
						|	_ -> [Waiting]
					)
				|	Follow (_, f) -> (
						match fmls.(f) with
							FIntPath (path_kind, g) -> process path_kind g
						|	_ -> [Waiting]
					)
				|	_ -> [Waiting]
			)
		|	Failed -> [Failed]
		|	Tracking (((kind, nonmods, mods) as bl), focus) -> (
				match r with
					Follow (bl', f) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match fmls.(f) with
								FIntBranch (br, le, ri) ->
									if br && (kind = EBlock) then
										[Tracking (add_to_block (kind, nonmods', mods) [le], focus);
										 Tracking (add_to_block (kind, nonmods', mods) [ri], focus)]
									else
										[Tracking (add_to_block (kind, nonmods', mods) [le;ri], focus)]
							|	FIntFixpoint (fixkind, le, ri) ->
									if fixkind then
										[Tracking (add_to_block (kind, nonmods', mods) [ri], focus);
										 Tracking (add_to_block (kind, nonmods', mods) [le;lnks.(f).(0)], focus)]
									else if f = focus then [Tracking (add_to_block (kind, nonmods', mods) [ri;lnks.(f).(0)], lnks.(f).(0))]
									else [Tracking (add_to_block (kind, nonmods', mods) [le;ri], focus);
										  Tracking (add_to_block (kind, nonmods', mods) [ri;lnks.(f).(0)], focus)]
							|	FIntPath (_, z) ->
									[Tracking ((kind, nonmods', mods), focus)]
							|	_ -> failwith "impossible (trace) 1"						
						)
				|	Branch (bl', f, dir) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match fmls.(f) with
								FIntPath (pathkind, z) ->
									if dir then [Failed]
									else [Tracking ((kind, nonmods', mods), focus)]
							|	FIntBranch (false, le, ri) ->
									[Tracking (add_to_block (kind, nonmods', mods) [if dir then le else ri], focus)]
							|	FIntFixpoint (fixkind, le, ri) ->
									let bl'' = add_to_block (kind, nonmods', mods) (if fixkind then [ri;lnks.(f).(0)] else [le;lnks.(f).(0)]) in
									if dir then [Failed]
									else if f = focus then [Tracking (bl'', lnks.(f).(0))]
									else [Tracking (bl'', focus)]
							|	_ -> failwith "impossible (trace) 2"
						)
				|	Delete (bl', f) ->
						if bl' <> bl then [s]
						else if f = focus then [Failed]
						else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							 [Tracking ((kind, nonmods', mods), focus)]
				|	Next0 -> (
						match fmls.(focus) with
							FIntNext focus' ->
								[Tracking (add_to_block (kind, [], []) mods, focus')]
						|	_ -> failwith "impossible Next0"
					)
				|	Next1 bl' -> (
						if (kind = ABlock) && (bl' <> bl) then [Failed]
						else match fmls.(focus) with
								FIntNext focus' ->
									[Tracking (add_to_block (kind, [], []) mods, focus')]
							 |	_ -> failwith "impossible Next1"
					)
				|	DeleteBlock bl' ->
						if bl' <> bl then [s] else [Failed]
			)
		in
			Iterators.of_list result
	in

	let accept = function
		Tracking (_, _) -> false
	| _ -> true
	in

	NBA.build states alphabet initial delta accept;;
