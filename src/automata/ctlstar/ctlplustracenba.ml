open Tcsautomata;;
open Tcsbasedata;;
open Tcsset;;
open Ctlstarformula;;
open Ctlstartracenba;;

type rules = Ctlstartracenba.rules

type ctlplus_trace_nba_state = Failed
                    | Waiting
                    | TrackingE of block
                    | TrackingA of block * int

let ctlplus_trace_nba ((r, fmls, lnks, props) as dcp) =

	let cmp_state = Comparators.default in
	let cmp_alpha = Comparators.default in
	
	let fmt_state = function
		Failed -> "<FAILED>"
	|	Waiting -> "<WAITING>"
	|	TrackingA (b, formula) -> "TRACKING_A(" ^ format_block dcp b ^ ", " ^ format_decomposed_formula dcp formula ^ ")"
	|	TrackingE b -> "TRACKING_E(" ^ format_block dcp b ^ ")"
	in

	let fmt_alpha = Ctlstartracenba.ctlstar_trace_nba_format_rule dcp in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = Waiting in
	
	let delta s r =
		let add_to_block = add_to_block dcp in
		let rec delta' s r = match s with
			Waiting -> (
				let checkablock nonmods mods f =
					match fmls.(f) with
						FIntFixpoint (false, _, _) -> Waiting::(delta' (TrackingA ((ABlock, nonmods, mods), f)) r)
					|	_ -> [Waiting]
				in
				let checkeblock nonmods mods =
					let rec subcheck f =
						match fmls.(f) with
							FIntBranch (_, le, ri) -> subcheck le && subcheck ri
						|	FIntFixpoint (false, le, ri) -> subcheck le && subcheck ri
						|	FIntFixpoint (true, _, _) -> false
						|	_ -> true
					in
					if (List.for_all subcheck nonmods) && (List.for_all subcheck mods)
					then Waiting::(delta' (TrackingE (EBlock, nonmods, mods)) r)
					else [Waiting]
				in
				match r with
					Branch ((ABlock, nonmods, mods), f, _) -> checkablock nonmods mods f
				|	Follow ((ABlock, nonmods, mods), f) -> checkablock nonmods mods f
				|	Branch ((EBlock, nonmods, mods), _, _) -> checkeblock nonmods mods
				|	Follow ((EBlock, nonmods, mods), _) -> checkeblock nonmods mods
				|	_ -> [Waiting]
			)
		|	Failed -> [Failed]
		|	TrackingE (((_, nonmods, mods) as bl)) -> (
				match r with
					Follow (bl', f) ->
						if bl' <> bl then [s] else (
							let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							match fmls.(f) with
								FIntBranch (br, le, ri) ->
									if br then
										[TrackingE((add_to_block (EBlock, nonmods', mods) [le]));
										 TrackingE((add_to_block (EBlock, nonmods', mods) [ri]))]
									else
										[TrackingE (add_to_block (EBlock, nonmods', mods) [le;ri])]
							|	FIntFixpoint (fixkind, le, ri) ->
									if fixkind then
										[TrackingE((add_to_block (EBlock, nonmods', mods) [ri]));
										 TrackingE((add_to_block (EBlock, nonmods', mods) [le;lnks.(f).(0)]))]
									else
										[TrackingE((add_to_block (EBlock, nonmods', mods) [le;ri]));
										 TrackingE((add_to_block (EBlock, nonmods', mods) [ri;lnks.(f).(0)]))]
							|	_ -> failwith "impossible (trace) 1"
						)
				|	Next0 ->
						[TrackingE (add_to_block (EBlock, [], []) mods)]
				|	Next1 _ ->
						[TrackingE (add_to_block (EBlock, [], []) mods)]
				|	Branch (bl', f, dir) ->
						if bl' <> bl then [s] else (
							match fmls.(f) with
								FIntPath (pathkind, z) ->
									if dir then [Failed]
									else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
										 [TrackingE ((EBlock, nonmods', mods))]
							|	_ -> failwith "impossible (trace) 2"
						)
				|	Delete (bl', f) ->
						if bl' <> bl then [s]
						else let nonmods' = TreeSet.elements ((TreeSet.remove f (TreeSet.of_list compare nonmods))) in
							 [TrackingE ((EBlock, nonmods', mods))]
				|	DeleteBlock bl' ->
						if bl' <> bl then [s] else [Failed]
			)
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
		in
			Iterators.of_list (delta' s r)
	in

	let accept = function
		TrackingE _ -> false
	|	TrackingA _ -> false
	| _ -> true
	in

	NBA.build states alphabet initial delta accept;;


let ctlplus_trace_nba_state_size (_, frs, _, _) =
	let value = 2.0 *. (2.0 ** (float (Array.length frs))) *. (1.0 +. float (Array.length frs)) +. 2.0 in
	let max_value = max_int / 8 in
	if (value > float max_value) then max_value else truncate value

