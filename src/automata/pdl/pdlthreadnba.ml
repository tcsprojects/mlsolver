open Tcsbasedata;;
open Tcsautomata;;
open Tcspdlformula;;

(* Run not accepting iff corresponding to nu thread *)

type pdl_thread_nba_state = Tracking of int | Waiting | Stopped;;

type rules = Branch of bool * int | Follow of int | Next of int | Delete of int;;

let pdl_thread_nba ((_, frs, lnks, prgs, _, _) as dcp) =

	let cmp_state = Comparators.default in
	let cmp_alpha = Comparators.default in
	
	let fmt_state = function
		Waiting -> "<WAITING>"
	|	Stopped -> "<STOPPED>"
	|	Tracking f -> format_decomposed_formula dcp f
	in

	let fmt_alpha = function
		Branch (dir, f) -> (if dir then "L" else "R") ^ "(" ^ format_decomposed_formula dcp f ^ ")"
	|	Delete f -> "D(" ^ format_decomposed_formula dcp f ^ ")"
	|	Follow f -> "F(" ^ format_decomposed_formula dcp f ^ ")"
	|	Next f -> "N(" ^ format_decomposed_formula dcp f ^ ")"
	in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = Waiting in

	let delta s r =
		let rec delta s r =
			match s with
				Stopped -> [Stopped]
			|	Waiting -> (
					let check i =
						match frs.(i) with
							FIntModality (false, p, _) -> (
								match prgs.(p) with
									FIntStar _ -> Waiting::(delta (Tracking i) r)
								|	_ -> [Waiting]
							)
						|	_ -> [Waiting]
					in
					match r with
						Follow i -> check i
					|	Branch (_, i) -> check i
					|	_ -> [Waiting]
				)
			|	Tracking i -> (
					match r with
						Delete f ->
							if f = i then [Stopped] else [s]
					|	Branch (dir, j) ->
							if i != j then [Tracking i] else (
								match frs.(j) with
									FIntBranch (false, le, ri) -> [Tracking (if dir then le else ri)]
								|	FIntModality (true, _, _) -> [Stopped]
								|	FIntModality (false, p, g) -> (
										match prgs.(p) with
											FIntChoice (_, _) -> [if dir then Tracking lnks.(i).(0) else Tracking lnks.(i).(1)]
										|	FIntStar _ -> [if dir then Tracking g else Tracking lnks.(i).(0)]
										|	_ -> failwith "impossible"
									)
								|	_ -> failwith "impossible"
							)
					|	Follow j -> if i != j then [Tracking i] else (						
							match frs.(j) with
								FIntBranch (true, le, ri) -> [Tracking le; Tracking ri]
							|	FIntModality (true, _, _) -> [Stopped]
							|	FIntModality (false, p, g) -> (
									match prgs.(p) with
										FIntConcat (_, _) -> [Tracking lnks.(i).(0)]
									|	FIntQuestion q -> [Tracking g; Tracking lnks.(i).(0)]
									|	_ -> failwith "impossible"
								)
							|	_ -> [Stopped]
						)
					|	Next j -> (
							match (frs.(i), frs.(j)) with
								(FIntModality (kind, pi, gi), FIntModality (false, pj, _)) ->
									if (pi = pj) && (kind || (i = j)) then [Tracking gi] else [Stopped]
								|	_ -> [Stopped]
						)
				)
		in
			Iterators.of_list (delta s r)
	in
	
	let accept = function
		Tracking _ -> false
	|	_ -> true
	in

	NBA.build states alphabet initial delta accept;;