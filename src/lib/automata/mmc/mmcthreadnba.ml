open Tcsbasedata;;
open Tcsautomata;;
open Tcsmmcformula;;

type mmc_thread_nba_state = Tracking of (int * int) | Waiting | Failed;;

type rules = Branch of bool * int | Follow of int | Delete of int;;

let mmc_thread_nba ((_, frs, _, vars) as dcp) =

	let cmp_state = Comparators.default in
	let cmp_alpha = Comparators.default in
	
	let fmt_state = function
		Waiting -> "<WAITING>"
	|	Failed -> "<FAILED>"
	|	Tracking (f, k) -> "(" ^ format_decomposed_formula dcp f ^ ", " ^ fst vars.(k) ^ ")"
	in

	let fmt_alpha = function
		Branch (dir, f) -> (if dir then "L" else "R") ^ "(" ^ format_decomposed_formula dcp f ^ ")"
	|	Delete f -> "D(" ^ format_decomposed_formula dcp f ^ ")"
	|	Follow f -> "F(" ^ format_decomposed_formula dcp f ^ ")"
	in

	let states = Domain.make cmp_state fmt_state in
	let alphabet = Domain.make cmp_alpha fmt_alpha in
	
	let initial = Waiting in
	
	let delta s r =
		let result = match s with
			Waiting -> (
				match r with
					Follow i -> (
						match frs.(i) with
							FIntVariable j ->
								let (_, (b, _, _, f)) = vars.(j) in
								if b then [Waiting; Tracking (f, j)] else [Waiting]
							| _ -> [Waiting]
						)
					| _ -> [Waiting]
			)
		|	Failed -> [Failed]
		|	Tracking (f, v) -> (
				match r with
					Branch (b, i) -> (
						if f = i
						then match frs.(i) with
								FIntBranch (false, le, ri) -> [Tracking ((if b then le else ri), v)]
							|	_ -> [Failed]
						else [s]
					)
				|	Delete i ->
						if f = i then [Failed] else [s]
				|	Follow i -> (
						let iexp = frs.(i) in
						match iexp with
							FIntBranch (true, le, ri) -> if i = f then [Tracking (le, v); Tracking (ri, v)] else [s]
						|	FIntModality (false, n) -> (
								match frs.(f) with
									FIntModality(b, n') ->
										if b || (n = n') then [Tracking (n', v)] else [Failed]
								| _ -> [Failed]
							)
						|	FIntVariable z -> (
								match frs.(f) with 
									FIntVariable z' ->
										if z = z' then (
											let (_, (b, prz, _, g)) = vars.(z) in
											let (_, (_, prv, _, _)) = vars.(v) in
												if (*b ||*) (prz <= prv) then [Tracking (g, v)] else [Failed]
										)
										else [s]
									| _ -> [s]
								)
						|	_ -> [s]
			))
		in
			Iterators.of_list result
	in
	
	let accept = function
		Tracking (i, j) -> (
			match frs.(i) with
				FIntVariable k -> k = j
			|	_ -> false
		)
	| 	_ -> false
	in

	NBA.build states alphabet initial delta accept;;
	
	
let mmc_thread_nba_state_size (_, frs, _, vars) = 1 + (Array.length frs) * (Array.length vars);;
