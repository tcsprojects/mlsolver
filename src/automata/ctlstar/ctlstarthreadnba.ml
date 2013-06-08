open Tcsbasedata;;
open Tcsautomata;;
open Ctlstarformula;;

type ctlstar_thread_nba_state = Tracking of int | Waiting | Failed;;

type rules = FormulaNext | FormulaBranch of int * bool | FormulaFollow of int | FormulaDelete of int

let ctlstar_thread_nba_format_state dcp = function
	Waiting -> "<WAITING>"
|	Failed -> "<FAILED>"
|	Tracking f -> format_decomposed_formula dcp f;;


let ctlstar_thread_nba_format_rule dcp = function
	FormulaBranch (f, dir) -> (if dir then "L" else "R") ^ "(" ^ format_decomposed_formula dcp f ^ ")"
|	FormulaFollow f -> "F(" ^ format_decomposed_formula dcp f ^ ")"
|	FormulaDelete f -> "D(" ^ format_decomposed_formula dcp f ^ ")"
|	FormulaNext -> "N";;

let ctlstar_thread_nba_a_block ((_, fmls, lnks, _) as dcp) =
	let states = Domain.make Comparators.default (ctlstar_thread_nba_format_state dcp) in
	let alphabet = Domain.make Comparators.default (ctlstar_thread_nba_format_rule dcp) in
	let initial = Waiting in
	let accept = function Tracking _ -> true | _ -> false in
	let delta s r =
		let rec delta s r =
			match s with
				Failed -> [Failed]
			|	Waiting -> (
					let check i =
						match fmls.(i) with
							FIntFixpoint(false, _, _) -> Waiting::delta (Tracking i) r
						|	_ -> [Waiting]
					in
					match r with
						FormulaFollow i -> check i
					|	FormulaBranch (i, _) -> check i
					|	_ -> [Waiting]
				)
			|	Tracking i -> (
					match r with
						FormulaNext -> (
							match fmls.(i) with
								FIntNext j -> [Tracking j]
							|	_ -> [Failed]
						)
					|	FormulaDelete f ->
							if f = i then [Failed] else [Tracking i]
					|	FormulaFollow f ->
							if f != i then [Tracking i] else failwith "impossible (thread 1)"
					|	FormulaBranch (f, dir) ->
							if i != f then [Tracking i] else (
								match fmls.(i) with
									FIntBranch _ -> failwith "impossible (thread 2)"
								|	FIntFixpoint (false, _, _) ->
										if dir then [Failed] else [Tracking (lnks.(i).(0))]
								|	_ -> [Failed]
							)
				)
		in
			Iterators.of_list (delta s r)
	in

	NBA.build states alphabet initial delta accept;;


let ctlstar_thread_nba_e_block ((_, fmls, lnks, _) as dcp) =
	let states = Domain.make Comparators.default (ctlstar_thread_nba_format_state dcp) in
	let alphabet = Domain.make Comparators.default (ctlstar_thread_nba_format_rule dcp) in
	let initial = Waiting in
	let accept = function Tracking _ -> false | _ -> true in
	let delta s r =
		let rec delta s r =
			match s with
				Failed -> [Failed]
			|	Waiting -> (
					let check i =
						match fmls.(i) with
							FIntFixpoint(true, _, _) -> Waiting::delta (Tracking i) r
						|	_ -> [Waiting]
					in
					match r with
						FormulaFollow i -> check i
					|	FormulaBranch (i, _) -> check i
					|	_ -> [Waiting]
				)
			|	Tracking i -> (
					match r with
						FormulaNext -> (
							match fmls.(i) with
								FIntNext j -> [Tracking j]
							|	_ -> [Failed]
						)
					|	FormulaDelete f ->
							if f = i then [Failed] else [Tracking i]
					|	FormulaFollow f ->
							if f != i then [Tracking i] else failwith "impossible (thread 3)"
					|	FormulaBranch (f, dir) ->
							if i != f then [Tracking i] else (
								match fmls.(i) with
									FIntBranch _ -> failwith "impossible (thread 4)"
								|	FIntFixpoint (true, _, _) ->
										if dir then [Failed] else [Tracking (lnks.(i).(0))]
								|	_ -> [Failed]
							)
				)
		in
			Iterators.of_list (delta s r)
	in

	NBA.build states alphabet initial delta accept;;
	  
	  
let ctlstar_thread_nba_size (_, frs, _, _) = 2 + Array.length frs;;
	  