open Solvers ;;

open Paritygame;;
open Tcsbasedata;;

let use_pgsolver options game =
	let (solver, _, _) = find_solver options.(0) in
	let (sol, strat) = solver [||] (pg_init (Array.length game) (fun i ->
	    let (a,b,c,_) = game.(i) in
	    (* (a,if b = 0 then plr_Even else plr_Odd, Array.to_list c,None))*)
	    let pl = if b = 0 then plr_Even else plr_Odd in
	    (a, pl, Array.to_list c, None)
	)) in
	(Array.map (fun pl -> if pl = plr_Even then 0 else 1) sol, strat);;




let use_partial_pgsolver options ((init, delta, omega, player, format), _) =
	let data i =
		(omega i, (if player i then plr_Even else plr_Odd))
	in
	let format' i =
		let s = format i in
		if s = "" then None else Some s
	in
	let game = (init, (fun i -> Enumerators.of_list (delta i)), data, format') in
	let (solver, _, _) = find_partial_solver options.(0) in
	let res = solver [||] game in
	let sol' i = Some (fst (res i) = plr_Even) in
	let strat' i = snd (res i) in
	(sol', strat');;


let register _ =
    Pgsolversregistry.register_solver use_pgsolver
                          "pgsolver"
						  "pgs"
						  ("<algorithm>\n     use pgsolver with" ^ fold_solvers (fun _ id _ _ s -> s ^ " " ^ id) "");
    Pgsolversregistry.register_partial_solver use_partial_pgsolver
                                  "partialpgsolver"
								  "ppgs"
								  ("<algorithm>\n     use partial pgsolver with" ^ fold_partial_solvers (fun _ id _ _ s -> s ^ " " ^ id) "");;