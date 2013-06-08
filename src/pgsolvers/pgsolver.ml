open Libpgsolver;;
open Tcsbasedata;;

let use_pgsolver options game =
	let (solver, _, _) = find_solver options.(0) in
	solver [||] (Array.map (fun (a,b,c,_) -> (a,b,c,None)) game);;

Pgsolvers.register_solver use_pgsolver
                          "pgsolver"
						  "pgs"
						  ("<algorithm>\n     use pgsolver with" ^ fold_solvers (fun _ id _ _ s -> s ^ " " ^ id) "");;


let use_partial_pgsolver options ((init, delta, omega, player, format), _) =
	let data i =
		(omega i, (if player i then 0 else 1))
	in
	let format' i =
		let s = format i in
		if s = "" then None else Some s
	in
	let game = (init, (fun i -> Enumerators.of_list (delta i)), data, format') in
	let (solver, _, _) = find_partial_solver options.(0) in
	let res = solver [||] game in
	let sol' i = Some (fst (res i) = 0) in
	let strat' i = snd (res i) in
	(sol', strat');;

Pgsolvers.register_partial_solver use_partial_pgsolver
                                  "partialpgsolver"
								  "ppgs"
								  ("<algorithm>\n     use partial pgsolver with" ^ fold_partial_solvers (fun _ id _ _ s -> s ^ " " ^ id) "");;