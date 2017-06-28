open Tcsgameparser;;
open Tcsgames;;

let parse_solution_only_solver options _ =
	parse_explicit_parity_solution (open_in options.(0));;

let call_external_solver options game =
	Random.self_init ();
	let temp_game_name = Filename.temp_file "mlsolver_temp_game_" ".gm" in
	let temp_sol_name = Filename.temp_file "mlsolver_temp_sol_" ".sol" in
	let handle = open_out temp_game_name in
	print_explicit_pg game (output_string handle);
	close_out handle;
	let _ = Sys.command (options.(0) ^ " " ^ temp_game_name ^ " > " ^ temp_sol_name) in
	let handle = open_in temp_sol_name in
	let ret = parse_explicit_parity_solution handle in
	close_in handle;
	Sys.remove temp_game_name;
	Sys.remove temp_sol_name;
	ret;;


let register _ =
    Pgsolversregistry.register_solver parse_solution_only_solver "parsesolution" "ps" "<file>\n     Parses the solution to the decision game and draws conclusions";
    Pgsolversregistry.register_solver call_external_solver "callexternal" "ce" ("<commandline>\n     Calls external parity game solver," ^
                                  "\n     e.g. -ce \"pgsolverbin -re -v 0 --solonly\"");;