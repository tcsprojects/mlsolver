open Arg ;;
open Tcsformulaparse ;;
open Validitygames;;
open Modelcheckinggames;;
open Tcsautomataparser;;
open Tcsautomata;;
open Tcsgames;;
open Tcstransitionsys;;
open Tcstiming;;
open Tcsargs;;
open Tcsmessage;;
open Tcsmetaformula;;
open Pgsolvers;;

type mlsolvermode = NoMode |
                    Validity of (bool * string) |
					Modelchecking of string

type workmode = DoNothing |
                PrintGameOnly |
                CallSolver of string * string array * solver_func |
				CallPartialSolver of string * string array * partial_solver_func
				
let mlsolvermode = ref NoMode

let workmode = ref DoNothing

let options = ref []

type input_kind = NoInput | InputFromFile of string | InputFromStdIn

let environment_file = ref NoInput

let validity_list = 
	String.concat ", " (fold_validity_procedures (fun _ ident _ l -> ident::l) [])

let modelchecking_list = 
	String.concat ", " (fold_modelchecking_procedures (fun _ ident _  l -> ident::l) [])
	
let verbosity = ref 1

let gen_counter_mod = ref false

let formula_string = ref ""

let ts_file = ref NoInput
	
let speclist =  [(["--validity"; "-val"], String(fun ident -> mlsolvermode := Validity (true, ident)),
                 "<logic>\n     Decides validity, valid arguments are " ^ validity_list);
				 (["--satisfiability"; "-sat"], String(fun ident -> mlsolvermode := Validity (false, ident)),
                 "<logic>\n     Decides satisfiability, valid arguments are " ^ validity_list);
				 (["--modelchecking"; "-mc"], String(fun ident -> mlsolvermode := Modelchecking ident),
                 "<logic>\n     Decides modelchecking, valid arguments are " ^ modelchecking_list);
				 (["--transitionsystem"; "-ts"], String(fun s -> ts_file := InputFromFile s),
				 "<file>\n     Parses transition system");
				 (["--stdints"; "-stts"], Unit(fun _ -> ts_file := InputFromStdIn),
				 "\n     Parses transition system from stdin");
				 (["--environment"; "-env"], String(fun s -> environment_file := InputFromFile s),
				 "<file>\n     Parses formula environment");
				 (["--stdinenv"; "-stenv"], Unit(fun _ -> environment_file := InputFromStdIn),
				 "\n     Parses formula environment from stdin");
				 (["--option"; "-opt"], String(fun s -> options := s::!options),
				 "\n     Adds options to decision procedure; available:" ^
				 "\n        ann         : add decision game annotations" ^
(*				 "\n        litpro      : enable literal propagation" ^ *)
				 "\n        comp        : compact games" ^
				 "\n        ctlplus     : use ctl plus optimization (experimental)" ^
				 "\n        prefersmall : prefer small subformulas" ^
				 "\n        preferlarge : prefer large subformulas" ^
                 "\n        woguarded   : without guarded transformation");
				 (["--printgame"; "-pg"], Unit(fun _ -> workmode := PrintGameOnly),
				 "\n     Generates and prints the decision game");
				 (["--generatemodel"; "-gm"], Unit(fun _ -> gen_counter_mod := true),
				 "\n     Generates a (counter)model if applicable");
				 (["--verbose"; "-ve"], Unit(fun _ -> verbosity := 2),
				 "\n     causes the program to be verbose");
				 (["--quiet"; "-qu"], Unit(fun _ -> verbosity := 0),
				 "\n     causes the program to be quiet")]
			@

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun s -> workmode := CallSolver(ident, [|s|], solve)),
  					 desc)::arr
 ) []

			@

  fold_partial_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun s -> workmode := CallPartialSolver(ident, [|s|], solve)),
  					 desc)::arr
 ) []
	
	
let total_transform game =
		Array.init (Array.length game) (fun i ->
			let (pr, pl, tr, de) = game.(i) in
			if Array.length tr > 0
			then (pr, pl, tr, de)
			else (1-pl, pl, [|i|], de)
		)
	
	
let header =
		"Modal Logic Solver 1.2\n" ^ 
		"Authors: Oliver Friedmann (University of Munich) and Martin Lange (University of Kassel), 2008-2013\n\n"

let _ =
	SimpleArgs.parsedef speclist (fun f -> formula_string := f) (header ^ "Usage: mlsolver [options] [formula]\n" ^
																		  "\nOptions are");
			
	let info_chan = MessageChannel.add_anonymous_root_channel () in
	let formula_chan = MessageChannel.add_anonymous_root_channel () in
	let constr_chan = MessageChannel.add_anonymous_root_channel () in

	let stderr_listener = MessageChannel.new_listener (fun _ s _ -> output_string stderr s; flush stderr) in
	
	let info_msg = MessageChannel.send_message info_chan in
	let constr_msg = MessageChannel.send_message constr_chan in
	
	if !verbosity > 0 then (
		MessageChannel.add_listener stderr_listener info_chan;
		if !verbosity > 1 then (
			MessageChannel.add_listener stderr_listener formula_chan;
			MessageChannel.add_listener stderr_listener constr_chan;
		);
	);
	
	info_msg (fun _ -> "\n" ^ header);
	
	(match !mlsolvermode with
		NoMode -> (
			output_string stderr "Failure: You need to specify either --validity, --satisfiability or --modelchecking.\n";
			exit 1
		)
	|	Validity (_, ident) -> (
			if not (mem_validity_procedure ident) then (
				output_string stderr ("Failure: Unknown decision procedure '" ^ ident ^ "'.\n");
				exit 1
			)
		)
	|	Modelchecking ident -> (
			if not (mem_modelchecking_procedure ident) then (
				output_string stderr ("Failure: Unknown decision procedure '" ^ ident ^ "'.\n");
				exit 1
			)
		)
	);
	
	if !formula_string = "" then (
		output_string stderr "Failure: You need to specify a formula.\n";
		exit 1
	);
	
	let environment =
		match !environment_file with
			NoInput -> []
		|	InputFromFile f -> (
			info_msg (fun _ -> "Parsing environment... ");
			let t = SimpleTiming.init true in
			let env = Tcsformulaparser.program Tcsformulalexer.lexer (Lexing.from_channel (open_in f)) in
			info_msg (fun _ -> (SimpleTiming.format t) ^ "\n");
			env
		)
		|	InputFromStdIn -> (
			info_msg (fun _ -> "Parsing environment... ");
			let t = SimpleTiming.init true in
			let env = Tcsformulaparser.program Tcsformulalexer.lexer (Lexing.from_channel stdin) in
			info_msg (fun _ -> (SimpleTiming.format t) ^ "\n");
			env
		)
	in
	
	info_msg (fun _ -> "Evaluating formula... ");
	let t = SimpleTiming.init true in
	let formula = eval_formula (parse_expression !formula_string) environment in
	info_msg (fun _ -> (SimpleTiming.format t) ^ "\n");
	
	match !mlsolvermode with
		NoMode -> ()
	|	Modelchecking ident -> (
			let (proc, descr) = find_modelchecking_procedure ident in
			let in_chan =
				match !ts_file with
					NoInput -> (
						output_string stderr "Failure: You need to specify a transition system.\n";
						exit 1
					)
				|	InputFromFile f -> open_in f
				|	InputFromStdIn -> stdin
			in
			info_msg (fun _ -> "\nUsing " ^ descr ^ "\n");
			info_msg (fun _ -> "Starting procedure...\n\n");
			let (game_cached, show_stats, answer) = proc formula in_chan (Array.of_list !options) (info_chan, formula_chan, constr_chan) in
			
			let build_game _ =
				info_msg (fun _ -> "Building...\n");
				let t' = SimpleTiming.init true in
				let (init_node, game) = build_explicit_initpg game_cached (fun i ->
					if (i mod 1000 = 0)
					then constr_msg (fun _ -> "\rBuilding..." ^ string_of_int i)
				) in
				info_msg (fun _ -> "\rBuilding... finished in " ^ SimpleTiming.format t' ^ "\n\n");
				(init_node, game)
			in
			
			let result =
				match !workmode with
					DoNothing -> None
				|	PrintGameOnly ->
						let (init_node, game) = build_game () in
						let game = total_transform game in
						show_stats ();
						print_explicit_initpg (init_node, game) print_string;
						None
				|	CallSolver (ident, options, solver) -> (
						let (_, game) = build_game () in
						show_stats ();
						info_msg (fun _ -> "Calling solver " ^ ident ^ "...\n\n");
						let t = SimpleTiming.init true in
						let (sol, strat) = solver options game in
						
						let sol' i =
							match sol.(i) with
								0 -> Some true
							|	1 -> Some false
							|	_ -> None
						in
						
						let strat' i =
							let s = strat.(i) in
							if s < 0 then None else Some s
						in
								
						info_msg (fun _ -> "\nFinished solving: " ^ (SimpleTiming.format t) ^ "\n\n");
						Some (sol', strat')
					)
				|	CallPartialSolver (ident, options, solver) -> (
						info_msg (fun _ -> "Calling partial solver " ^ ident ^ "...\n\n");
						let t = SimpleTiming.init true in
						let (sol, strat) = solver options game_cached in
						show_stats ();
						info_msg (fun _ -> "\nFinished solving: " ^ (SimpleTiming.format t) ^ "\n\n");
						Some (sol, strat)
					)
			in
			
			(match result with
				None -> ()
			|	Some (sol, _) -> (
					let res = answer sol in
					match res with
						ModelSatisfiesFormula -> 
							info_msg (fun _ -> "Transition system is a model of the formula!\n")
					|	ModelFalsifiesFormula -> 
							info_msg (fun _ -> "Transition system is no model of the formula!\n")
						)
			);
			info_msg (fun _ -> "\n");
	)
	|	Validity (valmode, ident) -> (
			let formula = if valmode then formula else FNeg formula in
			let (proc, descr) = find_validity_procedure ident in
			info_msg (fun _ -> "\nUsing " ^ descr ^ "\n");
			info_msg (fun _ -> "Starting procedure...\n\n");
			let (game_cached, show_stats, answer) = proc formula (Array.of_list !options) (info_chan, formula_chan, constr_chan) in

			let build_game _ =
				info_msg (fun _ -> "Building...\n");
				let t' = SimpleTiming.init true in
				let (init_node, game) = build_explicit_initpg game_cached (fun i ->
					if (i mod 1000 = 0)
					then constr_msg (fun _ -> "\rBuilding..." ^ string_of_int i)
				) in
				info_msg (fun _ -> "\rBuilding... finished in " ^ SimpleTiming.format t' ^ "\n\n");
				(init_node, game)
			in
				
			let result =
				match !workmode with
					DoNothing -> None
				|	PrintGameOnly ->
						let (init_node, game) = build_game () in
						let game = total_transform game in
						show_stats ();
						print_explicit_initpg (init_node, game) print_string;
						None
				|	CallSolver (ident, options, solver) -> (
						let (_, game) = build_game () in
						show_stats ();
						info_msg (fun _ -> "Calling solver " ^ ident ^ "...\n\n");
						let t = SimpleTiming.init true in
						let (sol, strat) = solver options game in
						
						let sol' i =
							match sol.(i) with
								0 -> Some true
							|	1 -> Some false
							|	_ -> None
						in
						
						let strat' i =
							let s = strat.(i) in
							if s < 0 then None else Some s
						in
								
						info_msg (fun _ -> "\nFinished solving: " ^ (SimpleTiming.format t) ^ "\n\n");
						Some (sol', strat')
					)
				|	CallPartialSolver (ident, options, solver) -> (
						info_msg (fun _ -> "Calling partial solver " ^ ident ^ "...\n\n");
						let t = SimpleTiming.init true in
						let (sol, strat) = solver options game_cached in
						show_stats ();
						info_msg (fun _ -> "\nFinished solving: " ^ (SimpleTiming.format t) ^ "\n\n");
						Some (sol, strat)
					)
			in
			
			match result with
				None -> ()
			|	Some (sol, strat) -> (
					let res = answer sol strat in
					match res with
						FormulaValid -> 
							if valmode
							then info_msg (fun _ -> "Formula is valid!\n")
							else info_msg (fun _ -> "Formula is unsatisfiable!\n")
					|	FormulaFalsifiable ->
							if valmode
							then info_msg (fun _ -> "Formula is falsifiable!\n")
							else info_msg (fun _ -> "Formula is satisfiable!\n")
					|	FormulaFalsifiableBy counter_mod -> (
							if valmode
							then info_msg (fun _ -> "Formula is falsifiable!\n")
							else info_msg (fun _ -> "Formula is satisfiable!\n");
							if !gen_counter_mod
							then counter_mod print_string
						)
			);
			info_msg (fun _ -> "\n");
	);;
