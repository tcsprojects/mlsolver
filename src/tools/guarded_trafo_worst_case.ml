open Tcsmmcformula;;
open Arg ;;
open Tcsargs;;
open Tcsset;;
open Tcslist;;
open Tcsstrings;;


let formula n =
	let rec inner = function
		1 -> FOr (FVariable "X_1", FDiamond (FVariable "X_1"))
	| m -> FOr (FOr (FVariable ("X_" ^ string_of_int m), FDiamond (FVariable ("X_" ^ string_of_int m))), inner (m-1))
	in
	let rec outer = function
		0 -> inner n
	| m -> FMu ("X_" ^ string_of_int m, outer (m-1))
	in
		outer n

let format_formula_option = ref false
let format_formula_as_tree_option = ref false
let format_subformulas_option = ref false
let format_fps_option = ref false

let fpwords f =
	let rec helper acc word = function
		FNeg g -> helper acc word g
	| FDiamond g -> helper acc word g
	| FBox g -> helper acc word g
	| FAnd (g, h) -> helper (helper acc word g) word h
	| FOr (g, h) -> helper (helper acc word g) word h
	| FMu (x, g) -> helper acc (x::word) g
	| FNu (x, g) -> helper acc (x::word) g
	| _ -> TreeSet.add (List.rev word) acc
		
	in
		helper (TreeSet.empty_def) [] f
		
let fpword_fmt set =
	string_of_int (TreeSet.cardinal set) ^ "\n" ^ 
	TreeSet.format (ListUtils.custom_format (fun s -> s) "" "\n" ",") set		
	
let im_sub = function
	| FMu (x, g) -> [g]
	| FNu (x, g) -> [g]
	| FDiamond g -> [g]
	| FBox g -> [g]
	| FAnd (g,h) -> [g;h]
	| FOr (g,h) -> [g;h]
	| _ -> []

let tree_sub_set =
	let rec helper set t = TreeSet.add t (List.fold_left helper set (im_sub t)) in
	helper TreeSet.empty_def
		
						

let speclist = 
	 [(["--format"], Unit(fun _ -> format_formula_option := true),
                 "\n     Formats formula");
	  (["--format_tree"], Unit(fun _ -> format_formula_as_tree_option := true),
                 "\n     Formats formula as tree");								
	  (["--format_fps"], Unit(fun _ -> format_fps_option := true),
                 "\n     Formats fixpoint words");								
	  (["--format_sf"], Unit(fun _ -> format_subformulas_option := true),
                 "\n     Formats subformulas")								
	 ]


let _ =
	let n = ref 0 in
	SimpleArgs.parsedef speclist (fun f -> n := int_of_string f)
				 ("Usage: guarded_trafo_worst_case [n] [options]\n" ^ "\nOptions are");
	let n = !n in
	let f = formula n in
	let g = guarded_transform f in
	let dec = normal_form_formula_to_decomposed_formula g in
  let (_, sub, _, _) = dec in
	
	print_string ("Length     : " ^ string_of_int (formula_length g) ^ "\n");
	print_string ("Height     : " ^ string_of_int (formula_height g) ^ "\n");
	print_string ("Variables  : " ^ string_of_int (let (vt, _, _) = formula_variable_occs g in vt) ^ "\n");
	print_string ("Subformulas: " ^ string_of_int (TreeSet.cardinal (tree_sub_set g)) ^ "\n");
	if (!format_formula_option) then (
		print_string "\n";
		print_string (format_formula g ^ "\n");
	);
	if (!format_formula_as_tree_option) then (
		print_string "\n";
		print_string (format_formula_as_tree g ^ "\n");
	);
	if (!format_fps_option) then (
		print_string "\n";
		print_string (fpword_fmt (fpwords g) ^ "\n");
	);
	if (!format_subformulas_option) then (
		Array.iteri (fun i s ->
			print_string (StringUtils.fillup_left (string_of_int i ^ " : " ^ format_decomposed_formula dec i ^ "\n") 5 ' ');
			flush stdout;
		) sub;
	);
	print_string "\n\n"
	;;
