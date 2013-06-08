open Metaformula;;
open Tcsarray;;


type pdl_formula = 
	FProp of string
  | FTT
  | FFF
  | FNeg of pdl_formula
  | FAnd of pdl_formula * pdl_formula
  | FOr of pdl_formula * pdl_formula
  | FDiamond of pdl_program * pdl_formula
  | FBox of pdl_program * pdl_formula
and pdl_program =
    FAtom of string
  | FConcat of pdl_program * pdl_program
  | FChoice of pdl_program * pdl_program
  | FStar of pdl_program
  | FQuestion of pdl_formula

let rec eval_metaformula f =
  match f with
    Metaformula.FProp s -> FProp (identifier_to_str s)
  | Metaformula.Ftt -> FTT
  | Metaformula.Fff -> FFF
  | Metaformula.FNeg f -> FNeg (eval_metaformula f)
  | Metaformula.FOr (f1, f2) -> FOr (eval_metaformula f1, eval_metaformula f2)
  | Metaformula.FAnd (f1, f2) -> FAnd (eval_metaformula f1, eval_metaformula f2)
  | Metaformula.FLabelledOp ("<>", p, f) -> FDiamond (eval_program p, eval_metaformula f)
  | Metaformula.FLabelledOp ("[]", p, f) -> FBox (eval_program p, eval_metaformula f)
  | _ -> failwith("Formula is not able to be interpreted as PDL")
and eval_program p =
  match p with
    Metaformula.FProp s -> FAtom (identifier_to_str s)
  | Metaformula.FBinaryOp (".", f1, f2) -> FConcat (eval_program f1, eval_program f2)
  | Metaformula.FOr (f1, f2) -> FChoice (eval_program f1, eval_program f2)
  | Metaformula.FUnaryOp ("*", g) -> FStar (eval_program g)
  | Metaformula.FUnaryOp ("?", g) -> FQuestion (eval_metaformula g)
  | _ -> failwith("Program is not able to be interpreted as PDL");;
  

let rec formula_length f =
	let rec helper a = function
		FNeg f -> helper (a + 1) f
	|	FAnd (f, g) -> helper (helper (a + 1) f) g
	|	FOr (f, g) -> helper (helper (a + 1) f) g
	|	FDiamond (p, f) -> program_length p + helper (a + 1) f
	|	FBox (p, f) -> program_length p + helper (a + 1) f
	|	_ -> a + 1
	in
		helper 0 f
and program_length f =
	let rec helper a = function
		FStar f -> helper (a + 1) f
	|	FQuestion f -> formula_length f + a + 1
	|	FConcat (f, g) -> helper (helper (a + 1) f) g
	|	FChoice (f, g) -> helper (helper (a + 1) f) g
	|	_ -> a + 1
	in
		helper 0 f
		

let rec and_collect f =
  match f with
    FAnd (f1, f2) -> List.append (and_collect f1) (and_collect f2)
  | f -> [f]

let rec or_collect f =
  match f with
    FOr (f1, f2) -> List.append (or_collect f1) (or_collect f2)
  | f -> [f]

let rec format_formula_with_brackets f =
  let form = format_formula f in
  match f with
	FAnd (f1, f2) -> "(" ^ form ^ ")"
  | FOr (f1, f2) -> "(" ^ form ^ ")"
  | f -> form
and format_formula f =
  let unaryr f s = s ^ (format_formula_with_brackets f) in
  let binary f1 f2 s = (format_formula_with_brackets f1) ^ s ^ (format_formula_with_brackets f2) in
  let n_nary list s =
    let n_nary_cb a i = a ^ s ^ (format_formula_with_brackets i)
  in
    List.fold_left n_nary_cb (format_formula_with_brackets (List.hd list)) (List.tl list)
  in
  let default_format f = match f with
    FProp p -> p
  | FTT -> "tt"
  | FFF -> "ff"
  | FNeg f' -> unaryr f' "!"
  | FAnd (f1, f2) -> n_nary (and_collect f) " & "
  | FOr (f1, f2) -> n_nary (or_collect f) " | "
  | FDiamond (p, g) -> "<" ^ format_program p ^ ">" ^ format_formula_with_brackets g
  | FBox (p, g) -> "[" ^ format_program p ^ "]" ^ format_formula_with_brackets g
in
  match f with
  | FOr (FNeg g1, g2) -> binary g1 g2 " ==> "
  | FOr (FAnd(g1, g2), FAnd(FNeg (g1'), FNeg (g2'))) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <==> " else default_format f
  | FOr (FAnd(FNeg (g1), FNeg (g2)), FAnd(g1', g2')) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <==> " else default_format f
  | FOr (FAnd(g1, FNeg (g2)), FAnd(FNeg (g1'), g2')) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <~~> " else default_format f
  | f -> default_format f
and format_program_with_brackets f =
  let form = format_program f in
  match f with
	FConcat (f1, f2) -> "(" ^ form ^ ")"
  | FChoice (f1, f2) -> "(" ^ form ^ ")"
  | f -> form
and format_program f =
  let binary f1 f2 s = (format_program_with_brackets f1) ^ s ^ (format_program_with_brackets f2) in
  match f with
    FAtom p -> p
  | FConcat (f1, f2) -> binary f1 f2 "."
  | FChoice (f1, f2) -> binary f1 f2 "|"
  | FStar p -> format_program_with_brackets p ^ "^*"
  | FQuestion p -> format_formula_with_brackets p ^ "?";;
  
  
let rec formula_to_positive g =
  match g with
  | FOr (f1, f2) -> FOr (formula_to_positive f1, formula_to_positive f2)
  | FAnd (f1, f2) -> FAnd (formula_to_positive f1, formula_to_positive f2)
  | FDiamond (p, g) -> FDiamond (program_to_positive p, formula_to_positive g)
  | FBox (p, g) -> FBox (program_to_positive p, formula_to_positive g)
  | FNeg f -> neg_formula_to_positive f
  | _ -> g
and neg_formula_to_positive g =
  match g with
    FProp s -> FNeg (FProp s)
  | FTT -> FFF
  | FFF -> FTT
  | FOr (f1, f2) -> FAnd (neg_formula_to_positive f1, neg_formula_to_positive f2)
  | FAnd (f1, f2) -> FOr (neg_formula_to_positive f1, neg_formula_to_positive f2)
  | FDiamond (p, g) -> FBox (program_to_positive p, neg_formula_to_positive g)
  | FBox (p, g) -> FDiamond (program_to_positive p, neg_formula_to_positive g)
  | FNeg f -> formula_to_positive f
and program_to_positive p =
  match p with
    FAtom p -> FAtom p
  | FConcat (f1, f2) -> FConcat (program_to_positive f1, program_to_positive f2)
  | FChoice (f1, f2) -> FChoice (program_to_positive f1, program_to_positive f2)
  | FStar p -> FStar (program_to_positive p)
  | FQuestion p -> FQuestion (formula_to_positive p)

let rec is_positive g =
  match g with
  | FOr (f1, f2) -> (is_positive f1) && (is_positive f2)
  | FAnd (f1, f2) -> (is_positive f1) && (is_positive f2)
  | FDiamond (p, g) -> (is_positive_program p) && (is_positive g)
  | FBox (p, g) -> (is_positive_program p) && (is_positive g)
  | FNeg (FProp _) -> true
  | FNeg _ -> false
  | _ -> true
and is_positive_program p =
  match p with
    FAtom _ -> true
  | FConcat (f1, f2) -> (is_positive_program f1) && (is_positive_program f2)
  | FChoice (f1, f2) -> (is_positive_program f1) && (is_positive_program f2)
  | FStar p -> is_positive_program p
  | FQuestion p -> is_positive p

type decomposed_pdl_formula_part =
    FIntAtom of bool (* player *)
  | FIntProp of bool * int (* negation * proposition reference *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntModality of bool * int * int (* player * program * formula *)
  
type decomposed_pdl_program_part =
	FIntPAtom of int (* program atom reference *)
  | FIntQuestion of int
  | FIntConcat of int * int
  | FIntChoice of int * int
  | FIntStar of int

type decomposed_pdl_formula =
	int *
	decomposed_pdl_formula_part array *
	(int array) array * (* contains additional links from formula to formulas *)
	decomposed_pdl_program_part array *
	(string * (int * int)) array * (* prop, positive formula, negative formula *)
	string array (* program atoms *)


let indexed_hashtbl_to_array hashtbl def =
	let a = Array.make (Hashtbl.length hashtbl) def in
	Hashtbl.iter (fun x (y, i) -> a.(i) <- (x, y)) hashtbl;
	a;;
	
let index_hashtbl_to_array hashtbl def =
	let a = Array.make (Hashtbl.length hashtbl) def in
	Hashtbl.iter (fun x i -> a.(i) <- x) hashtbl;
	a;;
	
let index_hashtbl_process hashtbl item =
	try
		Hashtbl.find hashtbl item
	with Not_found -> (
		let i = Hashtbl.length hashtbl in
		Hashtbl.add hashtbl item i;
		i
	);;

let normal_form_formula_to_decomposed_formula f link =
	let fml_htbl = Hashtbl.create 10 in
	let full_fml_htbl = Hashtbl.create 10 in
	let prg_htbl = Hashtbl.create 10 in
	let prop_htbl = Hashtbl.create 10 in
	let atom_htbl = Hashtbl.create 10 in
	let link_htbl = Hashtbl.create 10 in
	
	let process_proposition s =
		try
			snd (Hashtbl.find prop_htbl s)
		with Not_found -> (
			let l = Hashtbl.length prop_htbl in
			Hashtbl.add prop_htbl s ((-1, -1), l);
			l
		)
	in
	let update_proposition s b j =
		let ((x, y), l) = Hashtbl.find prop_htbl s in
		Hashtbl.replace prop_htbl s ((if b then (j, y) else (x, j)), l)
	in
	let process_decomp_formula full dec =
		let i = index_hashtbl_process fml_htbl dec in
		try
			let _ = Hashtbl.find full_fml_htbl full in i
		with
			Not_found -> (
				Hashtbl.add full_fml_htbl full i;
				i
			)
	in
	let process_decomp_program = index_hashtbl_process prg_htbl in
	let process_link i a = if not (Hashtbl.mem link_htbl i) then Hashtbl.add link_htbl i a in
	let process_patom = index_hashtbl_process atom_htbl in
	let translate ht = Array.init (Hashtbl.length ht) (Hashtbl.find ht) in

	let rec process_formula f =
		try
			Hashtbl.find full_fml_htbl f
		with
			Not_found ->
				let result = process_formula' f in
				let linkmap = Array.map process_formula (link f) in
				process_link result linkmap;
				result
	and process_formula' z = match z with
		FProp s -> let j = process_decomp_formula z (FIntProp (true, process_proposition s)) in
				   update_proposition s true j;
				   j
	|   FNeg (FProp s) -> let j = process_decomp_formula z (FIntProp (false, process_proposition s)) in
						  update_proposition s false j;
						  j
	|	FTT -> process_decomp_formula z (FIntAtom true)
	|	FFF -> process_decomp_formula z (FIntAtom false)
	|	FAnd (f1, f2) -> process_decomp_formula z (FIntBranch (false, process_formula f1, process_formula f2))
	|	FOr (f1, f2) -> process_decomp_formula z (FIntBranch (true, process_formula f1, process_formula f2))
	|	FDiamond (p, g) -> process_decomp_formula z (FIntModality (true, process_program p, process_formula g))
	|	FBox (p, g) -> process_decomp_formula z (FIntModality (false, process_program p, process_formula g))
	|	_ -> failwith "formula not in normal form"
	and process_program = function
		FAtom s -> process_decomp_program (FIntPAtom (process_patom s))
	|	FConcat (p1, p2) -> process_decomp_program (FIntConcat (process_program p1, process_program p2))
	|	FChoice (p1, p2) -> process_decomp_program (FIntChoice (process_program p1, process_program p2))
	|	FStar p -> process_decomp_program (FIntStar (process_program p))
	|	FQuestion f -> process_decomp_program (FIntQuestion (process_formula f))
	in
	let r = process_formula f in
		(r,
		 index_hashtbl_to_array fml_htbl (FIntAtom true),
		 translate link_htbl,
		 index_hashtbl_to_array prg_htbl (FIntPAtom 0),
		 indexed_hashtbl_to_array prop_htbl ("", (-1, -1)),
		 index_hashtbl_to_array atom_htbl "");;

let sort_decomposed_formula decomp comp_fmls =
	let (root, fmls, lnks, prgs, prop, atms) = decomp in
	let (fmls', ntof, otnf) = ArrayUtils.sort_with_permutations fmls (comp_fmls decomp) in
	let root' = otnf root in
	let n = Array.length fmls' in
	let lnks' = Array.make n [||] in
	let updatef = function
		FIntBranch (b, l, r) -> FIntBranch (b, otnf l, otnf r)
	|	FIntModality (b, p, f) -> FIntModality (b, p, otnf f)
	|	f -> f
	in
	for i = 0 to n - 1 do
		fmls'.(i) <- updatef fmls'.(i);
		lnks'.(i) <- Array.map otnf lnks.(ntof i)
	done;
	let updatep = function
	|	FIntQuestion f -> FIntQuestion (otnf f)
	|	f -> f
	in
	let prgs' = Array.map updatep prgs in
	let prop' = Array.map (fun (s, (f, f')) -> (s, (otnf f, otnf f'))) prop in
	(root', fmls', lnks', prgs', prop', atms);;

	
let get_formula_depth (_,f,_,p,_,_) g =
	let rec len_formula = function
		FIntBranch (_, i, j) -> 1 + max (len_formula f.(i)) (len_formula f.(j))
	|	FIntModality (_, i, j) -> 1 + max (len_program p.(i)) (len_formula f.(j))
	| _ -> 1
	and len_program = function
		FIntQuestion i -> 1 + len_formula f.(i)
	|	FIntConcat (i,j) -> 1 + max (len_program p.(i)) (len_program p.(j))
	|	FIntChoice (i,j) -> 1 + max (len_program p.(i)) (len_program p.(j))
	|	FIntStar i -> 1 + len_program p.(i)
	|	_ -> 1
	in
		len_formula g
	
let rec decomposed_formula_to_formula decomp =
	let (_, fmls, _, _, prop, _) = decomp in
	let rec format ind =
		match fmls.(ind) with
			FIntProp (b, i) -> let (s, _) = prop.(i) in if b then FProp s else FNeg (FProp s)
		|	FIntAtom b -> if b then FTT else FFF
		|	FIntBranch (b, f1, f2) -> if b then FOr (format f1, format f2) else FAnd (format f1, format f2)
		|	FIntModality (b, p, f) -> let q = decomposed_program_to_program decomp p in
		                              if b then FDiamond (q, format f) else FBox (q, format f)
	in
		format
and decomposed_program_to_program decomp =
	let (_, _, _, prgs, _, atom) = decomp in
	let rec format ind =
		match prgs.(ind) with
			FIntPAtom i -> FAtom (atom.(i))
		|	FIntConcat (f1, f2) -> FConcat (format f1, format f2)
		|	FIntChoice (f1, f2) -> FChoice (format f1, format f2)
		|	FIntStar f -> FStar (format f)
		|	FIntQuestion f -> FQuestion (decomposed_formula_to_formula decomp f)
	in
		format;;
	
		
let format_decomposed_formula decomp ind = format_formula (decomposed_formula_to_formula decomp ind);;

let format_decomposed_program decomp ind = format_program (decomposed_program_to_program decomp ind);;

let pdl_formula_link_map = function
	FDiamond (p, g) -> (
		match p with
			FConcat (p1, p2) -> [|FDiamond (p1, FDiamond (p2, g))|]
		|	FChoice (p1, p2) -> [|FDiamond (p1, g); FDiamond (p2, g)|]
		|	FStar p' -> [|FDiamond (p', FDiamond (p, g))|]
		|	_ -> [||]
	)
|	FBox (p, g) -> (
		match p with
			FConcat (p1, p2) -> [|FBox (p1, FBox (p2, g))|]
		|	FChoice (p1, p2) -> [|FBox (p1, g); FBox (p2, g)|]
		|	FStar p' -> [|FBox (p', FBox (p, g))|]
		|	FQuestion f -> [|formula_to_positive (FNeg f)|]
		|	_ -> [||]
	)
|	_ -> [||]

