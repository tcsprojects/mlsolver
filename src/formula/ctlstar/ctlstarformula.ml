open Metaformula;;
open Tcsarray;;
open Tcslist;;
open Tcsset;;


type ctlstar_formula = 
	FProp of string
  | FTT
  | FFF
  | FNeg of ctlstar_formula
  | FAnd of ctlstar_formula * ctlstar_formula
  | FOr of ctlstar_formula * ctlstar_formula
  | FNext of ctlstar_formula
  | FExists of ctlstar_formula
  | FForall of ctlstar_formula
  | FRelease of ctlstar_formula *ctlstar_formula
  | FUntil of ctlstar_formula * ctlstar_formula

let rec eval_metaformula f =
  match f with
    Metaformula.FProp s -> FProp (identifier_to_str s)
  | Metaformula.Ftt -> FTT
  | Metaformula.Fff -> FFF
  | Metaformula.FNeg f -> FNeg (eval_metaformula f)
  | Metaformula.FOr (f1, f2) -> FOr (eval_metaformula f1, eval_metaformula f2)
  | Metaformula.FAnd (f1, f2) -> FAnd (eval_metaformula f1, eval_metaformula f2)
  | Metaformula.FUnaryOp ("X", f) -> FNext (eval_metaformula f)
  | Metaformula.FUnaryOp ("F", f) -> FUntil (FTT, eval_metaformula f)
  | Metaformula.FUnaryOp ("G", f) -> FRelease (FFF, eval_metaformula f)
  | Metaformula.FUnaryOp ("E", f) -> FExists (eval_metaformula f)
  | Metaformula.FUnaryOp ("A", f) -> FForall (eval_metaformula f)
  | Metaformula.FBinaryOp ("U", f, g) -> FUntil (eval_metaformula f, eval_metaformula g)
  | Metaformula.FBinaryOp ("R", f, g) -> FRelease (eval_metaformula f, eval_metaformula g)
  | _ -> failwith("Formula cannot be interpreted in CTL*");;
  
let is_ctl_formula =
	let rec helper = function
		FProp _ -> true
	|	FTT -> true
	|	FFF -> true
	|	FNeg g -> helper g
	|	FOr (g, h) -> helper g && helper h
	|	FAnd (g, h) -> helper g && helper h
	|	FExists g -> helper' g
	|	FForall g -> helper' g
	|	_ -> false
	and helper' = function
		FNext g -> helper g
	|	FUntil (g, h) -> helper g && helper h
	|	FRelease (g, h) -> helper g && helper h
	|	_ -> false
	in
		helper

let formula_length f =
	let rec helper a = function
		FNeg f -> helper (a + 1) f
	|	FAnd (f, g) -> helper (helper (a + 1) f) g
	|	FOr (f, g) -> helper (helper (a + 1) f) g
	|	FNext f -> helper (a + 1) f
	|	FExists f -> helper (a + 1) f
	|	FForall f -> helper (a + 1) f
	|	FRelease (f, g) -> helper (helper (a + 1) f) g
	|	FUntil (f, g) -> helper (helper (a + 1) f) g
	|	_ -> a + 1
	in
		helper 0 f;;
  
let rec and_collect f =
  match f with
    FAnd (f1, f2) -> List.append (and_collect f1) (and_collect f2)
  | f -> [f]

let rec or_collect f =
  match f with
    FOr (f1, f2) -> List.append (or_collect f1) (or_collect f2)
  | f -> [f]

let rec format_with_brackets f =
  let form = format_formula f in
  match f with
	FAnd (f1, f2) -> "(" ^ form ^ ")"
  | FOr (f1, f2) -> "(" ^ form ^ ")"
  | FRelease (f, g) -> if f = FFF then form else "(" ^ form ^ ")"
  | FUntil (f, g) -> if f = FTT then form else "(" ^ form ^ ")"
  | f -> form
and format_formula f =
  let unaryr f s = s ^ (format_with_brackets f) in
  let binary f1 f2 s = (format_with_brackets f1) ^ s ^ (format_with_brackets f2) in
  let n_nary list s =
    let n_nary_cb a i = a ^ s ^ (format_with_brackets i)
  in
    List.fold_left n_nary_cb (format_with_brackets (List.hd list)) (List.tl list)
  in
  let default_format f = match f with
    FProp p -> p
  | FTT -> "tt"
  | FFF -> "ff"
  | FNeg f' -> unaryr f' "!"
  | FAnd (f1, f2) -> n_nary (and_collect f) " & "
  | FOr (f1, f2) -> n_nary (or_collect f) " | "
  | FNext f' -> unaryr f' "X"
  | FExists f' -> unaryr f' "E"
  | FForall f' -> unaryr f' "A"
  | FUntil (f', g') -> binary f' g' "U"
  | FRelease (f', g') -> binary f' g' "R"
in
  match f with
  | FOr (FNeg g1, g2) -> binary g1 g2 " ==> "
  | FOr (FAnd(g1, g2), FAnd(FNeg (g1'), FNeg (g2'))) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <==> " else default_format f
  | FOr (FAnd(FNeg (g1), FNeg (g2)), FAnd(g1', g2')) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <==> " else default_format f
  | FOr (FAnd(g1, FNeg (g2)), FAnd(FNeg (g1'), g2')) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <~~> " else default_format f
  | FUntil (FTT, g) -> unaryr g "F"
  | FRelease (FFF, g) -> unaryr g "G"
  | f -> default_format f;;
  
let rec formula_to_positive g =
  match g with
  | FOr (f1, f2) -> FOr (formula_to_positive f1, formula_to_positive f2)
  | FAnd (f1, f2) -> FAnd (formula_to_positive f1, formula_to_positive f2)
  | FExists f -> FExists (formula_to_positive f)
  | FForall f -> FForall (formula_to_positive f)
  | FUntil (f, g) -> FUntil (formula_to_positive f, formula_to_positive g)
  | FRelease (f, g) -> FRelease (formula_to_positive f, formula_to_positive g)
  | FNext f -> FNext (formula_to_positive f)
  | FNeg f -> neg_formula_to_positive f
  | _ -> g
and neg_formula_to_positive g =
  match g with
    FProp s -> FNeg (FProp s)
  | FTT -> FFF
  | FFF -> FTT
  | FOr (f1, f2) -> FAnd (neg_formula_to_positive f1, neg_formula_to_positive f2)
  | FAnd (f1, f2) -> FOr (neg_formula_to_positive f1, neg_formula_to_positive f2)
  | FExists f -> FForall (neg_formula_to_positive f)
  | FForall f -> FExists (neg_formula_to_positive f)
  | FUntil (f, g) -> FRelease (neg_formula_to_positive f, neg_formula_to_positive g)
  | FRelease (f, g) -> FUntil (neg_formula_to_positive f, neg_formula_to_positive g)
  | FNext f -> FNext (neg_formula_to_positive f)
  | FNeg f -> formula_to_positive f;;

let rec is_positive g =
  match g with
  | FOr (f1, f2) -> (is_positive f1) && (is_positive f2)
  | FAnd (f1, f2) -> (is_positive f1) && (is_positive f2)
  | FExists f -> is_positive f
  | FForall f -> is_positive f
  | FUntil (f, g) -> (is_positive f) && (is_positive g)
  | FRelease (f, g) -> (is_positive f) && (is_positive g)
  | FNext f -> is_positive f
  | FNeg (FProp _) -> true
  | FNeg _ -> false
  | _ -> true;;

type decomposed_ctlstar_formula_part =
    FIntAtom of bool (* player *)
  | FIntProp of bool * int (* negation * proposition reference *)
  | FIntNext of int (* formula *)
  | FIntPath of bool * int (* player * formula *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntFixpoint of bool * int * int (* player * left formula * right formula *)

type decomposed_ctlstar_formula =
	int *
	decomposed_ctlstar_formula_part array *
	(int array) array * (* contains additional links from formula to formulas *)
	(string * (int * int)) array (* prop, positive formula, negative formula *)

	
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
	let prop_htbl = Hashtbl.create 10 in
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
	let process_link i a = if not (Hashtbl.mem link_htbl i) then Hashtbl.add link_htbl i a in
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
	|	FNext f -> process_decomp_formula z (FIntNext (process_formula f))
	|	FExists f -> process_decomp_formula z (FIntPath (true, process_formula f))
	|	FForall f -> process_decomp_formula z (FIntPath (false, process_formula f))
	|	FUntil (f1, f2) -> process_decomp_formula z (FIntFixpoint (true, process_formula f1, process_formula f2))
	|	FRelease (f1, f2) -> process_decomp_formula z (FIntFixpoint (false, process_formula f1, process_formula f2))
	|	_ -> failwith "formula not in normal form"
	in
	let r = process_formula f in
		(r,
		 index_hashtbl_to_array fml_htbl (FIntAtom true),
		 translate link_htbl,
		 indexed_hashtbl_to_array prop_htbl ("", (-1, -1)));;

let sort_decomposed_formula decomp comp_fmls =
	let (root, fmls, lnks, prop) = decomp in
	let (fmls', ntof, otnf) = ArrayUtils.sort_with_permutations fmls (comp_fmls decomp) in
	let root' = otnf root in
	let n = Array.length fmls' in
	let lnks' = Array.make n [||] in
	let updatef = function
		FIntBranch (b, l, r) -> FIntBranch (b, otnf l, otnf r)
	| 	FIntNext l -> FIntNext (otnf l)
	| 	FIntPath (b, l) -> FIntPath (b, otnf l)
	| 	FIntFixpoint (b, l, r) -> FIntFixpoint (b, otnf l, otnf r)
	|	f -> f
	in
	for i = 0 to n - 1 do
		fmls'.(i) <- updatef fmls'.(i);
		lnks'.(i) <- Array.map otnf lnks.(ntof i)
	done;
	let prop' = Array.map (fun (s, (f, f')) -> (s, (otnf f, otnf f'))) prop in
	(root', fmls', lnks', prop');;

let get_formula_depth (_,f,_,_) p =
	let rec len = function
		FIntNext i -> 1 + len f.(i)
	|	FIntPath (_, i) -> 1 + len f.(i)
	|	FIntBranch (_, i, j) -> 1 + max (len f.(i)) (len f.(j))
	|	FIntFixpoint (_, i, j) -> 1 + max (len f.(i)) (len f.(j))
	| _ -> 1
	in
		len p

let decomposed_formula_to_formula decomp =
	let (_, fmls, _, prop) = decomp in
	let rec format ind =
		match fmls.(ind) with
			FIntProp (b, i) -> let (s, _) = prop.(i) in if b then FProp s else FNeg (FProp s)
		|	FIntAtom b -> if b then FTT else FFF
		|	FIntBranch (b, f1, f2) -> if b then FOr (format f1, format f2) else FAnd (format f1, format f2)
		|	FIntNext f -> FNext (format f)
		|	FIntPath (b, f) -> if b then FExists (format f) else FForall (format f)
		|	FIntFixpoint (b, f, g) -> if b then FUntil (format f, format g) else FRelease (format f, format g)
	in
		format;;
		
let format_decomposed_formula decomp ind = format_formula (decomposed_formula_to_formula decomp ind);;

type block_type = EBlock | ABlock

type block = block_type * 
             int list *     (* non-modal formulas *)
             int list       (* modal formulas *)

let format_block ((_, fmls, lnks, _) as dcp) (kind, nonmods, mods) =
	let fmt = format_decomposed_formula dcp in
	(match kind with EBlock -> "E" | ABlock -> "A") ^ "(" ^
	ListUtils.format (fun s -> s) ((List.map fmt nonmods) @ (List.map (fun i -> "X" ^ fmt i) mods))
	^ ")";;

let add_to_block (_, fmls, _, _) (kind, nonmods, mods) forms =
	let nonmods = ref (TreeSet.of_list_def nonmods) in
	let mods = ref (TreeSet.of_list_def mods) in
	List.iter (fun f ->
		match fmls.(f) with
			FIntNext i -> mods := TreeSet.add i !mods
		|	_ -> nonmods := TreeSet.add f !nonmods
	) forms;
	(kind, TreeSet.elements !nonmods, TreeSet.elements !mods)

let ctlstar_formula_link_map = function
	FUntil (f, g) -> [|FNext (FUntil (f, g))|]
|	FRelease (f, g) -> [|FNext (FRelease (f, g))|]
|	_ -> [||]

