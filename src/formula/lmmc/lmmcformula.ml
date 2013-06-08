open Metaformula;;
open Tcsarray;;


type labelled_mmc_formula = 
	FProp of string
  | FVariable of string
  | FTT
  | FFF
  | FNeg of labelled_mmc_formula
  | FAnd of labelled_mmc_formula * labelled_mmc_formula
  | FOr of labelled_mmc_formula * labelled_mmc_formula
  | FDiamond of string * labelled_mmc_formula
  | FBox of string * labelled_mmc_formula
  | FMu of string * labelled_mmc_formula
  | FNu of string * labelled_mmc_formula

let is_closed f =
  let rec check vars = function
    FVariable x -> List.mem x vars
  | FAnd (f1, f2) -> (check vars f1) && (check vars f2)
  | FOr (f1, f2) -> (check vars f1) && (check vars f2)
  | FNeg f' -> check vars f'
  | FDiamond (l, f') -> check vars f'
  | FBox (l, f') -> check vars f'
  | FMu (x, f') -> check (x::vars) f'
  | FNu (x, f') -> check (x::vars) f'
  | _ -> true
  in
    check [] f;;

let is_guarded f =
  let rec check vars = function
    FVariable x -> not (List.mem x vars)
  | FAnd (f1, f2) -> (check vars f1) && (check vars f2)
  | FOr (f1, f2) -> (check vars f1) && (check vars f2)
  | FNeg f' -> check vars f'
  | FDiamond (l, f') -> check [] f'
  | FBox (l, f') -> check [] f'
  | FMu (x, f') -> check (x::vars) f'
  | FNu (x, f') -> check (x::vars) f'
  | _ -> true
  in
    check [] f;;

let is_guarded_wrt f y =
  let rec check flag = function
    FVariable x -> x <> y || not flag
  | FAnd (f1, f2) -> (check flag f1) && (check flag f2)
  | FOr (f1, f2) -> (check flag f1) && (check flag f2)
  | FNeg f' -> check flag f'
  | FDiamond (_, f') -> check false f'
  | FBox (_, f') -> check false f'
  | FMu (x, f') -> check (flag || x == y) f'
  | FNu (x, f') -> check (flag || x == y) f'
  | _ -> true
  in
    check false f;;

let is_uniquely_bound f =
  let rec check vars = function
    FAnd (f1, f2) -> (check vars f1) && (check vars f2)
  | FOr (f1, f2) -> (check vars f1) && (check vars f2)
  | FNeg f' -> check vars f'
  | FDiamond (l, f') -> check [] f'
  | FBox (l, f') -> check [] f'
  | FMu (x, f') -> not (List.mem x vars) && (check (x::vars) f')
  | FNu (x, f') -> not (List.mem x vars) && (check (x::vars) f')
  | _ -> true
  in
    check [] f;;

let make_uniquely_bound f =
  let rec getName l v i =
    let s = v ^ (string_of_int i) in
    if List.mem s l then getName l v (i + 1) else s
  in
  let rec update vars subst x =
    if (List.mem x vars)
    then let x' = getName vars x 0
         in (x', fun y -> if (y = x) then x' else subst y)
    else (x, subst)
  in
  let rec binder vars subst = function
    FVariable x -> (vars, FVariable (subst x))
  | FMu (x, f') -> let (x', s') = update vars subst x in
                   let (v', f'') = binder (x'::vars) s' f' in
                     (v', FMu (x', f''))
  | FNu (x, f') -> let (x', s') = update vars subst x in
                   let (v', f'') = binder (x'::vars) s' f' in
                     (v', FNu (x', f''))
  | FAnd (f1, f2) -> let (v', f1') = binder vars subst f1 in
                     let (v'', f2') = binder v' subst f2 in
                       (v'', FAnd (f1', f2'))
  | FOr (f1, f2) -> let (v', f1') = binder vars subst f1 in
                    let (v'', f2') = binder v' subst f2 in
                      (v'', FOr (f1', f2'))
  | FNeg f' -> let (v', f'') = binder vars subst f' in
                         (v', FNeg f'')
  | FDiamond(l, f') -> let (v', f'') = binder vars subst f' in
                         (v', FDiamond(l, f''))
  | FBox(l, f')     -> let (v', f'') = binder vars subst f' in
                         (v', FBox(l, f''))
  | f -> (vars, f)
  in
    snd (binder [] (fun x -> x) f);;

let rec eval_metaformula f =
  match f with
    Metaformula.FProp s -> FProp (identifier_to_str s)
  | Metaformula.FVariable s -> FVariable (identifier_to_str s)
  | Metaformula.Ftt -> FTT
  | Metaformula.Fff -> FFF
  | Metaformula.FNeg f -> FNeg (eval_metaformula f)
  | Metaformula.FOr (f1, f2) -> FOr (eval_metaformula f1, eval_metaformula f2)
  | Metaformula.FAnd (f1, f2) -> FAnd (eval_metaformula f1, eval_metaformula f2)
  | Metaformula.FLabelledOp ("<>", l, f) -> FDiamond (as_identifier_to_str l, eval_metaformula f)
  | Metaformula.FLabelledOp ("[]", l, f) -> FBox (as_identifier_to_str l, eval_metaformula f)
  | Metaformula.FQuantor ("mu", x, f) -> FMu (identifier_to_str x, eval_metaformula f)
  | Metaformula.FQuantor ("nu", x, f) -> FNu (identifier_to_str x, eval_metaformula f)
  | _ -> failwith("Formula is not able to be interpreted as LMMC");;
  
  
let formula_length f =
	let rec helper a = function
		FNeg f -> helper (a + 1) f
	|	FAnd (f, g) -> helper (helper (a + 1) f) g
	|	FOr (f, g) -> helper (helper (a + 1) f) g
	|	FDiamond (_, f) -> helper (a + 2) f
	|	FBox (_, f) -> helper (a + 2) f
	|	FMu (_, f) -> helper (a + 2) f
	|	FNu (_, f) -> helper (a + 2) f
	|	_ -> a + 1
	in
		helper 0 f;;

let formula_height f =
	let rec helper a = function
		FNeg f -> helper (a + 1) f
	|	FAnd (f, g) -> max (helper (a + 1) f) (helper (a + 1) g)
	|	FOr (f, g) -> max (helper (a + 1) f) (helper (a + 1) g)
	|	FDiamond (_, f) -> helper (a + 1) f
	|	FBox (_, f) -> helper (a + 1) f
	|	FMu (_, f) -> helper (a + 1) f
	|	FNu (_, f) -> helper (a + 1) f
	|	_ -> a + 1
	in
		helper 0 f;;

let formula_variable_occs f =
	let rec helper (g, u) ung = function
    	FNeg f -> helper (g, u) ung f
    |   FAnd (a, b) -> helper (helper (g, u) ung a) ung b
    |   FOr (a, b) -> helper (helper (g, u) ung a) ung b
    |	FDiamond (_, f) -> helper (g, u) [] f
    |	FBox (_, f) -> helper (g, u) [] f
    |	FMu (x, f) -> helper (g, u) (x::ung) f
    |	FNu (x, f) -> helper (g, u) (x::ung) f
    |   FVariable x -> if List.mem x ung then (g, u+1) else (g+1, u) 
    |   _ -> (g, u)
 in
    let (g, u) = helper (0, 0) [] f in
    (g+u, g, u)


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
  | FVariable v -> v
  | FTT -> "tt"
  | FFF -> "ff"
  | FNeg f' -> unaryr f' "!"
  | FAnd (f1, f2) -> n_nary (and_collect f) " & "
  | FOr (f1, f2) -> n_nary (or_collect f) " | "
  | FDiamond (l, f') -> unaryr f' ("<" ^ l ^ ">")
  | FBox (l, f') -> unaryr f' ("[" ^ l ^ "]")
  | FMu (v, f') -> unaryr f' ("mu " ^ v ^ ".")
  | FNu (v, f') -> unaryr f' ("nu " ^ v ^ ".")
in
  match f with
  | FOr (FNeg g1, g2) -> binary g1 g2 " ==> "
  | FOr (FAnd(g1, g2), FAnd(FNeg (g1'), FNeg (g2'))) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <==> " else default_format f
  | FOr (FAnd(FNeg (g1), FNeg (g2)), FAnd(g1', g2')) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <==> " else default_format f
  | FOr (FAnd(g1, FNeg (g2)), FAnd(FNeg (g1'), g2')) -> if (g1 = g1') && (g2 = g2') then binary g1 g2 " <~~> " else default_format f
  | f -> default_format f;;

	
let format_formula_as_tree f =
	let rec helper indent = function
    FProp p -> indent ^ p
  | FVariable v -> indent ^ v
  | FTT -> indent ^ "tt"
  | FFF -> indent ^ "ff"
  | FNeg f' -> indent ^ "!" ^ "\n" ^ helper (indent ^ "  ") f'
  | FAnd (f1, f2) -> indent ^ "And" ^ "\n" ^ helper (indent ^ "  ") f1 ^ "\n" ^ helper (indent ^ "  ") f2
  | FOr (f1, f2) -> indent ^ "Or" ^ "\n" ^ helper (indent ^ "  ") f1 ^ "\n" ^ helper (indent ^ "  ") f2
  | FDiamond (l, f') -> indent ^ "<" ^ l ^ ">" ^ "\n" ^ helper (indent ^ "  ") f'
  | FBox (l, f') -> indent ^ "[" ^ l ^ "]" ^ "\n" ^ helper (indent ^ "  ") f'
  | FMu (v, f') -> indent ^ "mu " ^ v ^ "\n" ^ helper (indent ^ "  ") f'
  | FNu (v, f') -> indent ^ "nu " ^ v ^ "\n" ^ helper (indent ^ "  ") f'
	in
		helper "" f		
			  
let rec subst_formula f sub tar =
  if f = sub then tar
  else match f with
           FNeg f' -> FNeg (subst_formula f' sub tar)
         | FAnd (f1, f2) -> FAnd (subst_formula f1 sub tar, subst_formula f2 sub tar)
         | FOr (f1, f2) -> FOr (subst_formula f1 sub tar, subst_formula f2 sub tar)
         | FDiamond (l, f') -> FDiamond (l, subst_formula f' sub tar)
         | FBox (l, f') -> FBox (l, subst_formula f' sub tar)
         | FMu (x, f') -> FMu (x, subst_formula f' sub tar)
         | FNu (x, f') -> FNu (x, subst_formula f' sub tar)
         | _ -> f;;

let rec formula_to_positive g =
  match g with
  | FOr (f1, f2) -> FOr (formula_to_positive f1, formula_to_positive f2)
  | FAnd (f1, f2) -> FAnd (formula_to_positive f1, formula_to_positive f2)
  | FDiamond (l, f) -> FDiamond (l, formula_to_positive f)
  | FBox (l, f) -> FBox (l, formula_to_positive f)
  | FMu (x, f) -> FMu (x, formula_to_positive f)
  | FNu (x, f) -> FNu (x, formula_to_positive f)
  | FNeg f -> neg_formula_to_positive f
  | _ -> g
and neg_formula_to_positive g =
  match g with
    FProp s -> FNeg (FProp s)
  | FVariable x -> failwith ("Variable " ^ x ^ " is under an odd number of negations!")
  | FTT -> FFF
  | FFF -> FTT
  | FOr (f1, f2) -> FAnd (neg_formula_to_positive f1, neg_formula_to_positive f2)
  | FAnd (f1, f2) -> FOr (neg_formula_to_positive f1, neg_formula_to_positive f2)
  | FDiamond (l, f) -> FBox (l, neg_formula_to_positive f)
  | FBox (l, f) -> FDiamond (l, neg_formula_to_positive f)
  | FMu (v, f) -> FNu (v, neg_formula_to_positive (subst_formula f (FVariable v) (FNeg (FVariable v))))
  | FNu (v, f) -> FMu (v, neg_formula_to_positive (subst_formula f (FVariable v) (FNeg (FVariable v))))
  | FNeg f -> formula_to_positive f;;

let rec is_positive g =
  match g with
  | FOr (f1, f2) -> (is_positive f1) && (is_positive f2)
  | FAnd (f1, f2) -> (is_positive f1) && (is_positive f2)
  | FDiamond (l, f) -> is_positive f
  | FBox (l, f) -> is_positive f
  | FMu (x, f) -> is_positive f
  | FNu (x, f) -> is_positive f
  | FNeg (FProp _) -> true
  | FNeg _ -> false
  | _ -> true;;

let rec guarded_flatten f var pred = match f with
    FOr (f1, f2) -> FOr ((guarded_flatten f1 var pred), (guarded_flatten f2 var pred))
  | FAnd (f1, f2) -> FAnd ((guarded_flatten f1 var pred), (guarded_flatten f2 var pred))
  | FVariable v -> if v = var then pred else f
  | FNeg f' -> FNeg (guarded_flatten f' var pred)
  | _ -> f;;

let rec guarded_transform f =
  let rec flat v f pred = guarded_flatten (guarded_transform' f true) v pred
  and guarded_transform' f t2 = match f with
    FOr (f1, f2) -> FOr ((guarded_transform' f1 t2), (guarded_transform' f2 t2))
  | FAnd (f1, f2) -> FAnd ((guarded_transform' f1 t2), (guarded_transform' f2 t2))
  | FNeg f' -> FNeg (guarded_transform' f' t2)
  | FDiamond (l, g) -> FDiamond (l, guarded_transform' g false)
  | FBox (l, g) -> FBox (l, guarded_transform' g false)
  | FMu (v, g) ->
      let fl = flat v g FFF in
      let mufl = FMu (v, fl) in
        if t2 then subst_formula fl (FVariable v) mufl else mufl
  | FNu (v, g) ->
      let fl = flat v g FTT in
      let nufl = FNu (v, fl) in
        if t2 then subst_formula fl (FVariable v) nufl else nufl
  | _ -> f
in
  guarded_transform' f false;;


type decomposed_labelled_mmc_formula_part =
    FIntAtom of bool (* player *)
  | FIntVariable of int (* variable reference *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntModality of bool * int * int (* player * label reference * formula *)
  | FIntProp of bool * int (* negation * proposition reference *)

type mmc_proposition_data = int * int (* prop, positive formula, negative formula *)

type mmc_variable_data = bool * int * bool * int (* player, fixed point priority * formula *)

type mmc_label_data = string
  
type decomposed_labelled_mmc_formula =
	int *
	decomposed_labelled_mmc_formula_part array *
	(string * mmc_proposition_data) array *
	(string * mmc_variable_data) array *
	mmc_label_data array

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

let normal_form_formula_to_decomposed_formula f =
	let fml_htbl = Hashtbl.create 10 in
	let prop_htbl = Hashtbl.create 10 in
	let var_htbl = Hashtbl.create 10 in
	let label_htbl = Hashtbl.create 10 in
	
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
	let process_decomp_formula = index_hashtbl_process fml_htbl in
	let find_variable x = snd (Hashtbl.find var_htbl x) in
	let process_label = index_hashtbl_process label_htbl in
	let add_variable x =
		let l = Hashtbl.length var_htbl in
		Hashtbl.add var_htbl x ((false, -1, false, -1), l);
		l
	in
	let set_variable_data x pl pr gu j = Hashtbl.replace var_htbl x ((pl, pr, gu, j), find_variable x) in

	let rec process_formula = function
		FProp s -> let i = process_proposition s in
				   let j = process_decomp_formula (FIntProp (true, i)) in
				   update_proposition s true j;
				   (j, 0)
	|   FNeg (FProp s) -> let i = process_proposition s in
					      let j = process_decomp_formula (FIntProp (false, i)) in
						  update_proposition s false j;
						  (j, 0)
	|	FVariable x -> let i = find_variable x in
					   let j = process_decomp_formula (FIntVariable i) in
					   (j, 0)
	|	FTT -> let j = process_decomp_formula (FIntAtom true) in
			   (j, 0)
	|	FFF -> let j = process_decomp_formula (FIntAtom false) in
			   (j, 0)
	|	FAnd (f1, f2) -> let (j1, pr1) = process_formula f1 in
						 let (j2, pr2) = process_formula f2 in
						 let j = process_decomp_formula (FIntBranch (false, j1, j2)) in
						 (j, max pr1 pr2)
	|	FOr (f1, f2) -> let (j1, pr1) = process_formula f1 in
						let (j2, pr2) = process_formula f2 in
						let j = process_decomp_formula (FIntBranch (true, j1, j2)) in
						(j, max pr1 pr2)
	|	FDiamond (l, f) -> let (j1, pr1) = process_formula f in
						   let i = process_label l in
						   let j = process_decomp_formula (FIntModality (true, i, j1)) in
						   (j, pr1)
	|	FBox (l, f) -> let (j1, pr1) = process_formula f in
					   let i = process_label l in
					   let j = process_decomp_formula (FIntModality (false, i, j1)) in
					   (j, pr1)
	|	FMu (x, f) -> let i = add_variable x in
					  let (j1, pr1) = process_formula f in
					  let pr = pr1 + (if pr1 mod 2 = 1 then 2 else 1) in
					  let j = process_decomp_formula (FIntVariable i) in
					  set_variable_data x false pr (is_guarded_wrt (FMu (x,f)) x) j1;
					  (j, pr)
	|	FNu (x, f) -> let i = add_variable x in
					  let (j1, pr1) = process_formula f in
					  let pr = pr1 + (if pr1 mod 2 = 1 then 1 else 2) in
					  let j = process_decomp_formula (FIntVariable i) in
					  set_variable_data x true pr (is_guarded_wrt (FNu (x,f)) x) j1;
					  (j, pr)
	|	_ -> failwith "formula not in normal form"
	in
		let (i, _) = process_formula f in
		(i,
		 index_hashtbl_to_array fml_htbl (FIntAtom true),
		 indexed_hashtbl_to_array prop_htbl ("", (-1, -1)),
		 indexed_hashtbl_to_array var_htbl ("", (true, 0, false, 0)),
		 index_hashtbl_to_array label_htbl "");;

let sort_decomposed_formula decomp comp =
	let (root, fmls, prop, vars, lbls) = decomp in
	let (fmls', _, otn) = ArrayUtils.sort_with_permutations fmls (comp decomp) in
	let root' = otn root in
	let n = Array.length fmls' in
	let update = function
		FIntBranch (b, l, r) -> FIntBranch (b, otn l, otn r)
	|	FIntModality (b, l, f) -> FIntModality (b, l, otn f)
	|	f -> f
	in
	for i = 0 to n - 1 do
		fmls'.(i) <- update fmls'.(i)
	done;
	let prop' = Array.map (fun (s, (f, f')) -> (s, (otn f, otn f'))) prop in
	let vars' = Array.map (fun (s, (b, p, g, f)) -> (s, (b, p, g, otn f))) vars in
	(root', fmls', prop', vars', lbls);;

let decomposed_formula_subformula_cardinality (_, fmls, _, _, _) =
	Array.length fmls

let get_formula_depth (_,f,_,_,_) p =
	let rec len = function
		FIntModality (_, _, i) -> 1 + len f.(i)
	|	FIntBranch (_, i, j) -> 1 + max (len f.(i)) (len f.(j))
	| _ -> 1
	in
		len p

let decomposed_formula_to_formula decomp =
	let (_, fmls, prop, vars, lbls) = decomp in
	let fnd = Array.make (Array.length vars) false in
	let rec format ind =
		match fmls.(ind) with
			FIntProp (b, i) -> let (s, _) = prop.(i) in if b then FProp s else FNeg (FProp s)
		|	FIntVariable i ->
				if fnd.(i)
				then FVariable (fst vars.(i))
				else (
					let (x, (b, _, _, j)) = vars.(i) in
					fnd.(i) <- true;
					if b then FNu (x, format j) else FMu (x, format j)
				)
		|	FIntAtom b -> if b then FTT else FFF
		|	FIntBranch (b, f1, f2) -> if b then FOr (format f1, format f2) else FAnd (format f1, format f2)
		|	FIntModality (b, l, f) -> if b then FDiamond (lbls.(l), format f) else FBox (lbls.(l), format f)
	in
		format;;

let format_decomposed_formula decomp ind = format_formula (decomposed_formula_to_formula decomp ind);;