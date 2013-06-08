let rec range i j = if j >= i then i :: (range (i+1) j) else []

let rec remove_dups = function [] -> []
                             | x::xs -> x::(List.filter (fun y -> y <> x) (remove_dups xs))

type elevator = { position: int;
                  dooropen: bool;
                  requests: int list }

let storeys = ref 0

let show_state el = "[" ^ (String.make el.position '.') ^ (if el.dooropen then "O" else "X") ^ 
                    (String.make (!storeys - el.position - 1) '.') ^ "] " ^
                    String.concat "," (List.map string_of_int el.requests)

(* fair means that any new request will be added to the end of the list (FIFO),
   whereas unfair means that it will be added to the beginning (LIFO) *)
let fair = ref true

let rec insert x = 
  function []    -> [x]
         | y::ys -> if x=y then y::ys else y::(insert x ys)

let successors el = 
  (* any button could be pressed or no requests could come in *)
  let els = el::(List.map (fun i -> { position = el.position; 
                                      dooropen = el.dooropen; 
                                      requests = if !fair 
                                                 then insert i el.requests
                                                 else i::(List.filter (fun j -> j<>i) el.requests)}) 
                          (range 0 (!storeys-1))) in

  (* for each possibility decide in which direction to go *)
  remove_dups (List.map (fun el -> if el.requests <> [] && not el.dooropen then
                                   begin
                                     let r = List.hd el.requests in
                                     let delta = compare r el.position in
                                     let newposition = el.position + delta in
                                     { position = newposition;
                                       dooropen = (newposition=r);
                                       requests = if newposition=r then List.tl el.requests else el.requests }
                                     end
			           else
				     { position = el.position;
				       dooropen = false;
				       requests = el.requests })
		        els)

let isPressed i el = List.mem i el.requests

let isAt i el = (i = el.position)


module TSNode = 
struct
  type t = elevator
  let compare = compare
end ;;

module Coding = Map.Make(TSNode) ;;

module MyInt =
struct 
  type t = int 
  let compare = compare 
end;;

module IntSet = Set.Make(MyInt) ;;
module IntMap = Map.Make(MyInt) ;;


let initialTSNode = {position = 0; dooropen = false; requests = []}

(*
let computeTSSize _ = 
  let numberStoreys = !storeys in
  let numberRequestlists = 
    let rec possibilities n k = if k=0 then 1 else n * (possibilities (n-1) (k-1)) in
    let sum = List.fold_left (+) 0 in
    sum (List.map (fun i -> possibilities numberStoreys i) (range 0 numberStoreys))
  in
  numberStoreys * 2 * numberRequestlists

let show_conf el f = show_state el ^ " |= " ^ string_of_int f
*)

let _ =  

  let arguments = Array.sub Sys.argv 1 ((Array.length Sys.argv)-1) in

  let show_help _ =
(*    print_string (Info.get_title "Elevator Verification Game Generator"); *)
    print_string ("Usage: elevatortsgenerator [-u] n \n\n" ^
		  "       where n = Number of storeys that the elevator serves (>= 1)\n" ^
                  "             the optional argument -u causes the elevator to be unfair " ^
                  "(and thus not have the desired property\n\n")
  in
  if (Array.length arguments > 2 || Array.length arguments < 1) then (show_help (); exit 1);

  (try
    if arguments.(0) = "-u" then
      begin
        fair := false;
        storeys := int_of_string arguments.(1)
      end
    else
      storeys := int_of_string arguments.(0)
  with _ -> (show_help (); exit 1));

(*  let size = computeTSSize () in *)
  let visited = ref IntSet.empty in

  let encoding = ref Coding.empty in
  let index = ref 0 in
  let newIndex _ = 
    let i = !index in 
    incr index; i 
  in

  let encode el =
    try
      Coding.find el !encoding
    with 
      Not_found -> begin
                     let i = newIndex () in
                     encoding := Coding.add el i !encoding;
                     i
                   end
  in
  
  let ts = ref IntMap.empty in
  let el = initialTSNode in
  let todo = ref [ (el, encode el) ] in

  while !todo <> [] do
    let (el,i) = List.hd !todo in
    todo := List.tl !todo;
    if not (IntSet.mem i !visited) then
    begin
      let nextnodes = List.map (fun el -> (el, encode el)) (successors el) in
      let nextnodes_coded = List.map (fun (_,v) -> v) nextnodes in
      let propositions = if isPressed (!storeys -1) el then [ "isPressed" ] else [] in
      let propositions = (if isAt (!storeys -1) el then [ "isAt" ] else []) @ propositions in
      ts := IntMap.add i (nextnodes_coded, propositions) !ts;
      todo := nextnodes @ !todo
    end;
    visited := IntSet.add i !visited
  done;

  let size = !index in
  print_string ("ts " ^ string_of_int size ^ ";\n");
  print_string "start 0;\n";
  IntMap.iter (fun i -> fun (succs,props) -> print_string ((string_of_int i) ^ " " ^
                                                           String.concat "," (List.map string_of_int succs) ^ " " ^
                                                           String.concat "," props ^ ";\n")) !ts
  
