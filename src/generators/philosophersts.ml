let rec range i j = if j >= i then i :: (range (i+1) j) else []

let rec remove_dups = function [] -> []
                             | x::xs -> x::(List.filter (fun y -> y <> x) (remove_dups xs))

(* a table with n philosophers is modelled by an array storing
   the position of the forks:
   * 0 means on the table
   * -1 means fork is held by left philosopher
   * 1 means fork is held by right philosopher *)
 
type table = int array

let philosophers = ref 0

let show_state tab = String.concat ""
                       (List.map (function 0  -> "." 
                                         | -1 -> "<" 
                                         | 1  -> ">" 
                                         | _ -> failwith "unknown fork position")
                                 (Array.to_list tab))

(*
let rec insert x = 
  function []    -> [x]
         | y::ys -> if x=y then y::ys else y::(insert x ys)
*)

let successors tab = 
  (* in one step one fork can be picked up or placed down *)
  let tabs = ref [] in

  for i=0 to !philosophers-1 do
    match tab.(i) with
        0  -> begin
                let a = Array.copy tab in
                a.(i) <- -1;
                tabs := a :: !tabs;
                let a = Array.copy tab in
                a.(i) <- 1;
                tabs := a :: !tabs
              end
      | -1 -> begin
                let a = Array.copy tab in
                a.(i) <- 0;
                tabs := a :: !tabs
              end
      | 1  -> begin
                let a = Array.copy tab in
                a.(i) <- 0;
                tabs := a :: !tabs
              end
      | _  -> failwith "unknown fork position"
  done;

  !tabs

let hasLeftFork i tab = tab.(i) = 1
let hasRightFork i tab = tab.((i + 1) mod !philosophers) = -1


module TSNode = 
struct
  type t = table
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


let initialTSNode _ = Array.make !philosophers 0 

let _ =  

  let arguments = Array.sub Sys.argv 1 ((Array.length Sys.argv)-1) in

  let show_help _ =
(*    print_string (Info.get_title "Elevator Verification Game Generator"); *)
    print_string ("Usage: philosopherstsgenerator n \n\n" ^
		  "       where n = number of philosophers at the table (>= 1)\n\n")
  in
  if Array.length arguments <> 1 then (show_help (); exit 1);

  (try
      philosophers := int_of_string arguments.(0)
  with _ -> (show_help (); exit 1));

  let visited = ref IntSet.empty in

  let encoding = ref Coding.empty in
  let index = ref 0 in
  let newIndex _ = 
    let i = !index in 
    incr index; i 
  in

  let encode tab =
    try
      Coding.find tab !encoding
    with 
      Not_found -> begin
                     let i = newIndex () in
                     encoding := Coding.add tab i !encoding;
                     i
                   end
  in
  
  let ts = ref IntMap.empty in
  let tab = initialTSNode () in
  let todo = ref [ (tab, encode tab) ] in
  let props = ref [] in

  while !todo <> [] do
    let (tab,i) = List.hd !todo in
    todo := List.tl !todo;
    if not (IntSet.mem i !visited) then
    begin
      let nextnodes = List.map (fun tab -> (tab, encode tab)) (successors tab) in
      let nextnodes_coded = List.map (fun (_,v) -> v) nextnodes in

      props := [];
      for i=0 to !philosophers-1 do
        if hasLeftFork i tab && hasRightFork i tab then 
           props := ("eats" ^ string_of_int i) :: !props
      done;
      ts := IntMap.add i (nextnodes_coded, !props) !ts;
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
  
