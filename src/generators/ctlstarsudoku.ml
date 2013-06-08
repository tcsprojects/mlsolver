let bits n = 
  let rec blog i = function 1 -> i
                          | n -> blog (i+1) (n/2)
  in
  blog 1 n

(*
let rec repeat s = function 0 -> []
                          | n -> s::(repeat s (n-1))
*)

let rec power_of_two n =
  if n==0 then 1 
  else if n mod 2 == 0 then let p = power_of_two (n/2) in p*p
       else 2*(power_of_two (n-1))

(*
let how_many p l = List.fold_left (fun n -> fun x -> if p x then n+1 else n) 0 l
*)

(*
let rec range i j = if j < i then [] else i::(range (i+1) j)
*)

let rec printloop i j separator command = 
  if j < i then () 
  else if j=i then command i 
       else begin
             command i; 
             print_string separator; 
             printloop (i+1) j separator command
            end


type proposition = R  
                 | C  
                 | X  
                 | Y  
                 | V 

let prop p i =
   match p with
         R -> "r" ^ string_of_int i
       | C -> "c" ^ string_of_int i
       | X -> "x" ^ string_of_int i
       | Y -> "y" ^ string_of_int i
       | V -> "v" ^ string_of_int i

let help_message _ =
  print_string ("Encoding generalised Sudoku in a CTL* formula.\n\nUsage:\n\n  ctlstarsudokugenerator <dimension>" ^
               "\n\nwhere\n  <dimension> is a positive natural number n determining the size n^2 x n^2 of the " ^
               "matrix.\n\n");
  exit 1

let _ =
  let n = try
            int_of_string Sys.argv.(1) 
          with _ -> help_message ()
  in

  let bn = if n=1 then 0 else (bits (n-1))-1 in
  let bnn = if n=1 then 0 else (bits (n*n))-1 in

  print_string "#phi := ";
  (* erster Zustand repräsentiert Reihe 1, Spalte 1, Block (1,1) *)
  print_string "z & ";
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop R i ^ " & !" ^ prop C i ^ " & !" ^ prop X i ^ " & !" ^ 
                                               prop Y i));
  print_string " &\n";
  
  let rec at_most t i n =
    if i=0 then 
      begin
        if n mod 2 = 0 then "!" ^ prop t 0 else "tt"
      end
    else
      begin
        if n land (power_of_two i) > 0 then "(!" ^ prop t i ^ " | " ^ at_most t (i-1) n ^ ")"
        else "!" ^ prop t i ^ " & " ^ at_most t (i-1) n 
      end
  in
  (* es gibt nur die Reihen 0,..,n-1 *)
  print_string ("A G ((" ^ at_most R bn (n-1) ^ ") & (" ^ at_most C bn (n-1) ^ ") & (" ^ 
                at_most X bn (n-1) ^ ") & (" ^ at_most Y bn (n-1) ^ ")) &\n");

  let rec exactly t i n = 
    if i=0 then 
      begin
        (if n mod 2 = 0 then "!" else "") ^ prop t 0 
      end
    else
      begin
        (if n land (power_of_two i) > 0 then "" else "!") ^ prop t i ^ " & " ^ exactly t (i-1) n
      end
  in


  (* Struktur der Proxy-Zustaende beschreiben *)
  print_string ("A G( (z ==> (E X z & E X !z)) & (!z ==> A X !z)) & \n");
  print_string "A G(";
  printloop 0 bn " & " 
    (fun i -> let s = prop R i in
              print_string ("((" ^ s ^ " ==> A X (z | " ^ s ^ ")) & (!" ^ s ^ " ==> A X (z | !" ^ s ^ "))) & ");
              let s = prop C i in
              print_string ("((" ^ s ^ " ==> A X (z | " ^ s ^ ")) & (!" ^ s ^ " ==> A X (z | !" ^ s ^ "))) & ");
              let s = prop X i in
              print_string ("((" ^ s ^ " ==> A X (z | " ^ s ^ ")) & (!" ^ s ^ " ==> A X (z | !" ^ s ^ "))) & ");
              let s = prop Y i in
              print_string ("((" ^ s ^ " ==> A X (z | " ^ s ^ ")) & (!" ^ s ^ " ==> A X (z | !" ^ s ^ ")))"));
  print_string ") &\n"; 
  print_string "A G(";
  printloop 0 bnn " & " 
    (fun i -> let s = prop V i in
              print_string ("((" ^ s ^ " ==> A X (z | " ^ s ^ ")) & (!" ^ s ^ " ==> A X (z | !" ^ s ^ ")))"));
  print_string ") &\n"; 


  let exX = exactly X bn (n-1) in
  let exY = exactly Y bn (n-1) in
  let exR = exactly R bn (n-1) in
  let exC = exactly C bn (n-1) in

  (* es wird immer um eine Reihe mod n weitergezählt *)
  print_string ("A G((" ^ exR ^ " ==> A X(!z | ");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop R i));
  print_string (")) & (!(" ^ exR ^ ") ==> ((" ^ prop R 0 ^ " ==> A X(!z | !" ^ prop R 0 ^ ")) & (!" ^ prop R 0 ^
                " ==> A X(!z | " ^ prop R 0 ^ "))");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop R i in
              let pi' = prop R (i-1) in
              print_string ("(((" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (!" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | " ^ pi ^ ")) & ");
              print_string ("(((!" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | !" ^ pi ^ "))"));
  print_string "))) & \n";
  
  (* es wird immer um eine Spalte mod n weitergezählt, wenn Reihe komplett ist *)
  print_string ("A G((" ^ exR ^ " ==> ((" ^ exC ^ " ==> A X(!z | ");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop C i));
  print_string (")) & (!(" ^ exC ^ ") ==> ((" ^ prop C 0 ^ " ==> A X(!z | !" ^ prop C 0 ^ ")) & (!" ^ prop C 0 ^
                " ==> A X(!z | " ^ prop C 0 ^ "))");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop C i in
              let pi' = prop C (i-1) in
              print_string ("(((" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (!" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | " ^ pi ^ ")) & ");
              print_string ("(((!" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | !" ^ pi ^ "))"));
  print_string (")))) & (!(" ^ exR ^ ") ==> (");
  printloop 0 bn " & " (fun i -> print_string ("((" ^ prop C i ^ " ==> A X(!z | " ^ prop C i ^ ")) & " ^
                                               "(!" ^ prop C i ^ " ==> A X(!z | !" ^ prop C i ^ ")))"));
  print_string "))) &\n"; 
  

  (* es wird immer um eine X-Komponente weitergezählt, wenn Spalte komplett ist *)
  print_string ("A G((" ^ exC ^ " ==> ((" ^ exX ^ " ==> A X(!z | ");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop X i));
  print_string (")) & (!(" ^ exX ^ ") ==> ((" ^ prop X 0 ^ " ==> A X(!z | !" ^ prop X 0 ^ ")) & (!" ^ prop X 0 ^
                " ==> A X(!z | " ^ prop X 0 ^ "))");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop X i in
              let pi' = prop X (i-1) in
              print_string ("(((" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (!" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | " ^ pi ^ ")) & ");
              print_string ("(((!" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | !" ^ pi ^ "))"));
  print_string (")))) & (!(" ^ exC ^ ") ==> (");
  printloop 0 bn " & " (fun i -> print_string ("((" ^ prop X i ^ " ==> A X(!z | " ^ prop X i ^ ")) & " ^
                                               "(!" ^ prop X i ^ " ==> A X(!z | !" ^ prop X i ^ ")))"));
  print_string "))) &\n"; 
  

  (* es wird immer um eine Y-Komponente weitergezählt, wenn X-Komponente am Ende ist *)
  print_string ("A G((" ^ exX ^ " ==> ((" ^ exY ^ " ==> A X(!z | ");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop Y i));
  print_string (")) & (!(" ^ exY ^ ") ==> ((" ^ prop Y 0 ^ " ==> A X(!z | !" ^ prop Y 0 ^ ")) & (!" ^ prop Y 0 ^
                " ==> A X(!z | " ^ prop Y 0 ^ "))");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop Y i in
              let pi' = prop Y (i-1) in
              print_string ("(((" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (!" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | " ^ pi ^ ")) & ");
              print_string ("(((!" ^ pi ^ " & ((" ^ pi' ^ " & E X " ^ pi' ^ ") | (!" ^ pi ^ " & E X !" ^ pi' ^ 
                            "))) | (" ^ pi ^ " & " ^ pi' ^ " & E X !" ^ pi' ^ ")) ==> A X(!z | !" ^ pi ^ "))"));
  print_string (")))) & (!(" ^ exX ^ ") ==> (");
  printloop 0 bn " & " (fun i -> print_string ("((" ^ prop Y i ^ " ==> A X(!z | " ^ prop Y i ^ ")) & " ^
                                               "(!" ^ prop Y i ^ " ==> A X(!z | !" ^ prop Y i ^ ")))"));
  print_string "))) &\n"; 
    

  (* an jeder Stelle steht genau eine Zahl im Bereich [1,..,n^2] *)
  print_string ("A G(" ^ at_most V bnn (n*n) ^ " & (");
  printloop 0 bnn " | " (fun i -> print_string (prop V i));
  print_string ")) &\n";


  (* in keinem Block, keiner Zeile und keiner Spalte steht eine Zahl doppelt *)
  print_string "A G(!z | A((";
  printloop 0 bn " & "
    (fun i -> let xi = prop X i in 
              print_string ("(" ^ xi ^ " & F(!z & " ^ xi ^ ") | !" ^ xi ^ " & F(!z & !" ^ xi ^ "))"));
  print_string " & ";
  printloop 0 bn " & "
    (fun i -> let yi = prop Y i in
              print_string ("(" ^ yi ^ " & F(!z & " ^ yi ^ ") | !" ^ yi ^ " & F(!z & !" ^ yi ^ "))"));
  print_string " | ";
  printloop 0 bn " & "
    (fun i -> let xi = prop X i in 
              print_string ("(" ^ xi ^ " & F(!z & " ^ xi ^ ") | !" ^ xi ^ " & F(!z & !" ^ xi ^ "))"));
  print_string " & ";
  printloop 0 bn " & "
    (fun i -> let ri = prop R i in
              print_string ("(" ^ ri ^ " & F(!z & " ^ ri ^ ") | !" ^ ri ^ " & F(!z & !" ^ ri ^ "))"));
  print_string " | ";
  printloop 0 bn " & "
    (fun i -> let ci = prop C i in 
              print_string ("(" ^ ci ^ " & F(!z & " ^ ci ^ ") | !" ^ ci ^ " & F(!z & !" ^ ci ^ "))"));
  print_string " & ";
  printloop 0 bn " & "
    (fun i -> let yi = prop Y i in
              print_string ("(" ^ yi ^ " & F(!z & " ^ yi ^ ") | !" ^ yi ^ " & F(!z & !" ^ yi ^ "))"));
  print_string ") ==> (";
  printloop 0 bnn " | "
    (fun i -> let vi = prop V i in
              print_string (vi ^ " & F(!z & !" ^ vi ^ ") | !" ^ vi ^ " & F(!z & " ^ vi ^ ")"));
  print_string ")));\n";

