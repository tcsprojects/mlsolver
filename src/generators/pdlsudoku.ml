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
  print_string ("Encoding generalised Sudoku in a PDL formula.\n\nUsage:\n\n  pdlsudokugenerator <mode> <dimension>" ^
               "\n\nwhere\n  <mode> is either \"unary\" or \"binary\" determining how positions in the matrix are " ^
               "encoded, and\n  <dimension> is a positive natural number n determining the size n^2 x n^2 of the " ^
               "matrix.\n\n");
  exit 1

let _ =
  let binarymode = try
                     (String.uppercase_ascii Sys.argv.(1)) = "BINARY"
                   with _ -> help_message ()
  in
  let unarymode = try
                     (String.uppercase_ascii Sys.argv.(1)) = "UNARY"
                  with _ -> help_message ()
  in
  let n = try
            int_of_string Sys.argv.(2) 
          with _ -> help_message ()
  in

  if unarymode then  
  begin
  (* Nummerierung der Zust�nde mit Propositionen f�r Reihen (r), Spalten (c), Bl�cken (x,y) *)

  (* erster Zustand repr�sentiert Reihe 1, Spalte 1, Block (1,1) *)
  print_string ("#phi := " ^ prop R 1 ^ " & " ^ prop C 1 ^ " & " ^ prop X 1 ^ " & " ^ prop Y 1 ^ " &\n");
  
  (* jeder Zustand hat nur eine Reihe etc. *)
  print_string "[a^*](";
  printloop 1 n " | " 
            (fun i -> print_string "(";
                      printloop 1 n " & "
                                (fun j -> print_string ((if not (i=j) then "!" else "") ^ prop R j));
                      print_string ")");
  print_string ") &\n";
  print_string "[a^*](";
  printloop 1 n " | " 
            (fun i -> print_string "(";
                      printloop 1 n " & "
                                (fun j -> print_string ((if not (i=j) then "!" else "") ^ prop C j));
                      print_string ")");
  print_string ") &\n";
  print_string "[a^*](";
  printloop 1 n " | " 
            (fun i -> print_string "(";
                      printloop 1 n " & "
                                (fun j -> print_string ((if not (i=j) then "!" else "") ^ prop X j));
                      print_string ")");
  print_string ") &\n";
  print_string "[a^*](";
  printloop 1 n " | " 
            (fun i -> print_string "(";
                      printloop 1 n " & "
                                (fun j -> print_string ((if not (i=j) then "!" else "") ^ prop Y j));
                      print_string ")");
  print_string ") &\n";


  (* der letzte Zustand hat keine Nachfolger *)
  print_string ("[a^*.(" ^ prop X n ^ ")?.(" ^ prop Y n ^ ")?.(" ^ prop R n ^ ")?.(" ^ prop C n ^ ")?]ff &\n");

  (* jeder andere Zustand hat mindestens einen Nachfolger *)
  print_string ("[a^*]((!" ^ prop X n ^ " | !" ^ prop Y n ^ " | !" ^ prop R n ^ " | !" ^ prop C n ^ ") ==> <a>tt) & \n");

  (* es wird immer um eine Reihe mod n weitergez�hlt *)
  print_string "[a^*](";
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop R i ^ " ==> [a]" ^ prop R ((i mod n)+1) ^ ")"));
  print_string ") &\n";

  (* es wird immer um eine Spalte mod n weitergez�hlt, wenn Reihe komplett ist *)
  print_string ("[a^*]((" ^ prop R n ^ " ==> (");
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop C i ^ " ==> [a]" ^ prop C ((i mod n)+1) ^ ")"));
  print_string (")) & (!" ^ prop R n ^ " ==> (");
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop C i ^ " ==> [a]" ^ prop C i ^ ")"));
  print_string "))) &\n";

  (* es wird immer um eine X-Komponente weitergez�hlt, wenn Spalte komplett ist *)
  print_string ("[a^*]((" ^ prop C n ^ " ==> (");
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop X i ^ " ==> [a]" ^ prop X ((i mod n)+1) ^ ")"));
  print_string (")) & (!" ^ prop C n ^ " ==> (");
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop X i ^ " ==> [a]" ^ prop X i ^ ")"));
  print_string "))) &\n";

  (* es wird immer um eine Y-Komponente weitergez�hlt, wenn X-Komponente am Ende ist *)
  print_string ("[a^*]((" ^ prop X n ^ " ==> (");
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop Y i ^ " ==> [a]" ^ prop Y ((i mod n)+1) ^ ")"));
  print_string (")) & (!" ^ prop X n ^ " ==> (");
  printloop 1 n " & "
             (fun i -> print_string ("(" ^ prop Y i ^ " ==> [a]" ^ prop Y i ^ ")"));
  print_string "))) &\n";


  (* an jeder Stelle steht genau eine Zahl im Bereich [1,..,n^2] *)
  print_string "[a^*](";
  printloop 1 (n*n) " | "
             (fun i -> print_string "(";
                       printloop 1 (n*n) " & "
                                  (fun j -> print_string ((if not (i=j) then "!" else "") ^ prop V j));
                       print_string ")");
  print_string ") &\n";

  (* in keinem Block steht eine Zahl doppelt *)
  print_string "[a^*](";
  printloop 1 n " & "
    (fun i -> printloop 1 n " & "
                (fun j -> printloop 1 (n*n) " & "
                            (fun h -> print_string ("((" ^ prop X i ^ " & " ^ prop Y j ^ " & " ^ prop V h ^ 
                                                    ") ==> [a.a^*." ^ prop X i ^ "?." ^ prop Y j ^"?]!" ^ 
                                                    prop V h ^ ")"))));
  print_string ") &\n";


  (* in keine Reihe steht eine Zahl doppelt *)
  print_string "[a^*](";
  printloop 1 n " & "
    (fun i -> printloop 1 n " & "
                (fun j -> printloop 1 (n*n) " & "
                            (fun h -> print_string ("((" ^ prop X i ^ " & " ^ prop R j ^ " & " ^ prop V h ^ 
                                                    ") ==> [a.a^*." ^ prop X i ^ "?." ^ prop R j ^"?]!" ^ 
                                                    prop V h ^ ")"))));
  print_string ") &\n";

  (* in keine Spalte steht eine Zahl doppelt *)
  print_string "[a^*](";
  printloop 1 n " & "
    (fun i -> printloop 1 n " & "
                (fun j -> printloop 1 (n*n) " & "
                            (fun h -> print_string ("((" ^ prop Y i ^ " & " ^ prop C j ^ " & " ^ prop V h ^ 
                                                    ") ==> [a.a^*." ^ prop Y i ^ "?." ^ prop C j ^"?]!" ^ 
                                                    prop V h ^ ")"))));
  print_string ");"
  end
  
  else if binarymode then

  begin
  let bn = (bits (n-1))-1 in
  let bnn = (bits (n*n))-1 in

  print_string "#phi := ";
  (* erster Zustand repr�sentiert Reihe 1, Spalte 1, Block (1,1) *)
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
  print_string ("[a^*]((" ^ at_most R bn (n-1) ^ ") & (" ^ at_most C bn (n-1) ^ ") & (" ^ 
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
  let exX = exactly X bn (n-1) in
  let exY = exactly Y bn (n-1) in
  let exR = exactly R bn (n-1) in
  let exC = exactly C bn (n-1) in

  (* der letzte Zustand hat keine Nachfolger *)
  (* print_string ("[a^*.(" ^ exX ^ ")?.(" ^ exY ^ ")?.(" ^ exR ^ ")?.(" ^ exC ^ ")?]ff &\n"); *)

  (* jeder andere Zustand hat mindestens einen Nachfolger *)
  print_string ("[a^*]((!(" ^ exX ^ ") | !(" ^ exY ^ ") | !(" ^ exR ^ ") | !(" ^ exC ^ ")) <==> <a>tt) & \n");

  (* es wird immer um eine Reihe mod n weitergez�hlt *)
  print_string ("[a^*]((" ^ exR ^ " ==> [a](");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop R i));
  print_string (")) & (!(" ^ exR ^ ") ==> ((" ^ prop R 0 ^ " ==> [a]!" ^ prop R 0 ^ ") & (!" ^ prop R 0 ^
                " ==> [a]" ^ prop R 0 ^ ")");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop R i in
              let pi' = prop R (i-1) in
              print_string ("(<a>" ^ pi ^ " <==> ((" ^ pi ^ " & ((" ^ pi' ^ " & <a>" ^ pi' ^ ") | (!" ^ pi' ^ 
                            " & <a>!" ^ pi' ^ "))) | (!" ^ pi ^ " & " ^ pi' ^ " & <a>!" ^ pi' ^ ")))"));   
  print_string "))) & \n";
  
  (* es wird immer um eine Spalte mod n weitergez�hlt, wenn Reihe komplett ist *)
  print_string ("[a^*]((" ^ exR ^ " ==> ((" ^ exC ^ " ==> [a](");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop C i));
  print_string (")) & (!(" ^ exC ^ ") ==> ((" ^ prop C 0 ^ " ==> [a]!" ^ prop C 0 ^ ") & (!" ^ prop C 0 ^
                " ==> [a]" ^ prop C 0 ^ ")");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop C i in
              let pi' = prop C (i-1) in
              print_string ("(<a>" ^ pi ^ " <==> ((" ^ pi ^ " & ((" ^ pi' ^ " & <a>" ^ pi' ^ ") | (!" ^ pi' ^ 
                            " & <a>!" ^ pi' ^ "))) | (!" ^ pi ^ " & " ^ pi' ^ " & <a>!" ^ pi' ^ ")))"));   
  print_string (")))) & (!" ^ exR ^ " ==> (");
  printloop 0 bn " & " (fun i -> print_string ("(" ^ prop C i ^ " <==> <a>" ^ prop C i ^ ")"));
  print_string "))) &\n"; 
  

  (* es wird immer um eine X-Komponente weitergez�hlt, wenn Spalte komplett ist *)
  print_string ("[a^*]((" ^ exC ^ " ==> ((" ^ exX ^ " ==> [a](");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop C i));
  print_string (")) & (!(" ^ exX ^ ") ==> ((" ^ prop X 0 ^ " ==> [a]!" ^ prop X 0 ^ ") & (!" ^ prop X 0 ^
                " ==> [a]" ^ prop X 0 ^ ")");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop X i in
              let pi' = prop X (i-1) in
              print_string ("(<a>" ^ pi ^ " <==> ((" ^ pi ^ " & ((" ^ pi' ^ " & <a>" ^ pi' ^ ") | (!" ^ pi' ^ 
                            " & <a>!" ^ pi' ^ "))) | (!" ^ pi ^ " & " ^ pi' ^ " & <a>!" ^ pi' ^ ")))"));   
  print_string (")))) & (!" ^ exC ^ " ==> (");
  printloop 0 bn " & " (fun i -> print_string ("(" ^ prop X i ^ " <==> <a>" ^ prop X i ^ ")"));
  print_string "))) &\n"; 
  

  (* es wird immer um eine Y-Komponente weitergez�hlt, wenn X-Komponente am Ende ist *)
  print_string ("[a^*]((" ^ exX ^ " ==> ((" ^ exY ^ " ==> [a](");
  printloop 0 bn " & " (fun i -> print_string ("!" ^ prop X i));
  print_string (")) & (!(" ^ exY ^ ") ==> ((" ^ prop Y 0 ^ " ==> [a]!" ^ prop Y 0 ^ ") & (!" ^ prop Y 0 ^
                " ==> [a]" ^ prop Y 0 ^ ")");
  if bn >= 1 then print_string " & ";
  printloop 1 bn " & " 
    (fun i -> let pi = prop Y i in
              let pi' = prop Y (i-1) in
              print_string ("(<a>" ^ pi ^ " <==> ((" ^ pi ^ " & ((" ^ pi' ^ " & <a>" ^ pi' ^ ") | (!" ^ pi' ^ 
                            " & <a>!" ^ pi' ^ "))) | (!" ^ pi ^ " & " ^ pi' ^ " & <a>!" ^ pi' ^ ")))"));   
  print_string (")))) & (!" ^ exX ^ " ==> (");
  printloop 0 bn " & " (fun i -> print_string ("(" ^ prop Y i ^ " <==> <a>" ^ prop Y i ^ ")"));
  print_string "))) &\n"; 
  

  (* an jeder Stelle steht genau eine Zahl im Bereich [1,..,n^2] *)
  print_string ("[a^*](" ^ at_most V bnn (n*n) ^ " & (");
  printloop 0 bnn " | " (fun i -> print_string (prop V i));
  print_string ")) &\n";

  (* in keinem Block steht eine Zahl doppelt *)
  print_string "[a^*](";
  printloop 0 (n-1) " & "
    (fun i -> printloop 0 (n-1) " & "
                (fun j -> printloop 1 (n*n) " & "
                            (fun h -> let exY = exactly Y bn i in 
                                      let exX = exactly X bn j in
                                      let exV = exactly V bnn h in
                                      print_string ("((" ^ exY ^ " & " ^ exX ^ " & " ^ exV ^ ") ==> [a.a^*.(" ^ 
                                                    exY ^ ")?.(" ^ exX ^")?]!(" ^ exV ^ "))"))));
  print_string ") &\n";

  (* in keine Reihe steht eine Zahl doppelt *)
  print_string "[a^*](";
  printloop 0 (n-1) " & "
    (fun i -> printloop 0 (n-1) " & "
                (fun j -> printloop 1 (n*n) " & "
                            (fun h -> let exX = exactly X bn i in 
                                      let exR = exactly R bn j in
                                      let exV = exactly V bnn h in
                                      print_string ("((" ^ exX ^ " & " ^ exR ^ " & " ^ exV ^ ") ==> [a.a^*.(" ^ 
                                                    exX ^ ")?.(" ^ exR ^")?]!(" ^ exV ^ "))"))));
  print_string ") &\n";

  (* in keine Spalte steht eine Zahl doppelt *)
  print_string "[a^*](";
  printloop 0 (n-1) " & "
    (fun i -> printloop 0 (n-1) " & "
                (fun j -> printloop 1 (n*n) " & "
                            (fun h -> let exY = exactly Y bn i in 
                                      let exC = exactly C bn j in
                                      let exV = exactly V bnn h in
                                      print_string ("((" ^ exY ^ " & " ^ exC ^ " & " ^ exV ^ ") ==> [a.a^*.(" ^ 
                                                    exY ^ ")?.(" ^ exC ^")?]!(" ^ exV ^ "))"))));
  print_string ");"

  end

  else

  help_message ()
