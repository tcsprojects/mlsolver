let bits n = 
  let rec blog i = function 1 -> i
                          | n -> blog (i+1) (n/2)
  in
  blog 1 n

let rec range i j = if j < i then [] else i::(range (i+1) j)

let rec repeat s = function 0 -> []
                          | n -> s::(repeat s (n-1))

let rec power_of_two n =
  if n==0 then 1 
  else if n mod 2 == 0 then let p = power_of_two (n/2) in p*p
       else 2*(power_of_two (n-1))

let how_many p l = List.fold_left (fun n -> fun x -> if p x then n+1 else n) 0 l

let _ =
  let n = try
            int_of_string Sys.argv.(1) 
          with _ -> failwith "Cannot read dimension unit."
  in
  let m = n*n in
  let b = if m=1 then 1 else bits (m-1) in

  (* "Es gibt immer nur jeweils einen Nachfolger in h- und v-Richtung."
    [(h u v)*] forall(i=0..3).([h]b_i \/ [h]-b_i) /\ ([v]b_i \/ [v]-b_i)  *)

  print_string ("#phi:=");
  
  print_string ("(nu X1.[h]X1 & [v]X1 & " ^ 
                String.concat 
                  " & " 
                  (List.map 
                     (fun i -> "([h]b" ^ string_of_int i ^ " | [h]!b" ^ string_of_int i ^ ") & ([v]b" ^ 
                               string_of_int i ^ " | [v]!b" ^ string_of_int i ^ ")") 
                     (range 0 (b-1)))
               ^ ") & \n");


  (* "Das Modell ist ein Gitter."
    [(h u v)*] forall(i=0..3).( (<h><v>b_i -> [v][h]b_i) /\ (<h><v>-b_i -> [v][h]-b_i) ) *)

  print_string ("(nu X2.[h]X2 & [v]X2 & " ^
                String.concat 
                  " & " 
                  (List.map 
                     (fun i -> "([h][v]!b" ^ string_of_int i ^ " | [v][h]b" ^ string_of_int i ^ ") & " ^
                               "([v][h]!b" ^ string_of_int i ^ " | [h][v]b" ^ string_of_int i ^ ")") 
                     (range 0 (b-1)))
               ^ ") & \n");


 (* "Das Modell hat 9x9 Felder."
    [(h u v)9]ff /\ forall(i=0..8).[(h u v)^i](<h>tt /\ <v>tt) *)

  let rec more_successors x y = 
    if x=0 then
     begin
      if y=0 then (*"[h|v]ff"*) "([h]ff)&([v]ff)"
      else "([h]ff & <v>tt & [v]" ^ more_successors 0 (y-1) ^ ")"
     end
    else 
     begin
      if y=0 then "([v]ff & <h>tt & [h]" ^ more_successors (x-1) 0 ^ ")"
      else "(<h>tt & <v>tt & [h]" ^ more_successors (x-1) y ^ " & [v]" ^ more_successors x (y-1) ^ ")"
     end
  in

  print_string ((more_successors m m) ^ " & \n");


 (* "Überall steht höchstens eine 9." (Da nirgendwo eine 0 stehen darf und man
     nicht addieren, sondern nur vergleichen muss, benutze ich die Bitwerte 0000
     bis 1000, um die Zahlen 1,...,9 zu kodieren.)
     [(h u v)^*]( b_3 -> (-b_0 /\ -b_1 /\ -b_2) ) *)

  let rec is_bounded i = 
    let is_ith_bit_set = (m-1) land (power_of_two i) > 0 in
    if i==0 then 
     begin
      if not is_ith_bit_set then "!b0" else "tt"
     end
    else
     begin
      "(!b" ^ string_of_int i ^ (if not is_ith_bit_set then " & " else " | ") ^ is_bounded (i-1) ^ ")"
     end 

  in
  print_string ("(nu X3.[h]X3 & [v]X3 & " ^ is_bounded (b-1) ^ ") & \n");


 (*  "In jeder Zeile steht keine Zahl doppelt."
   [v*]-(<h*>( exists(j=1..8).forall(i=0..3).( (b_i /\ <h^j>b_i) \/ (-b_i /\ <h^j>-b_i) ) ) *)

  if m > 1 then
  print_string ("(nu X4.[v]X4 & nu X5.[h]X5 & !(" ^
                String.concat
                  " | "
                  (List.map 
                     (fun j -> "(" ^ String.concat 
                                 " & "
                                 (List.map 
                                   (fun i -> "((b" ^ string_of_int i ^ " & " ^ String.concat "" (repeat "<h>" j) 
                                             ^ "b" ^ string_of_int i ^ ") | (!b" ^ string_of_int i ^ " & "
                                             ^ String.concat "" (repeat "<h>" j) ^ "!b" ^ string_of_int i ^ "))" )
                                   (range 0 (b-1))) ^ ")")
                     (range 1 (m-1))) ^
                ")) & \n");


 (*  "In jeder Spalte steht keine Zahl doppelt."
   [h*]-(<v*>( exists(j=1..8).forall(i=0..3).( (b_i /\ <v^j>b_i) \/ (-b_i /\ <v^j>-b_i) ) ) *)

  if m > 1 then 
  print_string ("(nu X6.[h]X6 & nu X7.[v]X7 & !(" ^
                String.concat
                  " | "
                  (List.map 
                     (fun j -> "(" ^ String.concat 
                                 " & "
                                 (List.map 
                                   (fun i -> "((b" ^ string_of_int i ^ " & " ^ String.concat "" (repeat "<v>" j) 
                                             ^ "b" ^ string_of_int i ^ ") | (!b" ^ string_of_int i ^ " & "
                                             ^ String.concat "" (repeat "<v>" j) ^ "!b" ^ string_of_int i ^ "))" )
                                   (range 0 (b-1))) ^ ")")
                     (range 1 (m-1))) ^
                ")) & \n");


 (* "In keinem Block steht eine Zahl doppelt." *)

  if n > 1 then
   begin
    let all_valid_pairs = 
      let aux = ref [] in
      for x1 = 0 to (n-1) do
        for y1 = 0 to (n-1) do
          for x2 = x1 to (n-1) do
            for y2 = y1 to (n-1) do
              if not (x2 = x1 && y2 = y1) then
                aux := ((repeat "<h>" x1) @ (repeat "<v>" y1) , (repeat "<h>" x2) @ (repeat "<v>" y2)) :: !aux
            done 
          done
        done
      done;
      !aux
    in

    print_string ("(nu X8." ^ String.concat "" (repeat "[h]" n) ^ "X8 & " ^ 
                  String.concat "" (repeat "[v]" n) ^ "X8 & !(" ^
                  String.concat 
                    " | "
                    (List.map 
                      (fun (w,u) -> "(" ^ String.concat 
                                            " & "
                                            (List.map 
                                               (fun i -> "((" ^ String.concat "" w ^ "b" ^ string_of_int i ^
                                                         " & " ^ String.concat "" u ^ "b" ^ string_of_int i ^ 
                                                         ") | (" ^ String.concat "" w ^ "!b" ^ string_of_int i ^
                                                         " & " ^ String.concat "" u ^ "!b" ^ string_of_int i ^ "))")
                                               (range 0 (b-1))) ^ ")")
                      all_valid_pairs) ^ ")) & \n")
   end;

  print_string "tt;\n"
