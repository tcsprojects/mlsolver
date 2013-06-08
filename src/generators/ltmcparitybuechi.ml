let _ =
  let n = try
            int_of_string Sys.argv.(1) 
          with _ -> failwith "Cannot read parameter n."
  in

  let rec not_other_ones i j =
    if i = 0 then "tt"
    else if i = j then not_other_ones (i-1) j 
         else "!q" ^ string_of_int i ^ " & " ^ not_other_ones (i-1) j
  in

  let rec just_one_q = 
    function 0 -> "ff"
           | i -> "(q" ^ string_of_int i ^ " & " ^ not_other_ones n i ^ ") | " ^ just_one_q (i-1)
  in

  let always_just_one_q i = "nu X.(" ^ just_one_q i ^ ") & ()X" in 

  let infinitely_often_q i = "nu X.(mu Y.q" ^ string_of_int i ^ " | ()Y) & ()X" in 
  let finitely_often_q i = "!(" ^ infinitely_often_q i ^ ")" in

  let rec forall_greater_odd_finitely_often n i =
    if i > n then "tt"
    else finitely_often_q i ^ " & " ^ forall_greater_odd_finitely_often n (i+2)
  in

  let rec exists_even_inf_priority n i =
    if i = 0 then "ff"
    else if i mod 2 = 0 then "((" ^ infinitely_often_q i ^ ") & " ^ forall_greater_odd_finitely_often n (i+1) ^ ") | " ^
                             exists_even_inf_priority n (i-2)
         else exists_even_inf_priority n (i-1)
  in

  let rec parity_condition_bottom =
    function 0 -> "ff"
           | i -> "(q" ^ string_of_int i ^ " & ()X" ^ string_of_int i ^ ") | " ^ parity_condition_bottom (i-1)
  in

  let rec parity_condition n i =
    if i = 0 then parity_condition_bottom n
    else if i mod 2 = 0 then "nu X" ^ string_of_int i ^ "." ^ parity_condition n (i-1)
	 else "mu X" ^ string_of_int i ^ "." ^ parity_condition n (i-1)
  in

  print_string ("#phi := (" ^ always_just_one_q n ^ ") ==> \n         ((" ^ parity_condition n n ^ 
                ")\n           <==>\n         (" ^ exists_even_inf_priority n n ^ "));\n")

