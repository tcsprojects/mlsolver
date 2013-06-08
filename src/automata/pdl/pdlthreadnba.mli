open Tcsautomata

type pdl_thread_nba_state = Tracking of int | Waiting | Stopped;;

type rules = Branch of bool * int | Follow of int | Next of int | Delete of int;;

val pdl_thread_nba :
  Pdlformula.decomposed_pdl_formula ->
  (pdl_thread_nba_state, rules) Tcsautomata.NBA.t