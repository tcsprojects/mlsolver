open Tcsautomata

type ltmc_thread_nba_state = Tracking of (int * int) | Waiting | Failed

type rules = Branch of bool * int | Follow of int | Next | Delete of int

val ltmc_thread_nba :
  Ltmcformula.decomposed_ltmc_formula ->
  (ltmc_thread_nba_state, rules) NBA.t
  
val ltmc_thread_nba_state_size:
  Ltmcformula.decomposed_ltmc_formula ->
  NMAFunctions.state_size
