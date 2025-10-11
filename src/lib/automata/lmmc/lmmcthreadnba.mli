open Tcsautomata

type lmmc_thread_nba_state = Tracking of (int * int) | Waiting | Failed

type rules = Branch of bool * int | Follow of int | Delete of int

val lmmc_thread_nba :
  Tcslmmcformula.decomposed_labelled_mmc_formula ->
  (lmmc_thread_nba_state, rules) NBA.t
  
val lmmc_thread_nba_state_size:
  Tcslmmcformula.decomposed_labelled_mmc_formula ->
  NMAFunctions.state_size