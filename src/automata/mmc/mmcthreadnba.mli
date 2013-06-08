open Tcsautomata

type mmc_thread_nba_state = Tracking of (int * int) | Waiting | Failed

type rules = Branch of bool * int | Follow of int | Delete of int

val mmc_thread_nba :
  Mmcformula.decomposed_mmc_formula ->
  (mmc_thread_nba_state, rules) NBA.t
  
val mmc_thread_nba_state_size:
  Mmcformula.decomposed_mmc_formula ->
  NMAFunctions.state_size