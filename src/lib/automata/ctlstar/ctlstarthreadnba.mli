type ctlstar_thread_nba_state = Tracking of int | Waiting | Failed;;

type rules = FormulaNext | FormulaBranch of int * bool | FormulaFollow of int | FormulaDelete of int

val ctlstar_thread_nba_format_rule: Tcsctlstarformula.decomposed_ctlstar_formula -> rules -> string

val ctlstar_thread_nba_a_block :
  Tcsctlstarformula.decomposed_ctlstar_formula -> (ctlstar_thread_nba_state, rules) Tcsautomata.NBA.t

val ctlstar_thread_nba_e_block :
  Tcsctlstarformula.decomposed_ctlstar_formula -> (ctlstar_thread_nba_state, rules) Tcsautomata.NBA.t
 
val ctlstar_thread_nba_size :
  Tcsctlstarformula.decomposed_ctlstar_formula -> Tcsautomata.NMAFunctions.state_size
