open Tcsctlstarformula;;

type rules = Branch of block * int * bool
           | Follow of block * int
           | Next0
           | Next1 of block
           | Delete of block * int
           | DeleteBlock of block

type ctl_trace_nba_state = Failed
           | Waiting
           | Tracking of block * int

val ctl_trace_nba :
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  (ctl_trace_nba_state, rules) Tcsautomata.NBA.t