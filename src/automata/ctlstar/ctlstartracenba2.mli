open Tcsautomata;;
open Tcsctlstarformula;;

type rules = Ctlstartracenba.rules

type ctlstar_trace_nba2_state = Failed
		   | Waiting
           | TrackingE of block * block
		   | TrackingA of block * int

val ctlstar_trace_nba2 :
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  (ctlstar_trace_nba2_state, rules) NBA.t
  
val ctlstar_trace_nba2_state_size:
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  NMAFunctions.state_size