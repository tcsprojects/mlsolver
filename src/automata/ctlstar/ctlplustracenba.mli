open Tcsautomata;;
open Tcsctlstarformula;;

type rules = Ctlstartracenba.rules

type ctlplus_trace_nba_state = Failed
                    | Waiting
                    | TrackingE of block
                    | TrackingA of block * int


val ctlplus_trace_nba :
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  (ctlplus_trace_nba_state, rules) NBA.t

val ctlplus_trace_nba_state_size:
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  NMAFunctions.state_size