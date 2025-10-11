open Tcsautomata;;
open Tcsctlstarformula;;
open Ctlstarthreadnba;;

type rules = Branch of block * int * bool
           | Follow of block * int
           | Next0
           | Next1 of block
           | Delete of block * int
           | DeleteBlock of block
           
val ctlstar_trace_nba_format_rule: Tcsctlstarformula.decomposed_ctlstar_formula -> rules -> string
           

type ('a, 'b) ctlstar_trace_nba_state = Failed
					| Waiting
                    | TrackingE of block * 'a
                    | TrackingA of block * 'b

val ctlstar_trace_nba :
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  ('a, Ctlstarthreadnba.rules) DBA.t ->
  ('b, Ctlstarthreadnba.rules) NBA.t ->
  (('a, 'b) ctlstar_trace_nba_state, rules) NBA.t
  
val ctlstar_trace_nba_state_size:
  Tcsctlstarformula.decomposed_ctlstar_formula ->
  NMAFunctions.state_size -> NMAFunctions.state_size -> NMAFunctions.state_size