type state =
    Atom of int
  | Tuple of (int * int)
  
type modelchecking_game = state Tcsgames.initpg

val get_modelchecking_game :
  Tcsltmcformula.decomposed_ltmc_formula ->
  Tcstransitionsys.explicit_initts ->
  modelchecking_game
