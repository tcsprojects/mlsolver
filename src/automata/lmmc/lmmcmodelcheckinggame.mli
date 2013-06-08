type state =
    Atom of int
  | Tuple of (int * int)
  
type modelchecking_game = state Tcsgames.initpg

val get_modelchecking_game :
  Lmmcformula.decomposed_labelled_mmc_formula ->
  Tcstransitionsys.explicit_initlts ->
  modelchecking_game
