open Tcsgames

type solver_func = (string array -> (explicit_pg -> explicit_pg_solution * explicit_pg_strategy))

val register_solver: solver_func -> string -> string -> string -> unit

val mem_solver: string -> bool

val find_solver: string -> solver_func * string * string

val enum_solvers: (solver_func -> string -> string -> string -> unit) -> unit

val fold_solvers: (solver_func -> string -> string -> string -> 'a -> 'a) -> 'a -> 'a


type partial_solver_func = (string array -> (int initpg -> int initpg_solution * int initpg_strategy))

val register_partial_solver: partial_solver_func -> string -> string -> string -> unit

val mem_partial_solver: string -> bool

val find_partial_solver: string -> partial_solver_func * string * string

val enum_partial_solvers: (partial_solver_func -> string -> string -> string -> unit) -> unit

val fold_partial_solvers: (partial_solver_func -> string -> string -> string -> 'a -> 'a) -> 'a -> 'a
