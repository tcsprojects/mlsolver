type solver_func = Pgsolversregistry.solver_func

let register_solver = Pgsolversregistry.register_solver

let mem_solver = Pgsolversregistry.mem_solver

let find_solver = Pgsolversregistry.find_solver

let enum_solvers = Pgsolversregistry.enum_solvers

let fold_solvers = Pgsolversregistry.fold_solvers


type partial_solver_func = Pgsolversregistry.partial_solver_func

let register_partial_solver = Pgsolversregistry.register_partial_solver

let mem_partial_solver = Pgsolversregistry.mem_partial_solver

let find_partial_solver = Pgsolversregistry.find_partial_solver

let enum_partial_solvers = Pgsolversregistry.enum_partial_solvers

let fold_partial_solvers = Pgsolversregistry.fold_partial_solvers

let _ =
    Pgsolver.register ();
    Externalsolver.register ();;