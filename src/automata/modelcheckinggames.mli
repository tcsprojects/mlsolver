type modelchecking_answer = Modelcheckinggamesregistry.modelchecking_answer

type modelchecking_procedure = Modelcheckinggamesregistry.modelchecking_procedure

val register_modelchecking_procedure: Modelcheckinggamesregistry.modelchecking_procedure -> string -> string -> unit

val mem_modelchecking_procedure: string -> bool

val find_modelchecking_procedure: string -> Modelcheckinggamesregistry.modelchecking_procedure * string

val enum_modelchecking_procedures: (Modelcheckinggamesregistry.modelchecking_procedure -> string -> string -> unit) -> unit

val fold_modelchecking_procedures: (Modelcheckinggamesregistry.modelchecking_procedure -> string -> string -> 'a -> 'a) -> 'a -> 'a