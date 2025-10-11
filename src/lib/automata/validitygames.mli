type validity_answer = Validitygamesregistry.validity_answer

type validity_procedure = Validitygamesregistry.validity_procedure

val register_validity_procedure: Validitygamesregistry.validity_procedure -> string -> string -> unit

val mem_validity_procedure: string -> bool

val find_validity_procedure: string -> Validitygamesregistry.validity_procedure * string

val enum_validity_procedures: (Validitygamesregistry.validity_procedure -> string -> string -> unit) -> unit

val fold_validity_procedures: (Validitygamesregistry.validity_procedure -> string -> string -> 'a -> 'a) -> 'a -> 'a