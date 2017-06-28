type validity_answer = Validitygamesregistry.validity_answer

type validity_procedure = Validitygamesregistry.validity_procedure

let register_validity_procedure = Validitygamesregistry.register_validity_procedure;;

let mem_validity_procedure = Validitygamesregistry.mem_validity_procedure;;

let find_validity_procedure = Validitygamesregistry.find_validity_procedure;;

let enum_validity_procedures = Validitygamesregistry.enum_validity_procedures;;

let fold_validity_procedures = Validitygamesregistry.fold_validity_procedures;;

let _ =
    Ctlvaliditygame.register ();
    Ctlstarvaliditygame.register ();
    Lmmcvaliditygame.register ();
    Ltmcvaliditygame.register ();
    Mmcvaliditygame.register ();
    Pdlvaliditygame.register ();;