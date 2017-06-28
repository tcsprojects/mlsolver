type modelchecking_answer = Modelcheckinggamesregistry.modelchecking_answer

type modelchecking_procedure = Modelcheckinggamesregistry.modelchecking_procedure

let register_modelchecking_procedure = Modelcheckinggamesregistry.register_modelchecking_procedure;;

let mem_modelchecking_procedure = Modelcheckinggamesregistry.mem_modelchecking_procedure;;

let find_modelchecking_procedure = Modelcheckinggamesregistry.find_modelchecking_procedure;;

let enum_modelchecking_procedures = Modelcheckinggamesregistry.enum_modelchecking_procedures;;

let fold_modelchecking_procedures = Modelcheckinggamesregistry.fold_modelchecking_procedures;;

let _ =
    Ctlmodelcheckinggame.register ();
    Ctlstarmodelcheckinggame.register ();
    Lmmcmodelcheckinggame.register ();
    Ltmcmodelcheckinggame.register ();
    Mmcmodelcheckinggame.register ();
    Pdlmodelcheckinggame.register ();;