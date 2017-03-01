LIBS = nums,str

all: solver generators tools

generators: pdlsudokugenerator ctlstarsudokugenerator mucalcsudokugenerator ltmcparitybuechigenerator elevatortsgenerator philosopherstsgenerator

tools: guarded_trafo_worst_case

solver:
	ocamlbuild -libs $(LIBS) mlsolver.native
	mv mlsolver.native bin/mlsolver

guarded_trafo_worst_case:
	ocamlbuild guarded_trafo_worst_case.native
	mv guarded_trafo_worst_case.native bin/guarded_trafo_worst_case

pdlsudokugenerator:
	ocamlbuild pdlsudoku.native
	mv pdlsudoku.native bin/pdlsudokugenerator

ctlstarsudokugenerator:
	ocamlbuild ctlstarsudoku.native
	mv ctlstarsudoku.native bin/ctlstarsudokugenerator

mucalcsudokugenerator:
	ocamlbuild mucalcsudoku.native
	mv mucalcsudoku.native bin/mucalcsudokugenerator

ltmcparitybuechigenerator:
	ocamlbuild ltmcparitybuechi.native
	mv ltmcparitybuechi.native bin/ltmcparitybuechigenerator

elevatortsgenerator:
	ocamlbuild elevatorts.native
	mv elevatorts.native bin/elevatortsgenerator

philosopherstsgenerator:
	ocamlbuild philosophersts.native
	mv philosophersts.native bin/philosopherstsgenerator

clean:
	ocamlbuild -clean
