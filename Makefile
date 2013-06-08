ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif

ifeq ($(strip $(wildcard $(PGSOLVER))),)
else
	ADDMODULES=$(PGSOLVER)/libpgsolver.cmxa $(OBJDIR)/pgsolver.cmx
	ADDINTF=$(PGSOLVER)/libpgsolver.cmi $(OBJDIR)/pgsolver.cmi
	ADDINCL=-I $(PGSOLVER)
endif

TCSLIBOBJ=$(TCSLIBROOT)/obj

INCLUDES=-I $(SRCDIR) -I $(OBJDIR) -I $(OCAML_DIR) -I $(TCSLIBOBJ) $(ADDINCL)

MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
	$(OBJDIR)/metaformula.cmx \
	$(OBJDIR)/validitygames.cmx \
	$(OBJDIR)/modelcheckinggames.cmx \
	$(OBJDIR)/lmmcformula.cmx \
	$(OBJDIR)/mmcformula.cmx \
	$(OBJDIR)/ltmcformula.cmx \
	$(OBJDIR)/ctlstarformula.cmx \
	$(OBJDIR)/pdlformula.cmx \
	$(OBJDIR)/lmmcthreadnba.cmx \
	$(OBJDIR)/mmcthreadnba.cmx \
	$(OBJDIR)/ltmcthreadnba.cmx \
	$(OBJDIR)/pdlthreadnba.cmx \
	$(OBJDIR)/ctlstarthreadnba.cmx \
	$(OBJDIR)/ctlstartracenba.cmx \
	$(OBJDIR)/ctlstartracenba2.cmx \
	$(OBJDIR)/ctltracenba.cmx \
	$(OBJDIR)/ctlplustracenba.cmx \
	$(OBJDIR)/lmmcvaliditygame.cmx \
	$(OBJDIR)/mmcvaliditygame.cmx \
	$(OBJDIR)/ltmcvaliditygame.cmx \
	$(OBJDIR)/pdlvaliditygame.cmx \
	$(OBJDIR)/ctlstarvaliditygame.cmx \
	$(OBJDIR)/ctlvaliditygame.cmx \
	$(OBJDIR)/lmmcmodelcheckinggame.cmx \
	$(OBJDIR)/mmcmodelcheckinggame.cmx \
	$(OBJDIR)/ltmcmodelcheckinggame.cmx \
	$(OBJDIR)/pdlmodelcheckinggame.cmx \
	$(OBJDIR)/ctlstarmodelcheckinggame.cmx \
	$(OBJDIR)/ctlmodelcheckinggame.cmx \
	$(OBJDIR)/pgsolvers.cmx \
	$(OBJDIR)/externalsolver.cmx

PARSER_SRC=$(SRCDIR)/formula/parser/parser.ml
LEXER_SRC=$(SRCDIR)/formula/parser/lexer.ml
PARSERHELPER_SRC=$(SRCDIR)/formula/parser/parserhelper.ml

MORE_MODULES=$(OBJDIR)/parser.cmx \
	     $(OBJDIR)/lexer.cmx \
		 $(OBJDIR)/parserhelper.cmx

INTERFACES=$(MODULES:.cmx=.cmi)
PARSER_IFC=$(OBJDIR)/parser.cmi
PARSERHELPER_IFC=$(OBJDIR)/parserhelper.cmi

EXECUTABLE=$(BINDIR)/mlsolver

PACKAGE=mlsolver.tar

solver: $(INTERFACES) $(ADDINTF) parserlexer $(PARSER_IFC) $(MODULES) $(ADDMODULES) $(MORE_MODULES) main exec

all: solver generators

main: $(OBJDIR)/main.cmx

exec:
	$(OCAMLOPT) $(CPPCOMPILER) -o $(EXECUTABLE) $(MODULES) $(ADDMODULES) $(MORE_MODULES) $(OBJDIR)/main.cmx
	
$(PGSOLVER)/libpgsolver.cmi:
	make -C $(PGSOLVER)/.. all

$(PGSOLVER)/libpgsolver.cmxa:
	make -C $(PGSOLVER)/.. all

$(TCSLIBOBJ)/%.cmi:
	make -C $(TCSLIBROOT) all

$(TCSLIBOBJ)/tcslib.cmxa:
	make -C $(TCSLIBROOT) all

$(OBJDIR)/%.cmx: $(SRCDIR)/mlsolver/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/tools/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/generators/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/pgsolvers/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/lmmc/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/mmc/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<	

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/ltmc/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<	

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/ctlstar/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/pdl/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/formula/parser/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/lmmc/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/mmc/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/ltmc/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/pdl/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/ctlstar/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/ctl/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/generators/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/mlsolver/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/pgsolvers/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/lmmc/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/mmc/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/ltmc/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/ctlstar/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/pdl/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/lmmc/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/mmc/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/ltmc/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/pdl/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/ctlstar/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/ctl/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/formula/parser/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(SRCDIR)/formula/parser/parser.ml: $(SRCDIR)/formula/parser/parser.mly
	$(OCAMLYACC) $(SRCDIR)/formula/parser/parser.mly

$(OBJDIR)/parser.cmi: $(SRCDIR)/formula/parser/parser.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $(OBJDIR)/parser.cmi $(SRCDIR)/formula/parser/parser.mli

$(SRCDIR)/formula/parser/lexer.ml: $(SRCDIR)/formula/parser/lexer.mll
	$(OCAMLLEX) $(SRCDIR)/formula/parser/lexer.mll

parserlexer: $(INTERFACES) $(PARSER_SRC) $(PARSER_IFC) $(LEXER_SRC) $(PARSERHELPER_IFC) $(PARSERHELPER_SRC) 

generators: pdlsudokugenerator ctlstarsudokugenerator mucalcsudokugenerator ltmcparitybuechigenerator elevatortsgenerator philosopherstsgenerator

pdlsudokugenerator: $(OBJDIR)/pdlsudoku.cmx
	$(OCAMLOPT) $(INCLUDES) -o bin/pdlsudokugenerator $(OBJDIR)/pdlsudoku.cmx

ctlstarsudokugenerator: $(OBJDIR)/ctlstarsudoku.cmx
	$(OCAMLOPT) $(INCLUDES) -o bin/ctlstarsudokugenerator $(OBJDIR)/ctlstarsudoku.cmx

mucalcsudokugenerator: $(OBJDIR)/mucalcsudoku.cmx
	$(OCAMLOPT) $(INCLUDES) -o bin/mucalcsudokugenerator $(OBJDIR)/mucalcsudoku.cmx

ltmcparitybuechigenerator: $(OBJDIR)/ltmcparitybuechi.cmx
	$(OCAMLOPT) $(INCLUDES) -o bin/ltmcparitybuechigenerator $(OBJDIR)/ltmcparitybuechi.cmx

elevatortsgenerator: $(OBJDIR)/elevatorts.cmx
	$(OCAMLOPT) $(INCLUDES) -o bin/elevatortsgenerator $(OBJDIR)/elevatorts.cmx

philosopherstsgenerator: $(OBJDIR)/philosophersts.cmx
	$(OCAMLOPT) $(INCLUDES) -o bin/philosopherstsgenerator $(OBJDIR)/philosophersts.cmx

tools: $(OBJDIR)/guarded_trafo_worst_case.cmx
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/guarded_trafo_worst_case $(MODULES) $(ADDMODULES) $(MORE_MODULES) $(OBJDIR)/guarded_trafo_worst_case.cmx


package:
	$(TAR) cvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=scripts/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,^,mlsolver/," Makefile Config.default install.txt changelog.txt src bin obj test
	$(TAR) rvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=scripts/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,TCSlib,mlsolver/TCSlib," $(TCSLIBROOT)/obj $(TCSLIBROOT)/src $(TCSLIBROOT)/Makefile $(TCSLIBROOT)/Config.default
	$(GZIP) $(PACKAGE)

packagewithpgsolver: package
	make -C $(PGSOLVER)/.. package
	$(GZIP) -d mlsolver.tar.gz
	mv $(PGSOLVER)/../pgsolver.tar.gz .
	$(GZIP) -d pgsolver.tar.gz
	$(TAR) -A --file=mlsolver.tar pgsolver.tar
	rm pgsolver.tar
	$(GZIP) $(PACKAGE)

clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.cmx $(OBJDIR)/*.cmi
	rm -f $(SRCDIR)/formula/parser/parser.ml $(SRCDIR)/formula/parser/parser.mli $(SRCDIR)/formula/parser/lexer.ml
	rm -f $(EXECUTABLE)
