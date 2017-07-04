MLSolver
========

A tool for solving the satisfiability and validity problems for modal fixpoint logics.

Version 1.4, Copyright (c) 2008-2017

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Installation

Install the OCaml Package Manager [OPAM](https://opam.ocaml.org).

Then:
```bash	
opam update
opam upgrade
opam switch 4.03.0
eval `opam config env`
opam install ocamlbuild ocamlfind ounit TCSLib extlib ocaml-sat-solvers minisat pgsolver
git clone https://github.com/tcsprojects/mlsolver.git
cd mlsolver
ocaml setup.ml -configure
ocaml setup.ml -build
```


## Test

You can verify that everything is working by calling:

```bash
bin/ltmcparitybuechigenerator 3 | bin/mlsolver -stenv -val ltmc "#phi" -pgs recursive
```
