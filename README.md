MLSolver
========

A tool for solving the satisfiability and validity problems for modal fixpoint logics.

Version 1.3, Copyright (c) 2008-2017

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Installation

Install OCaml, OUnit, OPAM, Ocamlbuild.

Then:
```bash	
git clone https://github.com/tcsprojects/mlsolver.git
cd mlsolver
git submodule update --init
cd pgsolver
touch temp/Generatedsat.ml
git submodule update --init
cd ..
make
```
