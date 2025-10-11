(lang dune 3.18)
(name mlsolver)
(version 1.5)

(generate_opam_files true)

(source (github tcsprojects/mlsolver))

(package
 (name mlsolver)
 (maintenance_intent "(latest)")
 (synopsis "A tool for solving the satisfiability and validity problems for modal fixpoint logics")
 (description "A tool for solving the satisfiability and validity problems for modal fixpoint logics.")
 (depends
   (ocaml (>= 4.8))
   dune
   num
   (tcs-lib (>= 0.6))
   extlib
   (ocaml-sat-solvers (>= 0.8))
   (pgsolver (>= 4.4))
 )
 (license "BSD-3-clause")
 (authors "Oliver Friedmann" "Martin Lange") 
 (maintainers "Oliver Friedmann" "Martin Lange")
 (homepage "https://github.com/tcsprojects/mlsolver")
 (bug_reports "https://github.com/tcsprojects/mlsolver/issues")
)
