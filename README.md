MLSolver
========

A tool for solving the satisfiability and validity problems for modal fixpoint logics.

Version 1.5, Copyright (c) 2008-2025, BSD 3 LICENSE

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## OPAM

You can install this package via OPAM under the name `mlsolver`.


## Commands


### Build

```
dune build
```

### Release

1. Change version in `dune-project`.
2. Update `CHANGES.md`.
3. Run `dune build`.
4. Commit
```
  git status
  git add -A
  git commit -m "message"
  git tag v1.x [--force]
  git push origin master --tags [--force]
```
5. Release
```
  dune-release tag
  dune-release distrib
  dune-release publish
  dune-release opam pkg
  dune-release opam submit
```  


## Test

You can verify that everything is working by calling:

```bash
bin/ltmcparitybuechi 3 | bin/mlsolver -stenv -val ltmc "#phi" -pgs recursive
```
