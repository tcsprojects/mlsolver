#!/usr/bin/env bash
# opam uninstall mlsolver
# opam pin remove mlsolver
oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
# oasis2opam --local -y
# opam pin add mlsolver . -n -y
# opam install mlsolver
