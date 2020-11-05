#!/bin/sh
set -e
git clone https://gitlab.com/dailambda/scaml
cd scaml
git checkout master
opam switch create . ocaml-base-compiler.4.09.1 --no-install
opam install -y ./scaml.opam
