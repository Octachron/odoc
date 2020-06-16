#!/usr/bin/env bash
make -C ..
mkdir -p output
ocamlc -c -bin-annot so_far_away.mli
ocamlc -c -bin-annot t.mli
dune exec ../src/odoc/bin/main.exe -- compile --package output so_far_away.cmti
dune exec ../src/odoc/bin/main.exe -- compile --package output t.cmti
dune exec ../src/odoc/bin/main.exe -- latex --output-dir . so_far_away.odoc
dune exec ../src/odoc/bin/main.exe -- latex --output-dir . -I . t.odoc

pdflatex glue.tex

