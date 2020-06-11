#!/usr/bin/env bash
make -C ../../..
mkdir -p output
ocamlc -c -bin-annot t.mli
dune exec ../../odoc/bin/main.exe -- compile --package output t.cmti
dune exec ../../odoc/bin/main.exe -- latex --output-dir . t.odoc
dune exec ../../odoc/bin/main.exe -- html --output-dir . t.odoc

pdflatex glue.tex

