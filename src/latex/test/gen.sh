#!/usr/bin/env bash
make -C ../../..
ocamlc -c -bin-annot t.mli
dune exec ../../odoc/bin/main.exe -- compile --package test t.cmti
dune exec ../../odoc/bin/main.exe -- latex --output-dir . t.odoc
dune exec ../../odoc/bin/main.exe -- html --output-dir . t.odoc

pdflatex glue.tex

