#!/usr/bin/env bash

modules="camlinternalFormatBasics atomic seq option result bool char uchar sys list bytes string unit marshal obj array float int int32 int64 nativeint lexing parsing set map stack queue camlinternalLazy lazy stream buffer camlinternalFormat printf arg printexc fun gc digest random hashtbl weak format scanf callback camlinternalOO oo camlinternalMod genlex ephemeron filename complex arrayLabels listLabels bytesLabels stringLabels moreLabels stdLabels spacetime bigarray"
odoc="dune exec ../../../odoc/bin/main.exe"

for i in $modules
do
    ocamlopt -c -bin-annot -nostdlib -nopervasives $i.mli
done
for i in $modules
do
    echo  $odoc -- compile --package output $i.cmti
    $odoc -- compile --package output $i.cmti
done

for i in $modules
do
    echo $odoc -- latex --output-dir . $i.odoc
    $odoc -- latex --output-dir . $i.odoc
done

rm index.tex
for i in $modules
do
    echo \\input{output/${i^}.tex} >> index.tex
done

pdflatex glue.tex
