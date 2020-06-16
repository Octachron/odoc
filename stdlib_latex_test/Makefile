

premodules=camlinternalFormatBasics stdlib
postmodules=atomic seq option result bool char uchar sys list bytes string unit marshal obj array float int int32 int64 nativeint lexing parsing set map stack queue camlinternalLazy lazy stream buffer camlinternalFormat printf arg printexc fun gc digest random hashtbl weak format scanf callback camlinternalOO oo camlinternalMod genlex ephemeron filename complex arrayLabels listLabels bytesLabels stringLabels moreLabels stdLabels spacetime bigarray

modules = $(premodules) $(postmodules)

odoc=dune exec ../src/odoc/bin/main.exe

latex_files = $(addprefix output/,$(addsuffix .tex, $(modules)))
html_files = $(addprefix output/,$(addsuffix .html, $(modules)))

odoc = ../_build/default/src/odoc/bin/main.exe

all: pre glue.pdf

.PHONY: html
html:  $(html_files)



.PHONY: pre
pre:
	dune build ../src/odoc/bin/main.exe


glue.pdf: $(latex_files) pre
	pdflatex glue.tex


stdlib.cmi: camlinternalFormatBasics.cmi stdlib.mli
	ocamlopt -no-alias-deps -c -bin-annot -nostdlib -nopervasives -w -49 stdlib.mli

camlinternalFormatBasics.cmi: camlinternalFormatBasics.mli
	ocamlopt -no-alias-deps -c -bin-annot -nostdlib -nopervasives $<


$(addsuffix .cmi, $(postmodules)):%.cmi:%.mli stdlib.cmi
	ocamlopt -no-alias-deps -c -bin-annot -nostdlib -nopervasives -open Stdlib $<

$(addsuffix .cmti, $(modules)):%.cmti:%.cmi

$(addsuffix .odoc, $(modules)):%.odoc: %.cmti
	$(odoc) compile --package output $<


$(latex_files):output/%.tex: %.odoc
	  $(odoc) latex --output-dir . $<

$(html_files):output/%.html: %.odoc
	  $(odoc) html --output-dir . $<

clean:
	rm *.cmti
	rm *.cmi
	rm *.odoc
	rm output/*.tex

include Makefile.depends
