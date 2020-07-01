AWK=awk
export AWK
define add_stdlib_prefix
  $(or $(filter stdlib camlinternal%,$1), stdlib__$1)
endef

# Modules should be listed in dependency order.
STDLIB_MODS=\
  camlinternalFormatBasics camlinternalAtomic \
  stdlib \
  seq option result bool char uchar \
  sys list bytes string unit marshal obj array float int int32 int64 nativeint \
  lexing parsing set map stack queue camlinternalLazy lazy stream buffer \
  camlinternalFormat printf arg atomic \
  printexc fun gc digest random hashtbl weak \
  format scanf callback camlinternalOO oo camlinternalMod genlex ephemeron \
  filename complex arrayLabels listLabels bytesLabels stringLabels moreLabels \
  stdLabels spacetime bigarray

STDLIB_MODULES=\
  $(foreach module, $(STDLIB_MODS), $(call add_stdlib_prefix,$(module)))

OBJS=$(addsuffix .cmo,$(STDLIB_MODULES))
NOSTDLIB= camlinternalFormatBasics.cmo camlinternalAtomic.cmo stdlib.cmo
OTHERS=$(filter-out $(NOSTDLIB),$(OBJS))
PREFIXED_OBJS=$(filter stdlib__%.cmo,$(OBJS))
UNPREFIXED_OBJS=$(PREFIXED_OBJS:stdlib__%.cmo=%)

COMPFLAGS=-strict-sequence -absname -w +a-4-9-41-42-44-45-48 \
          -g -warn-error A -bin-annot -nostdlib \
          -safe-string -strict-formats

ocamlopt=ocamlopt $(COMPFLAGS) -I .


.SUFFIXES: .mli .cmi

all: pre glue.pdf


export AWK

%.cmi: %.mli
	$(ocamlopt) $(shell ./Compflags $@) -I . -c $<

stdlib__%.cmi: %.mli
	$(ocamlopt) $(shell ./Compflags $@) -o $@ -c $<



# Dependencies on Stdlib (not tracked by ocamlc -depend)

$(OTHERS) std_exit.cmo: stdlib.cmi
$(OTHERS:.cmo=.cmi) std_exit.cmi: stdlib.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: stdlib.cmi
$(OTHERS:.cmo=.cmx) std_exit.cmx: stdlib.cmx

clean::
	rm -f *.cm* *.o *.obj *.a *.lib *.odoc output/*.tex

include .depend

EMPTY :=
SPACE := $(EMPTY) $(EMPTY)

.PHONY: depend
depend:
	ocamldep $(filter-out stdlib.%,$(wildcard *.mli *.ml)) \
	  > .depend.tmp
	ocamldep -pp "awk -f ./remove_module_aliases.awk" \
	  stdlib.ml stdlib.mli >> .depend.tmp
	sed -Ee \
	  's#(^| )(${subst ${SPACE},|,${UNPREFIXED_OBJS}})[.]#\1stdlib__\2.#g' \
	  .depend.tmp > .depend
	rm -f .depend.tmp


odoc=dune exec ../src/odoc/bin/main.exe

latex_files = $(addprefix output/,$(addsuffix .tex, $(STDLIB_MODULES)))
html_files = $(addprefix output/,$(addsuffix .html, $(STDLIB_MODULES)))

odoc = ../_build/default/src/odoc/bin/main.exe


.PHONY: html
html:  $(html_files)



.PHONY: pre
pre:
	dune build ../src/odoc/bin/main.exe


glue.pdf: $(latex_files) pre
	pdflatex glue.tex



$(addsuffix .cmti, $(STDLIB_MODULES)):%.cmti:%.cmi

$(addsuffix .odoc, $(STDLIB_MODULES)):%.odoc: %.cmti
	$(odoc) compile --package output -I . $<


$(latex_files):output/%.tex: %.odoc
	  $(odoc) latex --output-dir . -I . $<

$(html_files):output/%.html: %.odoc
	  $(odoc) html --output-dir . -I . $<


include .depend
