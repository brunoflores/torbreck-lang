ROOTDIR = ../..
OCAMLRUN = ocamlrun

CAMLLATEX = $(OCAMLRUN) $(addprefix -I ,$(LD_PATH)) \
	$(ROOTDIR)/tools/ocamltex -repo-root $(ROOTDIR) -n 80 -v false

TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2

%.gen.tex: %.etex
	$(CAMLLATEX) $< -o $@

%.tex: %.gen.tex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@

FILES = core.tex

etex-files: $(FILES)
all: $(FILES)

.PHONY: clean
clean:
	rm -f *.tex
