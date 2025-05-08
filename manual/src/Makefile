ROOTDIR = ../..
CAMLLATEX = $(ROOTDIR)/_build/default/tools/ocamltex.exe
# TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2

%.gen.tex: %.etex
	$(CAMLLATEX) $< -o $@

# %.tex: %.gen.tex
# 	$(TEXQUOTE) < $< > $*.texquote_error.tex
# 	mv $*.texquote_error.tex $@

%.tex: %.gen.tex
	mv $< $@

FILES = core.tex

etex-files: $(FILES)
all: $(FILES)

.PHONY: clean
clean:
	rm -f *.tex
