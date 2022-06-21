# The standard library

CAMLCOMP=../../_build/default/light/bin/light.exe
COMPFLAGS=-no-stdlib

# The list of all interfaces.
# Order irrelevant.

STD_INTF= sys.zi int.zi io.zi string.zi obj.zi fvect.zi vect.zi bool.zi eq.zi \
          exc.zi ref.zi

# The list of all implementations.
# In dependency order.

STD_IMPL= io.zo vect.zo exc.zo

all: rebuild $(STD_INTF) $(STD_IMPL)

# Define custom suffix rules.
# https://www.tutorialspoint.com/makefile/makefile_suffix_rules.htm
.SUFFIXES: .ml .mli .zi .zo

# Build .zi from .mli
.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

# Build .zo from .ml
.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.PHONY:
rebuild:
	dune build ../bin/light.exe

clean:
	rm -f *.zi *.zo
