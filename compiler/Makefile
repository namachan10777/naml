.PHONY: all
all: compiler1st

.PHONY: clean
clean:
	rm -f *.cmo
	rm -f *.cmx
	rm -f *.cmi
	rm -f *.cmt
	rm -f *.o
	rm -f compiler1st

compiler1st: main.ml Makefile
	ocamlopt $< -o $@
