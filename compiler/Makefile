.PHONY: all
all: test compiler1st

.PHONY: clean
clean:
	rm -f *.cmo
	rm -f *.cmx
	rm -f *.cmi
	rm -f *.cmt
	rm -f *.o
	rm -f *_test
	rm -f compiler1st

.PHONY: test
test: lex_test
	./lex_test

test.cmx: test.ml
	ocamlopt $< -c

parser.cmx: parser.ml
	ocamlopt $< -c

lex_test: lex.ml test.cmx parser.cmx
	ocamlopt -o $@ test.cmx lex.ml

compiler1st: main.ml Makefile
	ocamlopt $< -o $@
