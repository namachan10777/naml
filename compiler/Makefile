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

test.cmx: test.ml Makefile
	ocamlopt  $< -c

parser.cmx: parser.ml Makefile
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

lex.cmx: lex.ml
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

lex_test: lex.cmx test.cmx parser.cmx
	ocamlfind ocamlopt -package ppx_deriving.show test.cmx parser.cmx $< -o $@

compiler1st: main.ml Makefile
	ocamlopt $< -o $@
