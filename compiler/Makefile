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
	rm -f *.pp.ml

.PHONY: test
test: lex_test parser_test
	./lex_test
	./parser_test

test.cmx: test.ml
	ocamlopt  $< -c

ast.cmx: ast.ml
	ocamlopt $< -c

parser.cmx: parser.ml ast.ml
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

lex.cmx: lex.ml parser.ml test.ml
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

lex_test.cmx: lex_test.ml parser.cmx test.cmx
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

lex_test: test.cmx lex.cmx ast.cmx lex_test.cmx 
	ocamlfind ocamlopt -package ppx_deriving.show $^ -o $@

parser_test.cmx: parser_test.ml parser.ml ast.ml
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

parser.cmx: parser.ml ast.cmx
	ocamlfind ocamlopt -package ppx_deriving.show $< -c

parser_test: lex.cmx test.cmx ast.cmx parser.cmx parser_test.cmx 
	ocamlfind ocamlopt -package ppx_deriving.show $^ -o $@

compiler1st: main.ml Makefile
	ocamlopt $< -o $@
