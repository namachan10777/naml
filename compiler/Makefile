OCAMLOPT=ocamlfind ocamlopt -package ppx_deriving.show

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
test: lex_test parser_test typing_test
	./lex_test
	./parser_test
	./typing_test

test.cmx: test.ml
	$(OCAMLOPT) $< -c

ast.cmx: ast.ml parser.cmx
	$(OCAMLOPT) $< -c

lex.cmx: lex.ml test.cmx
	$(OCAMLOPT) $< -c

lex_test.cmx: lex_test.ml parser.cmx test.cmx
	$(OCAMLOPT) $< -c

lex_test: test.cmx lex.cmx lex_test.cmx 
	$(OCAMLOPT) $^ -o $@

parser_test.cmx: parser_test.ml parser.cmx lex.cmx
	$(OCAMLOPT) $< -c

parser.cmx: parser.ml lex.cmx
	$(OCAMLOPT) $< -c

parser_test: lex.cmx test.cmx parser.cmx parser_test.cmx 
	$(OCAMLOPT) $^ -o $@

typing.cmx: typing.ml alpha.cmx
	$(OCAMLOPT) $< -c

typing_test.cmx: typing_test.ml typing.cmx
	$(OCAMLOPT) $< -c

typing_test: typing_test.cmx alpha.cmx
	$(OCAMLOPT) $^ -o $@

alpha.cmx: alpha.ml ast.cmx
	$(OCAMLOPT) $< -c

compiler1st: main.ml Makefile
	ocamlopt $< -o $@
