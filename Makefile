OCAMLOPT=ocamlfind ocamlopt -package ppx_deriving.show

.PHONY: all
all: test 1st

.PHONY: clean
clean:
	rm -f *.cmo
	rm -f *.cmx
	rm -f *.cmi
	rm -f *.cmt
	rm -f *.o
	rm -f *_test
	rm -f compiler
	rm -f *.pp.ml

.PHONY: test
test: lex_test
	./lex_test

lex.cmx: lex.ml
	$(OCAMLOPT) $< -c

lex_test.cmx: lex_test.ml lex.cmx
	$(OCAMLOPT) $< -c

lex_test: lex.cmx lex_test.cmx
	$(OCAMLOPT) $^ -o $@
