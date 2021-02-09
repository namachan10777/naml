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
test: lex_test parser_test typing_test closure_test
	./lex_test
	./parser_test
	./typing_test
	./closure_test

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

parser_test.cmx: parser_test.ml parser.cmx lex.cmx util.cmx
	$(OCAMLOPT) $< -c

id.cmx: id.ml
	$(OCAMLOPT) $< -c

parser.cmx: parser.ml lex.cmx
	$(OCAMLOPT) $< -c

parser_test: lex.cmx test.cmx parser.cmx util.cmx parser_test.cmx 
	$(OCAMLOPT) $^ -o $@

alpha.cmx: alpha.ml types.cmx ast.cmx
	$(OCAMLOPT) $< -c

typing.cmx: typing.ml util.cmx types.cmx alpha.cmx
	$(OCAMLOPT) $< -c

typing_test.cmx: typing_test.ml util.cmx typing.cmx util.cmx ast.cmx alpha.cmx
	$(OCAMLOPT) $< -c

typing_test: lex.cmx parser.cmx ast.cmx util.cmx id.cmx types.cmx alpha.cmx typing.cmx test.cmx  util.cmx alpha.cmx typing_test.cmx
	$(OCAMLOPT) $^ -o $@

util.cmx: util.ml
	$(OCAMLOPT) $< -c

types.cmx: types.ml util.cmx id.cmx
	$(OCAMLOPT) $< -c

closure.cmx: closure.ml typing.cmx
	$(OCAMLOPT) $< -c

closure_test.cmx: closure_test.ml closure.cmx  ast.cmx alpha.cmx typing.cmx lex.cmx parser.cmx types.cmx util.cmx
	$(OCAMLOPT) $< -c

ir.cmx: ir.ml closure.cmx emit.cmx
	$(OCAMLOPT) $< -c

closure_test: util.cmx lex.cmx parser.cmx ast.cmx types.cmx alpha.cmx typing.cmx closure.cmx closure_test.cmx
	$(OCAMLOPT) $^ -o $@

emit.cmx: emit.ml
	$(OCAMLOPT) $< -c

main.cmx: main.ml parser.cmx lex.cmx types.cmx closure.cmx ir.cmx emit.cmx
	$(OCAMLOPT) $< -c

compiler1st: util.cmx lex.cmx parser.cmx ast.cmx id.cmx types.cmx alpha.cmx typing.cmx closure.cmx emit.cmx ir.cmx main.cmx
	ocamlopt $^ -o $@
