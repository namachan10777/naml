REPORT1_OUTPUTS= \
	report/report1.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/k1main.ml

REPORT2_OUTPUTS= \
	report/report2.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai2_2.ml \

REPORT3_OUTPUTS= \
	report/report3.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai2_4.ml \
	code/lib/kadai2_5.ml

REPORT4_OUTPUTS= \
	report/report4.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai3_2.ml

REPORT5_OUTPUTS= \
	report/report5.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/k4ast.ml \
	code/lib/k4lex.ml \
	code/lib/k4top.ml \
	code/lib/k4parser.mly

REPORT6_OUTPUTS = \
	code/lib/k5ast.ml \
	code/lib/k5lex.ml \
	code/lib/k5top.ml \
	code/lib/k5parser.mly \
	report/report6.pdf

REPORT7_OUTPUTS = \
	code/lib/k7ast.ml \
	code/lib/k7lex.ml \
	code/lib/k7top.ml \
	code/lib/k7parser.mly \
	report/report7.pdf

REPORT8_OUTPUTS = \
	code/lib/cam.ml \
	code/lib/zam.ml \
	code/lib/compile_to_cam.ml \
	code/test/compile_to_cam.ml \
	code/test/compile_to_zam.ml \
	code/lib/compile_to_zam.ml \
	report/report8.pdf

DIST=dist

OUTPUTS= \
	$(DIST)/kadai1.tar.gz \
	$(DIST)/kadai2.tar.gz \
	$(DIST)/kadai3.tar.gz \
	$(DIST)/kadai4.tar.gz \
	$(DIST)/kadai5.tar.gz \
	$(DIST)/kadai6.tar.gz \
	$(DIST)/kadai7.tar.gz \
	$(DIST)/kadai8.tar.gz \
	final.pdf

.PHONY: all
all: $(OUTPUTS) compiler


.PHONY: clean
clean:
	rm -f $(shell find . -type f -name '*.satysfi-aux')
	rm -f $(shell find . -type f -name '*.pdf')
	rm -f $(shell find . -type f -name '*.tar.gz')
	rm -rf $(shell find . -type d -name '_build')
	rm -rf *.fdb *.aux *.log *.blg *.fls *.bbl *.dvi *.fdb_latexmk sourcelist.tex
	rm -f compiler/*.cmo
	rm -f compiler/*.cmx
	rm -f compiler/*.cmi
	rm -f compiler/*.cmt
	rm -f compiler/*.o
	rm -f compiler/*_test
	rm -f compiler2 compiler1
	rm -f compiler/*.pp.ml
	rm -f *.out *.s


$(DIST)/kadai1.tar.gz: $(REPORT1_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT1_OUTPUTS)

$(DIST)/kadai2.tar.gz: $(REPORT2_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT2_OUTPUTS)

$(DIST)/kadai3.tar.gz: $(REPORT3_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT3_OUTPUTS)

$(DIST)/kadai4.tar.gz: $(REPORT4_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT4_OUTPUTS)

$(DIST)/kadai5.tar.gz: $(REPORT5_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT5_OUTPUTS)

$(DIST)/kadai6.tar.gz: $(REPORT6_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT6_OUTPUTS)

$(DIST)/kadai7.tar.gz: $(REPORT7_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT7_OUTPUTS)

$(DIST)/kadai8.tar.gz: $(REPORT8_OUTPUTS)
	mkdir -p $(DIST)
	tar czf $@ $(REPORT8_OUTPUTS)

BENCHS = dist/fib_result.txt dist/ack_result.txt dist/tarai_result.txt

dist/fib_result.txt: compiler1 compiler2 ./code/example/fib.ml compiler/example/fib.ml ./gen_bench.fish
	fish ./gen_bench.fish fib &> $@

dist/ack_result.txt: compiler1 compiler2 ./code/example/ack.ml compiler/example/ack.ml ./gen_bench.fish
	fish ./gen_bench.fish ack &> $@

dist/tarai_result.txt: compiler1 compiler2 ./code/example/tarai.ml compiler/example/tarai.ml ./gen_bench.fish
	fish ./gen_bench.fish tarai &> $@

TYPETEST_SOURCES = $(wildcard typetest/*)
TYPETEST_TARGETS = $(patsubst %.ml,%.txt,$(TYPETEST_SOURCES))

typetest/%.txt: typetest/%.ml compiler2 Makefile
	cat $<  > $@
	compiler2 typing $< &>> $@ || true

compiler1: $(find code -type f -name "*.ml")
	cd code && dune build && cp _build/default/bin/main.exe ../compiler1

sourcelist.tex: $(shell find . -type f -name "*.ml") gen_source_list.sh
	./gen_source_list.sh > $@

typetestlist.tex: $(TYPETEST_TARGETS) ./gen_typetest_list.sh
	./gen_typetest_list.sh > $@

bench_list.tex: $(BENCHS)
	./gen_bench_list.sh > $@

final.pdf: final.tex $(wildcard figures/*) sourcelist.tex bench_list.tex typetestlist.tex
	latexmk $<

%.pdf: %.saty
	satysfi -b $< -o $@

OCAMLOPT=ocamlfind ocamlopt -package ppx_deriving.show

.PHONY: test
test: compiler/lex_test compiler/parser_test compiler/typing_test compiler/closure_test
	./compiler/lex_test
	./compiler/parser_test
	./compiler/typing_test
	./compiler/closure_test
	cd code && dune runtest

compiler/test.cmx: compiler/test.ml
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/ast.cmx: compiler/ast.ml compiler/parser.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/lex.cmx: compiler/lex.ml compiler/test.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/lex_test.cmx: compiler/lex_test.ml compiler/parser.cmx compiler/test.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/lex_test: compiler/test.cmx compiler/lex.cmx compiler/lex_test.cmx 
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$^) -o $(patsubst compiler/%,%,$@)

compiler/parser_test.cmx: compiler/parser_test.ml compiler/parser.cmx compiler/lex.cmx compiler/util.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/id.cmx: compiler/id.ml
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/parser.cmx: compiler/parser.ml compiler/lex.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/parser_test: compiler/lex.cmx compiler/test.cmx compiler/parser.cmx compiler/util.cmx compiler/parser_test.cmx 
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$^) -o $(patsubst compiler/%,%,$@)

compiler/alpha.cmx: compiler/alpha.ml compiler/types.cmx compiler/ast.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/typing.cmx: compiler/typing.ml compiler/util.cmx compiler/types.cmx compiler/alpha.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/typing_test.cmx: compiler/typing_test.ml compiler/util.cmx compiler/typing.cmx compiler/util.cmx compiler/ast.cmx compiler/alpha.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/typing_test: compiler/lex.cmx compiler/parser.cmx compiler/ast.cmx compiler/util.cmx \
	compiler/id.cmx compiler/types.cmx compiler/alpha.cmx compiler/typing.cmx compiler/test.cmx  compiler/util.cmx compiler/alpha.cmx compiler/typing_test.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$^) -o $(patsubst compiler/%,%,$@)

compiler/util.cmx: compiler/util.ml
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/types.cmx: compiler/types.ml compiler/util.cmx compiler/id.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/closure.cmx: compiler/closure.ml compiler/typing.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/closure_test.cmx: compiler/closure_test.ml compiler/closure.cmx  compiler/ast.cmx \
	compiler/alpha.cmx compiler/typing.cmx compiler/lex.cmx compiler/parser.cmx compiler/types.cmx compiler/util.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/codegen.cmx: compiler/codegen.ml compiler/closure.cmx compiler/emit.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/closure_test: compiler/util.cmx compiler/lex.cmx compiler/parser.cmx compiler/ast.cmx \
	compiler/types.cmx compiler/alpha.cmx compiler/typing.cmx compiler/closure.cmx compiler/closure_test.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$^) -o $(patsubst compiler/%,%,$@)

compiler/emit.cmx: compiler/emit.ml
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler/main.cmx: compiler/main.ml compiler/parser.cmx compiler/lex.cmx compiler/types.cmx compiler/closure.cmx compiler/codegen.cmx compiler/emit.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$<) -c

compiler2: compiler/util.cmx compiler/lex.cmx compiler/parser.cmx compiler/ast.cmx \
	compiler/id.cmx compiler/types.cmx compiler/alpha.cmx compiler/typing.cmx compiler/closure.cmx compiler/emit.cmx compiler/codegen.cmx compiler/main.cmx
	cd compiler && $(OCAMLOPT) $(patsubst compiler/%,%,$^) -o ../$@
