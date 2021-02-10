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
all: $(OUTPUTS)

.PHONY: clean
clean:
	rm -f $(shell find . -type f -name '*.satysfi-aux')
	rm -f $(shell find . -type f -name '*.pdf')
	rm -f $(shell find . -type f -name '*.tar.gz')
	rm -rf $(shell find . -type d -name '_build')

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

final.pdf: final.tex $(wildcard figures/*)
	lualatex $< --halt-on-error -interaction=batchmode -shell-escape

%.pdf: %.saty
	satysfi -b $< -o $@
