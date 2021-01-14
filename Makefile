REPORT1_OUTPUTS= \
	report/report1.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai1/main.ml

REPORT2_OUTPUTS= \
	report/report2.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai2/kadai2_2.ml \

REPORT3_OUTPUTS= \
	report/report3.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai2/kadai2_4.ml \
	code/lib/kadai2/kadai2_5.ml

REPORT4_OUTPUTS= \
	report/report4.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai3/kadai3_2.ml

REPORT5_OUTPUTS= \
	report/report5.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai4/ast.ml \
	code/lib/kadai4/lex.ml \
	code/lib/kadai4/top.ml \
	code/lib/kadai4/parser.mly

REPORT6_OUTPUTS = \
	report/report6.pdf

DIST=dist

OUTPUTS= \
	$(DIST)/kadai1.tar.gz \
	$(DIST)/kadai2.tar.gz \
	$(DIST)/kadai3.tar.gz \
	$(DIST)/kadai4.tar.gz \
	$(DIST)/kadai5.tar.gz \
	$(DIST)/kadai6.tar.gz

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

%.pdf: %.saty
	satysfi -b $< -o $@
