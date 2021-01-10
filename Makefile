REPORT1_OUTPUTS= \
	report/report1.pdf \
	code/dune-project \
	code/lib/dune \
	code/lib/kadai1_3.ml

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
	code/lib/kadai4_ast.ml \
	code/lib/kadai4_lex.ml \
	code/lib/kadai4_top.ml \
	code/lib/kadai4_parser.mly

OUTPUTS= \
	kadai1.tar.gz \
	kadai2.tar.gz \
	kadai3.tar.gz \
	kadai4.tar.gz \
	kadai5.tar.gz

.PHONY: all
all: $(OUTPUTS)

kadai1.tar.gz: $(REPORT1_OUTPUTS)
	tar czf $@ $(REPORT1_OUTPUTS)

kadai2.tar.gz: $(REPORT2_OUTPUTS)
	tar czf $@ $(REPORT2_OUTPUTS)

kadai3.tar.gz: $(REPORT3_OUTPUTS)
	tar czf $@ $(REPORT3_OUTPUTS)

kadai4.tar.gz: $(REPORT4_OUTPUTS)
	tar czf $@ $(REPORT4_OUTPUTS)

kadai5.tar.gz: $(REPORT5_OUTPUTS)
	tar czf $@ $(REPORT5_OUTPUTS)

%.pdf: %.saty
	satysfi -b $< -o $@
