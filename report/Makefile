SRC := $(wildcard *.saty)
OUT := $(patsubst %.saty,%.pdf,$(SRC))

.PHONY: all
all: $(OUT)

%.pdf: %.saty Makefile
	satysfi -b $< -o $@
