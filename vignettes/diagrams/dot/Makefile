SOURCE := $(wildcard *.dot)
PDF := $(SOURCE:%.dot=%.pdf)
SVG := $(SOURCE:%.dot=%.svg)

.PHONY: all

all: $(PDF) $(SVG)

%.pdf: %.dot
	@echo "$< -> $@"
	@dot -Tpdf -o $@ $<

%.svg: %.dot
	@echo "$< -> $@"
	@dot -Tsvg -o $@ $<
	@mv $@ ../
