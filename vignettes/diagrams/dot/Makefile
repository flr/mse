SOURCE := $(wildcard *.dot)
PDF := $(SOURCE:%.dot=%.pdf)
SVG := $(SOURCE:%.dot=%.svg)
PNG := $(SOURCE:%.dot=%.png)

.PHONY: all

all: $(PDF) $(SVG) $(PNG)

%.pdf: %.dot
	@echo "$< -> $@"
	@dot -Tpdf -o $@ $<

%.svg: %.dot
	@echo "$< -> $@"
	@dot -Tsvg -o $@ $<
	@mv $@ ../

%.png: %.dot
	@echo "$< -> $@"
	@dot -Tpng -o $@ $<

