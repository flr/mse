PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDATE := $(shell sed -n "s/Date: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

R_FILES := $(wildcard $(PKGSRC)/R/*.R)
VIGNETTE_FILES := $(wildcard $(PKGSRC)/vignette/*.Rmd)
HELP_FILES := $(wildcard $(PKGSRC)/man/*.Rd)

.PHONY: all clean vignette

all: NEWS README.md roxygen build

README.md: DESCRIPTION
	sed -i 's/Version: *\([^ ]*\)/Version: $(PKGVERS)/' README.md
	sed -i 's/Date: *\([^ ]*\)/Date: $(PKGDATE)/' README.md

gh-pages: $(HELP_FILES)
	R --vanilla --silent -e "library(staticdocs);" \
  -e "build_site('../$(PKGNAME)/', site_path='gh-pages', launch=FALSE)"; \
	rm -f Rplots.pdf  
	git subtree push --prefix gh-pages origin gh-pages

NEWS: NEWS.md
	sed -e 's/^-/  -/' -e 's/^## *//' -e 's/^#/\t\t/' <NEWS.md | fmt -80 >NEWS

roxygen: $(R_FILES)
	R --vanilla --silent -e "library(devtools);" \
		-e "document(roclets='rd')"

vignette: $(VIGNETTE_FILES)
	R CMD Sweave --engine=knitr::rmarkdown $(VIGNETTE_FILES)

build:
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: NEWS README.md roxygen gh-pages build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

clean:
	cd ..;\
	rm -r $(PKGNAME).Rcheck $(PKGNAME)_$(PKGVERS).tar.gz
