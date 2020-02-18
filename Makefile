PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDATE := $(shell sed -n "s/Date: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

GITDATE=$(shell (git log -1 --date=short --pretty=format:"%ad"))
GITVERS=$(shell (date -d `git log -1 --date=short --pretty=format:"%ad"` +%Y%m%d))

R_FILES := $(wildcard $(PKGSRC)/R/*.R)
HELP_FILES := $(wildcard $(PKGSRC)/man/*.Rd)
VIGNETTES := $(wildcard $(PKGSRC)/vignettes/*.Rmd)

all: build ## Defaults to build only

.PHONY: all release roxygen help

README.md: DESCRIPTION  ## Updates README.md version and date from DESCRIPTION
	sed -i 's/Version: *\([^ ]*\)/Version: $(PKGVERS)/' README.md
	sed -i 's/Date: *\([^ ]*\)/Date: $(PKGDATE)/' README.md

NEWS: NEWS.md ## Recreates NEWS from NEWS.md
	sed 's/^# / /' NEWS.md > NEWS
	sed -i 's/^#\{2\}//' NEWS

docs: $(HELP_FILES) $(VIGNETTES) README.md NEWS  ## Create html pages at FLR
	R --vanilla --silent -e "options(repos='http://cran.r-project.org'); pkgdown::build_site(preview=FALSE)"

roxygen: $(R_FILES) ## Update Rd files from roxygen
	R --vanilla --silent -e "library(devtools);" \
		-e "document(roclets='rd')"

$(HELP_FILES): $(R_FILES)
	R --vanilla --silent -e "library(devtools);" \
		-e "document(roclets='rd')"

update: ## Change date in DESCRIPTION to last git commit date
	sed -i 's/Date: *\([^ ]*\)/Date: $(GITDATE)/' DESCRIPTION

release: build docs ## build docs
	
build: README.md NEWS ## R CMD build
	cd ..;\
	R CMD build $(PKGSRC) --compact-vignettes

buildNV: README.md NEWS  ## R CMD build --no-build-vignettes
	cd ..;\
	R CMD build $(PKGSRC) --no-build-vignettes

install: ../$(PKGNAME)_$(PKGVERS).tar.gz  ## R CMD INSTALL
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

checkCRAN: ../$(PKGNAME)_$(PKGVERS).tar.gz ## R CMD check --as-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

check: ../$(PKGNAME)_$(PKGVERS).tar.gz  ## R CMD check
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

clean:  ## remove tar.gz and *.Rcheck
	cd ..;\
	rm -rf $(PKGNAME).Rcheck $(PKGNAME)_$(PKGVERS).tar.gz

help:  ## Show this help.
	@echo ""
	@sed -e '/#\{2\}/!d; s/\\$$//; s/:[^#]*/:/; s/#\{2\} */\t\t/' $(MAKEFILE_LIST)
	@echo ""

