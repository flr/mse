PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDATE := $(shell sed -n "s/Date: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

GITDATE=$(shell (git log -1 --date=short --pretty=format:"%ad"))
GITVERS=$(shell (date -d `git log -1 --date=short --pretty=format:"%ad"` +%Y%m%d))

R_FILES := $(wildcard $(PKGSRC)/R/*.R)
HELP_FILES := $(wildcard $(PKGSRC)/man/*.Rd)
VIGNETTES := $(wildcard $(PKGSRC)/vignettes/*.Rmd)

all: build

.PHONY: all release roxygen

README.md: DESCRIPTION
	sed -i 's/Version: *\([^ ]*\)/Version: $(PKGVERS)/' README.md
	sed -i 's/Date: *\([^ ]*\)/Date: $(PKGDATE)/' README.md

NEWS: NEWS.md
	sed 's/^# / /' NEWS.md > NEWS
	sed -i 's/^##//' NEWS

docs: $(HELP_FILES) $(VIGNETTES) README.md NEWS
	R --vanilla --silent -e "options(repos='http://cran.r-project.org'); pkgdown::build_site(preview=FALSE)"

roxygen: $(R_FILES)
	R --vanilla --silent -e "library(devtools);" \
		-e "document(roclets='rd')"

$(HELP_FILES): $(R_FILES)
	R --vanilla --silent -e "library(devtools);" \
		-e "document(roclets='rd')"

update:
	sed -i 's/Date: *\([^ ]*\)/Date: $(GITDATE)/' DESCRIPTION

release: build docs
	
build: README.md NEWS
	cd ..;\
	R CMD build $(PKGSRC) --compact-vignettes

buildNV: README.md NEWS
	cd ..;\
	R CMD build $(PKGSRC) --no-build-vignettes

install: ../$(PKGNAME)_$(PKGVERS).tar.gz
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

checkCRAN: ../$(PKGNAME)_$(PKGVERS).tar.gz
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

check: ../$(PKGNAME)_$(PKGVERS).tar.gz
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	cd ..;\
	rm -rf $(PKGNAME).Rcheck $(PKGNAME)_$(PKGVERS).tar.gz

# TARGETS
#
# - README.md: Updates README.md version and date from DESCRIPTION
# - NEWS: Uppdates NEWS in R format from NEWS.md
# - docs: Creates html pages at flr-project.org/PKG
# - roxygen: Runs roxygen to create Rd files
# - update: Changfes date in DESCRIPTION to last git commit date
# - release: build docs
# - build: R CMD build
# - buildNV: R CMD build without vignettes
# - install: R CMD INSTALL
# - checkCRAN: R CMD check --as-cran
# - check: R CMD check
# - clean: remove tar.gz and .Rcheck
