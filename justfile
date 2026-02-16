PKG := "SeverityEstimate"
VERSION := `Rscript -e "cat(read.dcf('DESCRIPTION')[,'Version'])"`
TARBALL := PKG + "_" + VERSION + ".tar.gz"

default: clean format lint docs test

[unix]
[doc('Clean up auxiliary files and directories')]
clean:
	rm -f  *.tar.gz
	rm -rf ..Rcheck/
	rm -rf .Rproj.user/

[doc('Build man pages using roxygen')]
docs:
	#!/usr/bin/env Rscript
	if (require(roxygen2)) roxygen2::roxygenize() else stop("missing 'roxygen2'")

[doc('Format R code using air')]
format:
	air format .

[doc('Check R code using air')]
lint: lintair lintr

[doc('Check R code using air')]
lintair:
	air format . --check

[doc('Check R code using lintr')]
lintr:
	#!/usr/bin/env Rscript
	if (require(devtools)) devtools::load_all() else stop("missing 'devtools'")
	if (require(lintr))	lintr::lint_package() else stop("missing 'lintr'")
	

[doc('Run unit tests using devtools')]
test:
	#!/usr/bin/env Rscript 
	library(devtools)
	devtools::test()

[doc('Run unit tests using devtools, stopping on first failure')]
test-fast:
	#!/usr/bin/env Rscript
	library(devtools)
	devtools::test(stop_on_failure=TRUE)

[group('renv')]
[doc('Install package dependencies using renv')]
renv-install:
	#!/usr/bin/env Rscript
	library(renv)
	renv::install(dependencies='most')

[group('renv')]
[doc('Install and update dependencies using renv')]
renv-update:
	#!/usr/bin/env Rscript
	library(renv)
	renv::install(dependencies='most')
	renv::update()

[group('renv')]
[doc('Install, update, and snapshot new updates using renv')]
renv-snapshot:
	#!/usr/bin/env Rscript
	library(renv)
	renv::install(dependencies='most')
	renv::update()
	renv::snapshot()

[doc('Install development version of SeverityEstimate')]
install: renv-install
	R CMD INSTALL .

[doc('Remove development version of SeverityEstimate')]
remove:
	R CMD REMOVE {{ PKG }}

[doc('Build a tar.gz artifact')]
build:
	R CMD build .

[doc('Check the built tar.gz artifact')]
check: build
	R CMD check {{ TARBALL }} --no-manual --no-tests
