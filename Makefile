
.PHONY: help
help:
	$(info renv: initiate renv and install packages)
	$(info )
	$(info check, test, document: run devtools::*())
	$(info check-quick: devtools::check() with no tests or vignettes)
	$(info )
	$(info fmt: run styler::style_pkg())
	@:

.PHONY: check
check:
	Rscript -e 'devtools::check()'

.PHONY: check-quick
check-quick:
	Rscript -e \
	 'devtools::check(args = c("--timings", "--no-tests"), vignettes = FALSE)'

.PHONY: document
document:
	Rscript -e 'devtools::document()'

.PHONY: test
test:
	Rscript -e 'options(warn = 2)' -e 'devtools::test()'

.PHONY: fmt
fmt:
	Rscript -e 'styler::style_pkg()'


.PHONY: renv
renv:
	Rscript -e 'renv::init(bare = TRUE)' && pkgr install
