
.PHONY: help
help:
	$(info renv: initiate renv and install packages)
	$(info )
	$(info check-all: devtools::check() and pkgdown::check_pkgdown())
	$(info check, test, document: run devtools::*())
	$(info check-quick: devtools::check() with no tests or vignettes)
	$(info check-pkgdown: run pkgdown::check_pkgdown())
	$(info cov: run tests and display coverage report)
	$(info )
	$(info fmt: run styler::style_pkg())
	$(info )
	$(info site: build site under docs/)
	@:

.PHONY: check-all
check-all: check check-pkgdown

check_base = error_on = "note", env_vars = c("NOT_CRAN" = "true", "_R_CHECK_SYSTEM_CLOCK_" = "false")

.PHONY: check
check:
	Rscript -e 'devtools::check($(check_base))'

.PHONY: check-quick
check-quick:
	Rscript -e \
	 'devtools::check($(check_base), args = c("--timings", "--no-tests"), vignettes = FALSE)'

.PHONY: check-pkgdown
check-pkgdown:
	Rscript -e 'pkgdown::check_pkgdown()'

.PHONY: document
document:
	Rscript -e 'devtools::document()'

.PHONY: test
test:
	Rscript -e 'options(warn = 2)' -e 'devtools::test()'

.PHONY: cov
cov:
	mkdir -p coverage
	Rscript -e 'covr::report(file = "coverage/index.html", browse = TRUE)'

.PHONY: fmt
fmt:
	Rscript -e 'styler::style_pkg()'

.PHONY: site
site:
	Rscript -e 'pkgdown::build_site()'

.PHONY: renv
renv:
	Rscript -e 'renv::init(bare = TRUE)' && pkgr install
