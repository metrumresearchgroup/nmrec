
.PHONY: help
help:
	$(info renv: initiate renv and install packages)
	@:

.PHONY: renv
renv:
	Rscript -e 'renv::init(bare = TRUE)' && pkgr install
