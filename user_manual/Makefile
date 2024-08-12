SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

generate:
	LKQL_PATH=$(ROOT_DIR)/../lkql_checker/share/lkql lkql doc --std -O generated stdlib

html: Makefile generate
	LKQL_PATH=$(ROOT_DIR)/../lkql_checker/share/lkql lkql doc --std -O generated stdlib
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

clean:
	rm -rf ./generated ./build

.PHONY: help Makefile
