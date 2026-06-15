# Makefile for Sphinx documentation

CP=cp

WORK_DIR=__python__
GPRDOC_FILE=
OUT_FILE=gpr_reference/source/attributes.rst

HTML_DIR = html
INFO_DIR = info
PDF_DIR  = pdf
INST_DIR = inst

-include ../makefile.setup

# Sphinx sub-documents with their own Makefiles
SPHINX_SUBDOCS = gpr2_library_reference gpr2_quick_start gpr_reference gpr_user_guide
# This is referenced in many places so is now just an index pointing to the
# other gpr documents
SPHINX_INDEX = gprbuild_ug

SUBDOC_ROOT_gpr2_library_reference = libgpr2
SUBDOC_ROOT_gpr2_quick_start = libgpr2
SUBDOC_ROOT_gpr_reference = gpr
SUBDOC_ROOT_gpr_user_guide = gpr
SUBDOC_ROOT_gprbuild_ug = gprbuild

.PHONY: help clean all html info pdf

all: $(SPHINX_SUBDOCS:%=subdoc-all-%) $(SPHINX_INDEX:%=subdoc-index-%)

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files (all docs)"
	@echo "  info       to make info files (all docs)"
	@echo "  pdf        to make PDF files (all docs)"
	@echo "  all        to build all docs in all formats"

clean:
	for d in $(SPHINX_SUBDOCS); do $(MAKE) -C $$d clean; done
	rm -rf $(INST_DIR)

html: $(SPHINX_SUBDOCS:%=subdoc-html-%) $(SPHINX_INDEX:%=subdoc-index-%)

info: $(SPHINX_SUBDOCS:%=subdoc-info-%)

pdf: $(SPHINX_SUBDOCS:%=subdoc-pdf-%)

subdoc-html-%:
	$(MAKE) -C $* html
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(HTML_DIR)
	cp -r $*/build/html/. $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(HTML_DIR)/

subdoc-index-%:
	$(MAKE) -C $* html
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$(HTML_DIR)
	cp -r $*/build/html/. $(INST_DIR)/$(SUBDOC_ROOT_$*)/$(HTML_DIR)/

subdoc-info-%:
	$(MAKE) -C $* info
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(INFO_DIR)
	cp -r $*/build/texinfo/*.info $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(INFO_DIR)/

subdoc-pdf-%:
	$(MAKE) -C $* pdf
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(PDF_DIR)
	cp $*/build/latex/*.pdf $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(PDF_DIR)/

subdoc-all-%:
	$(MAKE) -C $* all
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(HTML_DIR)
	cp -r $*/build/html/. $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(HTML_DIR)/
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(PDF_DIR)
	cp $*/build/latex/*.pdf $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(PDF_DIR)/
	mkdir -p $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(INFO_DIR)
	cp $*/build/texinfo/*.info $(INST_DIR)/$(SUBDOC_ROOT_$*)/$*/$(INFO_DIR)/

gen:
	python3 ${WORK_DIR}/generate_gpr2_documentation.py --input=${GPRDOC_FILE} --output=${OUT_FILE}
