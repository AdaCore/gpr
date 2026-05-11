# Makefile for Sphinx documentation

CP=cp

WORK_DIR=__python__
GPRDOC_FILE=
OUT_FILE=gpr_reference/source/attributes.rst

HTML_DIR = html
PDF_DIR  = pdf

-include ../makefile.setup

# Sphinx sub-documents with their own Makefiles
SPHINX_SUBDOCS = gpr2_library_reference gpr2_quick_start gpr_reference gpr_user_guide

.PHONY: help clean all html pdf

all: $(SPHINX_SUBDOCS:%=subdoc-all-%)

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files (all docs)"
	@echo "  pdf        to make PDF files (all docs)"
	@echo "  all        to build all docs in all formats"

clean:
	for d in $(SPHINX_SUBDOCS); do $(MAKE) -C $$d clean; done
	rm -rf $(HTML_DIR) $(PDF_DIR)

html: $(SPHINX_SUBDOCS:%=subdoc-html-%)

pdf: $(SPHINX_SUBDOCS:%=subdoc-pdf-%)

subdoc-html-%:
	$(MAKE) -C $* html
	mkdir -p $(HTML_DIR)/$*
	cp -r $*/build/html/. $(HTML_DIR)/$*/

subdoc-pdf-%:
	$(MAKE) -C $* pdf
	mkdir -p $(PDF_DIR)
	cp $*/build/latex/*.pdf $(PDF_DIR)/$*.pdf

subdoc-all-%:
	$(MAKE) -C $* all
	mkdir -p $(HTML_DIR)/$* $(PDF_DIR)
	cp -r $*/build/html/. $(HTML_DIR)/$*/
	cp $*/build/latex/*.pdf $(PDF_DIR)/$*.pdf

gen:
	python3 ${WORK_DIR}/generate_gpr2_documentation.py --input=${GPRDOC_FILE} --output=${OUT_FILE}
