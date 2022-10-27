# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

GPR2_ROOT=../../
GPR2_GPR=$(GPR2_ROOT)gpr2.gpr
GPR2DOC_GPR=$(GPR2_ROOT)gpr2doc.gpr
GPR2DOC_OUTPUT_DIR=tmp
GPR2DOC_DIR=$(GPR2_ROOT)$(GPR2DOC_OUTPUT_DIR)/
STATIC_DIR=$(SOURCEDIR)/_static/

EXCLUDED_FILE=gpr2-builtin.ads,\
gpr2-containers.ads,\
gpr2-file_readers.ads,\
gpr2-kb-compiler_iterator.ads,\
gpr2-kb-parsing.ads,\
gpr2-kb.ads,\
gpr2-path_name-set.ads,\
gpr2-project-attr_values.ads,\
gpr2-project-attribute-set.ads,\
gpr2-project-attribute_cache.ads,\
gpr2-project-attribute_index.ads,\
gpr2-project-definition.ads,\
gpr2-project-import-set.ads,\
gpr2-project-import.ads,\
gpr2-project-name_values.ads,\
gpr2-project-parser-create.ads,\
gpr2-project-parser-registry.ads,\
gpr2-project-parser-set.ads,\
gpr2-project-parser.ads,\
gpr2-project-pretty_printer.ads,\
gpr2-project-registry-attribute-description.ads,\
gpr2-project-registry-pack.ads,\
gpr2-project-registry.ads,\
gpr2-project-source-artifact.ads,\
gpr2-project-source-part_set.ads,\
gpr2-project-source-set.ads,\
gpr2-project-typ-set.ads,\
gpr2-project-unit_info-set.ads,\
gpr2-project-variable-set.ads,\
gpr2-project-view-set.ads,\
gpr2-project-view-vector.ads,\
gpr2-project.ads,\
gpr2-source_info-parser-ada_language.ads,\
gpr2-source_info-parser-ali.ads,\
gpr2-source_info-parser-d.ads,\
gpr2-source_info-parser-registry.ads,\
gpr2-source_info-parser.ads,\
gpr2-source_reference-attribute.ads,\
gpr2-source_reference-identifier-set.ads,\
gpr2-source_reference-identifier.ads,\
gpr2-source_reference-pack.ads,\
gpr2-source_reference-scalar_value.ads,\
gpr2-source_reference-text_value-g_set.ads,\
gpr2-source_reference-text_value.ads,\
gpr2-source_reference-value.ads,\
gpr2-source_reference.ads,\
gpr2-unit-list.ads,\
gpr2-view_ids-dags.ads,\
gpr2-view_ids-set.ads,\
gpr2-view_ids-vector.ads,\
gpr2-view_ids.ads

all: Makefile
	# remove gnatcoll_iconv & gnatcoll_gmp deps
	sed "s/with .gnatcoll_/--  /" $(GPR2_GPR) > $(GPR2DOC_GPR)
	# remove generated sources references
	sed -i "s/KB_Build/--  /" $(GPR2DOC_GPR)
	sed -i "s/Langkit_/--  /" $(GPR2DOC_GPR)
	sed -i "s/.src.kb.,/\"src\/kb\"\);/" $(GPR2DOC_GPR)
	sed -i "s/.*gpr_parser/--  /" $(GPR2DOC_GPR)
	# update gpr2doc.gpr output dirs
	sed -i "s/for Object_Dir/for Object_Dir use \"$(GPR2DOC_OUTPUT_DIR)\";--/" $(GPR2DOC_GPR)
	sed -i "s/for Library_Dir/for Library_Dir use \"$(GPR2DOC_OUTPUT_DIR)\/lib\";--/" $(GPR2DOC_GPR)
	# generate doc in $(GPR2DOC_DIR)gnatdoc
	#mkdir -p $(GPR2DOC_DIR)
	#gnatdoc --preserve-source-formatting  --no-subprojects --enable-build -P $(GPR2DOC_GPR) --ignore-files="$(EXCLUDED_FILE)"
	# copy generated doc in $(STATIC_DIR)
	mkdir -p $(STATIC_DIR)
	#cp -r $(GPR2DOC_DIR)gnatdoc $(STATIC_DIR)
	# generate gpr2 user manual
	@$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	@$(SPHINXBUILD) -M latexpdf "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	