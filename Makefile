##############################################################################
##                                                                          ##
##                            GPR2 PROJECT LIBRARY                          ##
##                                                                          ##
##          Copyright (C) 2016-2022, Free Software Foundation, Inc.         ##
##                                                                          ##
## This library is free software;  you can redistribute it and/or modify it ##
## under terms of the  GNU General Public License  as published by the Free ##
## Software  Foundation;  either version 3,  or (at your  option) any later ##
## version. This library is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN# ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            ##
##                                                                          ##
## As a special exception under Section 7 of GPL version 3, you are granted ##
## additional permissions described in the GCC Runtime Library Exception,   ##
## version 3.1, as published by the Free Software Foundation.               ##
##                                                                          ##
## You should have received a copy of the GNU General Public License and    ##
## a copy of the GCC Runtime Library Exception along with this program;     ##
## see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    ##
## <http://www.gnu.org/licenses/>.                                          ##
##                                                                          ##
##############################################################################

# Makefile targets
# ----------------
#
# Setup:                   make [VAR=VALUE] setup (see below)
# Build libgpr2:           make
# Install libgpr2:         make install

# Variables which can be set:
#
#   prefix        : root install directory
#   ENABLE_SHARED : yes / no (or empty)
#   GPR2_BUILD    : debug / release / release_checks / gnatcov
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   PROFILER      : Include gprof support instrumentation (yes / no)
#   LOCAL_GPR2    : whether tools should be built with the local gpr2
#                   project or use an installed one. (yes/no)
#   GPR2KBDIR     : path to the gprconfig_kb repository

# Look for the source directory (in case of out-of-tree builds):
#
# Supports symlink of Makefile to the build directory
# Supports building with make -f <gpr path>/Makefile from the build directory
#
# first let's check if Makefile is symlinked: realpath will return the actual
# (after link resolution) relative path of the Makefile from PWD.
MFILE      := $(shell realpath --relative-to=. "$(firstword ${MAKEFILE_LIST})"))
# as Makefile is in the root dir, SOURCE_DIR is just dirname of the Makefile
# path above.
SOURCE_DIR := $(shell dirname "${MFILE}")

# Load current setup if any
-include makefile.setup

# Else setup defaults:
# Install in GNAT by default
prefix	          ?= $(dir $(shell which gnatls))..
# Enable shared libs by default.
ENABLE_SHARED     ?= yes
# Use release mode with assertions
GPR2_BUILD        ?= release_checks
# Use max number of processors by default
PROCESSORS        ?= 0
# Don't use gcov by default
PROFILER          ?= no
# Use local libgpr2 sources to build the gpr2 tools
LOCAL_GPR2        ?= yes
# gprconfig_kb repository location
GPR2KBDIR         ?= ${SOURCE_DIR}/../gprconfig_kb/db
# Used to pass extra options to GPRBUILD, like -d or -v for instance
GPRBUILD_OPTIONS  ?=
# Level of coverage requested from gnatcov code coverage testing
GNATCOV_LEVEL     ?= stmt
# To override in case the python executable is not named "python" when
# generating the langkit parser
PYTHON            ?= python

# Build modes for this repository:
# * debug: no optimisation and debug info
# * release: code optimisation and no assertion or contracts checking
# * release_checks: code optimisation with assertions and contracts checks
# * gnatcov: code optimisation with gnat coverage instrumentation and assertions
BUILD_TYPES       := debug release release_checks gnatcov

# Location of the project files
GPR2              := ${SOURCE_DIR}/gpr2.gpr
GPR2TOOLS         := ${SOURCE_DIR}/tools/gpr2-tools.gpr
GPR2KB            := ${SOURCE_DIR}/src/kb/collect_kb.gpr

# adapt build dirs to out-of-tree builds
ifeq (${SOURCE_DIR},.)
BUILD_ROOT        := .build
else
BUILD_ROOT        := .
endif

KB_BUILD_DIR      := ${BUILD_ROOT}/kb

# Do not build static-pic and relocatable libs if shared libs are not supported
LIBGPR2_TYPES     := static
ifeq (${ENABLE_SHARED},yes)
  ifneq (${GPR2_BUILD},gnatcov)
    LIBGPR2_TYPES := static relocatable static-pic
  endif
endif

# Add SOURCE_DIR in the project search path if LOCAL_GPR2 is set
AP_GPR2           :=
BUILD_LIBGPR2     :=
ifeq (${LOCAL_GPR2},yes)
  AP_GPR2         := -aP ${SOURCE_DIR}
  BUILD_LIBGPR2   := build-lib-static
endif

# Code coverage support
ifneq (${GPR2_BUILD},gnatcov)
   COVERAGE_BUILD_FLAGS :=
else
   COVERAGE_BUILD_FLAGS := \
           --implicit-with=gnatcov_rts \
           --src-subdirs=gnatcov-instr
   COVERAGE_INSTR_FLAGS := -XGPR2_BUILD=${GPR2_BUILD} \
           -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}" \
           -XLIBRARY_TYPE=static -XXMLADA_BUILD=static
   COVERAGE_INSTR       := gnatcov instrument --level $(GNATCOV_LEVEL) \
           --dump-trigger=atexit \
            ${COVERAGE_INSTR_FLAGS}
endif

# Command line helpers:
GPR_OPTIONS := -XGPR2_BUILD=${GPR2_BUILD} \
               -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}"

BUILDER     := gprbuild -p -j${PROCESSORS} ${GPR_OPTIONS} \
               ${GPRBUILD_OPTIONS} -XPROFILER=${PROFILER} \
               ${COVERAGE_BUILD_FLAGS}
INSTALLER   := gprinstall -p -f ${GPR_OPTIONS} --prefix='${prefix}'
CLEANER     := gprclean -eL -p ${RBD} -XGPR2_BUILD=${GPR2_BUILD} \
               -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}"
UNINSTALLER := ${INSTALLER} -p -f --uninstall

# attributes documentation generation
GPRDOC      := ${BUILD_ROOT}/${GPR2_BUILD}/gprdoc
DOCOUT      := ${BUILD_ROOT}/attributes.json

.PHONY: force doc

#########
# build #
#########

all: ${LIBGPR2_TYPES:%=build-lib-%} build-tools

# Knowledge base
${KB_BUILD_DIR}:
	mkdir -p ${KB_BUILD_DIR}

${KB_BUILD_DIR}/config.kb: ${KB_BUILD_DIR} $(wildcard $(GPR2KBDIR)/*)
	gprbuild -p -P ${GPR2KB} -XKB_BUILD_DIR=${KB_BUILD_DIR} --relocate-build-tree
	${KB_BUILD_DIR}/collect_kb -o $@ ${GPR2KBDIR}

# Libgpr2
build-lib-%: ${KB_BUILD_DIR}/config.kb
ifneq (${GPR2_BUILD},gnatcov)
	${BUILDER} -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		${GPR2}
else
	echo "gpr2 library built from gpr2-tools in gnatcov mode"
endif

# Gpr2 tools
build-tools: ${BUILD_LIBGPR2} coverage-instrument
	${BUILDER} -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
		${GPR2TOOLS} ${AP_GPR2}

# Gnatcov instrumentation
coverage-instrument:
ifeq (${GPR2_BUILD},gnatcov)
# Remove artifacts from previous instrumentations, so that stale units
# that are not overriden by new ones don't get in our way.
	rm -rf "${BUILD_ROOT}/${GPR2_BUILD}/obj-*/*gnatcov-instr"
	mkdir -p "${BUILD_ROOT}/${GPR2_BUILD}"

	${COVERAGE_INSTR} -P ${GPR2TOOLS}
endif

###########
# Install #
###########

uninstall-libs:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/gpr2))
	${UNINSTALLER} $(notdir ${GPR2})
endif

uninstall-tools:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/gpr2-tools))
	${UNINSTALLER} $(notdir ${GPR2TOOLS})
endif

install: uninstall-libs ${LIBGPR2_TYPES:%=install-lib-%} install-tools

install-libs: uninstall-libs ${LIBGPR2_TYPES:%=install-lib-%}

install-lib-%:
	${INSTALLER} -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		--build-name=$* \
		--build-var=LIBRARY_TYPE \
		--build-var=GPR2_LIBRARY_TYPE \
		${GPR2}

install-tools: uninstall-tools
	${INSTALLER} -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
		--build-name=static --mode=usage ${GPR2TOOLS} ${AP_GPR2}

#########
# setup #
#########

.SILENT: setup setup2

setup:
	echo "prefix           ?= ${prefix}" > makefile.setup
	echo "ENABLE_SHARED    ?= ${ENABLE_SHARED}" >> makefile.setup
	echo "GPR2_BUILD       ?= ${GPR2_BUILD}" >> makefile.setup
	echo "PROCESSORS       ?= ${PROCESSORS}" >> makefile.setup
	echo "PROFILER         ?= ${PROFILER}" >> makefile.setup
	echo "LOCAL_GPR2       ?= ${LOCAL_GPR2}" >> makefile.setup
	echo "GPR2KBDIR        ?= ${GPR2KBDIR}" >> makefile.setup
	echo "GPRBUILD_OPTIONS ?= ${GPRBUILD_OPTIONS}" >> makefile.setup
	echo "GNATCOV_LEVEL    ?= ${GNATCOV_LEVEL}" >> makefile.setup
	echo "PYTHON           ?= ${PYTHON}" >> makefile.setup

###########
# Cleanup #
###########

distclean: clean ${BUILD_TYPES:%=clean-buildtype-%}
	rm -f makefile.setup

clean: clean-tools ${LIBGPR2_TYPES:%=clean-libtype-%}
	rm -rf ${KB_BUILD_DIR}
	rm -rf ${BUILD_ROOT}/autoconf
	make -C ${SOURCE_DIR}/doc clean

clean-libtype-%:
	-${CLEANER} -XLIBRARY_TYPE=$* -P ${GPR2}

clean-buildtype-%:
	rm -rf ${BUILD_ROOT}/$*

clean-tools:
	-${CLEANER} -XLIBRARY_TYPE=static -P ${GPR2TOOLS} ${AP_GPR2}

#################
# Documentation #
#################

${DOCOUT}: force

docgen:
	${GPRDOC} > ${DOCOUT}
	${GPRDOC} --display=json > ${SOURCE_DIR}/testsuite/tests/gprdoc/attrs.json
	make -C ${SOURCE_DIR}/doc gen GPRDOC_FILE="../${DOCOUT}"


###########
# Langkit #
###########

# Langkit parser (GPR project parser)
update-langkit: $(wildcard ${SOURCE_DIR}/langkit/language/**/*.py)
	rm -rf ${SOURCE_DIR}/langkit/gen
	${MAKE} -C ${SOURCE_DIR}/langkit setup DEST="gen" PYTHONEXE=${PYTHON}
