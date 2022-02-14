##############################################################################
##                                                                          ##
##                            GPR2 PROJECT LIBRARY                          ##
##                                                                          ##
##          Copyright (C) 2016-2021, Free Software Foundation, Inc.         ##
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
#   BUILD         : debug / release / release_checks / gnatcov
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   PROFILER      : Include gprof support instrumentation (yes / no)
#   TARGET        : target triplet for cross-compilation

HOST    = $(shell gcc -dumpmachine)
TARGET := $(shell gcc -dumpmachine)

# Look for the source directory (in case of out-of-tree builds):
#
# first let's check if Makefile is symlinked: realpath will return the actual
# (after link resolution) relative path of the Makefile from PWD.
MFILE         := $(shell realpath --relative-to=. "$(firstword ${MAKEFILE_LIST})"))
# as Makefile is in the root dir, SOURCE_DIR is just dirname of the Makefile
# path above.
SOURCE_DIR    := $(shell dirname "${MFILE}")

prefix	      := $(dir $(shell which gnatls))..
BUILD          = release
PROCESSORS     = 0
PROFILER       = no
GPRINSTALL     = gprinstall
PYTHON         = python

# Whether to use gpr<name> or the alternate gpr2<name> tools names
GPR2_TOOLS_PREFIX=gpr

# Whether we want to force a (re-)generation of the langkit parser.
# Set this to "force" to regenerate the parser.
FORCE_PARSER_GEN=

# check for out-of-tree build
ifeq (${SOURCE_DIR},.)
GPR2=gpr2.gpr
GPR2TOOLS=gpr2-tools.gpr
GPR2KB=src/kb/collect_kb.gpr
GPR2KBDIR=src/kb/gprconfig_kb
BUILD_ROOT=.build
else
GPR2=${SOURCE_DIR}/gpr2.gpr
GPR2TOOLS=${SOURCE_DIR}/gpr2-tools.gpr
GPR2KB=${SOURCE_DIR}/src/kb/collect_kb.gpr
GPR2KBDIR=${SOURCE_DIR}/src/kb/gprconfig_kb
BUILD_ROOT=.
endif
LANGKIT_GENERATED_SRC=${BUILD_ROOT}/lkparser
KB_BUILD_DIR=${BUILD_ROOT}/kb
GNATCOV_LEVEL=stmt

ENABLE_SHARED := $(shell gprbuild ${GTARGET} -c -q -p \
        -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}" \
        -P${SOURCE_DIR}/config/test_shared 2>/dev/null && echo "yes")

# Used to pass extra options to GPRBUILD, like -d or -v for instance
GPRBUILD_OPTIONS=

# Load current setup if any
-include makefile.setup

# target options for cross-build
ifeq (${HOST},${TARGET})
GTARGET=
else
GTARGET=--target=${TARGET}
endif

LIBGPR2_TYPES=static
ifeq (${ENABLE_SHARED},yes)
ifneq (${BUILD},gnatcov)
   LIBGPR2_TYPES=static relocatable static-pic
endif
endif

BUILD_TYPES=debug release release_checks gnatcov

ifneq (${BUILD},gnatcov)
   COVERAGE_BUILD_FLAGS=
else
   COVERAGE_BUILD_FLAGS= \
           --implicit-with=gnatcov_rts_full \
           --src-subdirs=gnatcov-instr
   COVERAGE_INSTR_FLAGS= -XBUILD=${BUILD} \
           -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}" \
           -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
           -XLANGKIT_SUPPORT_BUILD=static
   COVERAGE_INSTR=gnatcov instrument --level $(GNATCOV_LEVEL) \
           --dump-trigger=atexit \
            ${COVERAGE_INSTR_FLAGS}
endif

GPR_OPTIONS=${GTARGET} -XBUILD=${BUILD} \
        -XGPR2_TOOLS_PREFIX=${GPR2_TOOLS_PREFIX} \
        -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}"

BUILDER=gprbuild -p -m -j${PROCESSORS} ${GPR_OPTIONS} ${GPRBUILD_OPTIONS} \
             -XPROFILER=${PROFILER} ${COVERAGE_BUILD_FLAGS}
INSTALLER=${GPRINSTALL} -p -f ${GPR_OPTIONS} --prefix='${prefix}'
CLEANER=gprclean -eL -p ${RBD} -XBUILD=${BUILD} \
        -XBUILD_ROOT="${CURDIR}/${BUILD_ROOT}"
UNINSTALLER=${INSTALLER} -p -f --uninstall

.PHONY: force

#########
# build #
#########

all: ${LIBGPR2_TYPES:%=build-%} build-tools

# Knowledge base
${KB_BUILD_DIR}:
	mkdir -p ${KB_BUILD_DIR}

${KB_BUILD_DIR}/config.kb: ${KB_BUILD_DIR} $(wildcard $(GPR2KBDIR)/*)
	gprbuild -p -P ${GPR2KB} -XKB_BUILD_DIR=${KB_BUILD_DIR} --relocate-build-tree
	${KB_BUILD_DIR}/collect_kb -o $@ ${GPR2KBDIR}

# Langkit parser (GPR + Ada support)
${LANGKIT_GENERATED_SRC}: $(wildcard ${SOURCE_DIR}/langkit/language/**/*.py) ${FORCE_PARSER_GEN}
	${MAKE} -C ${SOURCE_DIR}/langkit setup DEST="${CURDIR}/${LANGKIT_GENERATED_SRC}" PYTHONEXE=${PYTHON}
	touch ${LANGKIT_GENERATED_SRC}

# Libgpr2
build-%: ${KB_BUILD_DIR}/config.kb ${LANGKIT_GENERATED_SRC}
ifneq (${BUILD},gnatcov)
	${BUILDER} -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		-XLANGKIT_SUPPORT_BUILD=$* ${GPR2}
else
	echo "gpr2 library built from gpr2-tools in gnatcov mode"
endif

# Gpr2 tools
build-tools: build-static coverage-instrument
	${BUILDER} -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
		-XLANGKIT_SUPPORT_BUILD=static ${GPR2TOOLS}
ifeq (${BUILD},gnatcov)
# ignore the gpr_parser during coverage
	rm ${BUILD_ROOT}/${BUILD}/obj-static/gpr_parser*.sid
	rm ${BUILD_ROOT}/${BUILD}/lib-static/gpr_parser*.sid
endif

# Gnatcov instrumentation
coverage-instrument:
ifeq (${BUILD},gnatcov)
# Remove artifacts from previous instrumentations, so that stale units
# that are not overriden by new ones don't get in our way.
	rm -rf "${BUILD_ROOT}/${BUILD}/obj-*/*gnatcov-instr"
	mkdir -p "${BUILD_ROOT}/${BUILD}"

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

install: uninstall-libs ${LIBGPR2_TYPES:%=install-%} install-tools

install-%:
	${INSTALLER} -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		-XLANGKIT_SUPPORT_BUILD=$* \
		--build-name=$* --build-var=LIBRARY_TYPE \
		--build-var=GPR2_BUILD ${GPR2}

install-tools: uninstall-tools
	${INSTALLER} -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
		-XLANGKIT_SUPPORT_BUILD=static --build-name=static \
		--mode=usage ${GPR2TOOLS}

#########
# setup #
#########

.SILENT: setup setup2

setup:
	echo "prefix=${prefix}" > makefile.setup
	echo "ENABLE_SHARED=${ENABLE_SHARED}" >> makefile.setup
	echo "BUILD=${BUILD}" >> makefile.setup
	echo "PROCESSORS=${PROCESSORS}" >> makefile.setup
	echo "PROFILER=${PROFILER}" >> makefile.setup
ifneq (${HOST},${TARGET})
	echo "TARGET=${TARGET}" >> makefile.setup
endif
	echo "GPR2KBDIR=${GPR2KBDIR}" >> makefile.setup
	echo "GPR2_TOOLS_PREFIX=${GPR2_TOOLS_PREFIX}" >> makefile.setup
	echo "PYTHON=${PYTHON}" >> makefile.setup

setup2: setup
	echo "GPRINSTALL=\"${BUILD_ROOT}/${BUILD}/obj-tools/${GPR2_TOOLS_PREFIX}install\"" >> makefile.setup

###########
# Cleanup #
###########

distclean: clean ${BUILD_TYPES:%=clean-buildtype-%}
	rm -rf ${LANGKIT_GENERATED_SRC}
	rm -f makefile.setup

clean: clean-tools ${LIBGPR2_TYPES:%=clean-libtype-%}
	rm -rf ${KB_BUILD_DIR}
	rm -rf ${BUILD_ROOT}/autoconf
	make -C ${SOURCE_DIR}/langkit clean DEST="${CURDIR}/${LANGKIT_GENERATED_SRC}"

clean-libtype-%:
	-${CLEANER} -XLIBRARY_TYPE=$* -P ${GPR2}

clean-buildtype-%:
	rm -rf ${BUILD_ROOT}/$*

clean-tools:
	-${CLEANER} -XLIBRARY_TYPE=static -P ${GPR2TOOLS}
