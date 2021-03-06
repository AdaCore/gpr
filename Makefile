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
#   BUILD         : debug release
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   TARGET        : target triplet for cross-compilation

HOST    = $(shell gcc -dumpmachine)
TARGET := $(shell gcc -dumpmachine)

prefix	      := $(dir $(shell which gnatls))..
BUILD         = release
PROCESSORS    = 0
BUILD_DIR     =
SOURCE_DIR    := $(shell dirname "$(MAKEFILE_LIST)")
ENABLE_SHARED := $(shell gprbuild $(GTARGET) -c -q -p \
	-P$(MAKEPREFIX)config/test_shared 2>/dev/null && echo "yes")
GPRINSTALL=gprinstall

# Load current setup if any
-include makefile.setup

# check for out-of-tree build
ifeq ($(SOURCE_DIR),.)
RBD=
GPR2=gpr2.gpr
GPR2TOOLS=gpr2-tools.gpr
GPR2KB=src/kb/collect_kb.gpr
GPR2KBDIR=src/kb/gprconfig_kb
MAKEPREFIX=
LANGKIT_GENERATED_SRC=langkit/build
else
RBD=--relocate-build-tree
GPR2=$(SOURCE_DIR)/gpr2.gpr
GPR2TOOLS=$(SOURCE_DIR)/gpr2-tools.gpr
GPR2KB=$(SOURCE_DIR)/src/kb/collect_kb.gpr
GPR2KBDIR=$(SOURCE_DIR)/src/kb/gprconfig_kb
MAKEPREFIX=$(SOURCE_DIR)/
LANGKIT_GENERATED_SRC=$(shell pwd)/langkit/build
endif

# target options for cross-build
ifeq ($(HOST),$(TARGET))
GTARGET=
else
GTARGET=--target=$(TARGET)
endif

ifeq ($(ENABLE_SHARED), yes)
   LIBGPR2_TYPES=static relocatable static-pic
else
   LIBGPR2_TYPES=static
endif

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

GPR_OPTIONS=$(GTARGET) $(RBD) -XBUILD=${BUILD} \
	-aP ${LANGKIT_GENERATED_SRC}/lib/gnat \
	-XLANGKIT_GENERATED_SRC=${LANGKIT_GENERATED_SRC}

BUILDER=gprbuild -p -m -j${PROCESSORS} ${GPR_OPTIONS} ${GPRBUILD_OPTIONS}
INSTALLER=${GPRINSTALL} -p -f ${GPR_OPTIONS} --prefix=${prefix}
CLEANER=gprclean -q $(RBD)
UNINSTALLER=$(INSTALLER) -p -f --uninstall

#########
# build #
#########

all: kb build build-tools

kb:
	gprbuild -p $(GPR2KB)
	$(SOURCE_DIR)/src/kb/collect_kb -o $(SOURCE_DIR)/src/kb/config.kb \
		$(GPR2KBDIR)

build: ${LIBGPR2_TYPES:%=build-%}

build-%:
	$(BUILDER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		-XLANGKIT_SUPPORT_BUILD=$* $(GPR2)

build-tools:
	$(BUILDER) -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
		-XLANGKIT_SUPPORT_BUILD=static $(GPR2TOOLS)

###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/gpr2))
	$(UNINSTALLER) --install-name=gpr2 $(GPR2)
endif

install: uninstall ${LIBGPR2_TYPES:%=install-%} install-tools

install-%:
	$(INSTALLER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		-XLANGKIT_SUPPORT_BUILD=$* \
		--build-name=$* --build-var=LIBRARY_TYPE \
		--build-var=GPR2_BUILD $(GPR2)

install-tools:
	$(INSTALLER) -XLIBRARY_TYPE=static -XXMLADA_BUILD=static \
		-XLANGKIT_SUPPORT_BUILD=static --build-name=static \
		--mode=usage --install-name=gpr2 $(GPR2TOOLS)

#########
# setup #
#########

.SILENT: setup setup2

setup: langkit/build
	echo "prefix=$(prefix)" > makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "BUILD=$(BUILD)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup
	echo "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup
	echo "GPR2KBDIR=$(GPR2KBDIR)" >> makefile.setup

setup2: setup
	echo "GPRINSTALL=.build/$(BUILD)/obj-tools/gpr2install" >> makefile.setup

langkit:
	mkdir -p langkit

langkit/build: langkit
	$(MAKE) -C ${SOURCE_DIR}/langkit setup DEST=$(shell pwd)/langkit/build

###########
# Cleanup #
###########

clean: ${LIBGPR2_TYPES:%=clean-%}

clean-%:
	-$(CLEANER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* $(GPR2)
	make -C langkit clean
