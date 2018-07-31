##############################################################################
##                                                                          ##
##                            GPR2 PROJECT LIBRARY                          ##
##                                                                          ##
##             Copyright (C) 2017, Free Software Foundation, Inc.           ##
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

MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(patsubst %/,%,$(dir $(MKFILE_PATH)))

DEST := "build"

#  langkit build directory (for now we use the build directory directly
#  until the langkit project has stabilized and there is a library installed
#  into the environment.
export LANGKIT=$(HOME)/dev/builds/langkit/build/build

#  QUEX installation
export QUEX_PATH=$(CURRENT_DIR)/quex

export PYTHONPATH=$(LANGKIT)

setup:
	mkdir -p ${DEST}
	python manage.py --build-dir=${DEST} generate --generate-unparser

clean:
	rm -fr build
