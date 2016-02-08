##############################################################################
##                                                                          ##
##                            PGPR PROJECT PARSER                           ##
##                                                                          ##
##            Copyright (C) 2016, Free Software Foundation, Inc.            ##
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

QUEX_ROOT=/opt/quex/quex-0.65.4

all: build

generate:
	-rm src/*
	python manage.py generate

GPROPTS=-XQUEX_ROOT=$(QUEX_ROOT)

build: generate
	gprbuild -p -m -XLIBRARY_TYPE=static $(GPROPTS) gpr_parser.gpr
	gprbuild -p -m -XLIBRARY_TYPE=relocatable $(GPROPTS) gpr_parser.gpr

force:

uninstall:
	-gprinstall -f --uninstall --prefix=$(PWD)/install gpr_parser.gpr

install: uninstall force
	gprinstall -p --prefix=$(PWD)/install -XLIBRARY_TYPE=static \
		--build-name=static $(GPROPTS) gpr_parser.gpr
	gprinstall -p --prefix=$(PWD)/install -XLIBRARY_TYPE=relocatable \
		--build-name=relocatable $(GPROPTS) gpr_parser.gpr

clean:
	gprclean -XLIBRARY_TYPE=static $(GPROPTS) gpr_parser.gpr
	gprclean -XLIBRARY_TYPE=relocatable $(GPROPTS) gpr_parser.gpr
	rm -fr build install lib obj
