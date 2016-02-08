#! /bin/sh

# this script can be used to regenerate the LangKit parser support for the
# libgpr_parser.

# The following variables must be set according to the environment:

#  GNATpython
GPYTHON=/opt/gpython/

#  LibAdaLang build directory (for now we use the build directory directly
#  until the libadalang project as stabilized and there is a library installed
#  into the environment.
ADALANG=$HOME/dev/builds/libadalang/build/build

#  QUEX installation
QUEX=/opt/quex/quex-0.65.4

#  Now, set the environment and build/install

export PATH=$GPYTHON/bin:$PATH
export PYTHONPATH=$ADALANG:$PYTHONPATH
export PYTHONPATH=/usr/lib/python2.7/dist-packages:$PYTHONPATH

make QUEX_ROOT=$QUEX build install
