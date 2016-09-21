#! /bin/sh

# this script can be used to regenerate the LangKit parser support for the
# libgpr_parser.

# The following variables must be set according to the environment:

#  GNATpython
GPYTHON=/opt/gpython/

#  langkit build directory (for now we use the build directory directly
#  until the langkit project has stabilized and there is a library installed
#  into the environment.
LANGKIT=$HOME/dev/builds/langkit/build/build

#  QUEX installation
QUEX=/opt/quex/quex-0.65.4

#  Now, set the environment and build/install

export PATH=$GPYTHON/bin:$PATH
export PYTHONPATH=$LANGKIT:$PYTHONPATH
export PYTHONPATH=/usr/lib/python2.7/dist-packages:$PYTHONPATH

make QUEX_ROOT=$QUEX build install
