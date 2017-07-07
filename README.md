LibGPR2 - Parser for GPR Project files
======================================


INSTALLING THIS LIBRARY
-----------------------

The LibGPR2 library depends on some other external libraries:

- GNATcoll

- Langkit

Both must be installed on the system to be able to compile LibGPR2.

To regenerate the GPR parser from the Langkit grammar the following
additional external modules are required:

- Python

- Langkit (build directory)


CONFIGURING LibGPR2
-------------------

The following variables can be used to configure the libGPR2 library:

   prefix     : location of the installation, the default is the running
                GNAT installation root.

   BUILD      : control the build options : release (default) or debug

   PROCESSORS : parallel compilation (default is 0, depends on the number
                of cores)

   TARGET     : for cross-compilation, auto-detected for native platforms

   SOURCE_DIR : for out-of-tree build

To use the default options:

   $ make setup

For example, to setup libGPR2 to install a debug version on /opt/libgpr2:

   $ make prefix=/opt/libgpr2 BUILD=debug install


BUILDING LibGPR2
----------------

LibGPR2 is using project file, to build it is as simple as:

$ gprbuild gpr2.gpr

Though, to build all versions of the library (static, relocatable and
static-pic) plus the associated tools it is simpler to use the
provided Makefile:

$ make

Then, to install it:

$ make install


USING THE LIBRARY
-----------------

See the libGPR2 examples.


TESTING THE LIBRARY
-------------------

To run the testsuite GNATpython is required:

$ cd testsuite
$ ./testsuite.py


BUG REPORTS
-----------

Please send questions and bug reports to report@gnat.com following
the same procedures used to submit reports with the GNAT toolset itself.
