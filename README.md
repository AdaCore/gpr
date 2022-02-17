LibGPR2 - Parser for GPR Project files
======================================


INSTALLING THIS LIBRARY
-----------------------

The LibGPR2 library depends on some other external libraries:

* [GNATcoll](https://github.com/AdaCore/gnatcoll-core)

* [Langkit](https://github.com/AdaCore/langkit)

Both must be installed on the system to be able to compile LibGPR2.

The library also depends on the Knowledge Base containing information
of different targets, runtimes, compilers, etc.:

* [GPRCONFIG KB](https://github.com/AdaCore/gprconfig_kb)

To regenerate the GPR parser from the Langkit grammar the following additional
external modules are required:

* Python

* Langkit (build directory)


CONFIGURING LibGPR2
-------------------

The following variables can be used to configure the libGPR2 library:

* `prefix`: Location of the installation, the default is the running GNAT
  installation root.

* `GPR2_BUILD`: Control the build options: `release` (default) or `debug`.

* `PROCESSORS`: Parallel compilation (default is 0, depends on the number of
   cores).

* `TARGET`: For cross-compilation, auto-detected for native platforms.

* `SOURCE_DIR`: For out-of-tree build.

* `GPR2KBDIR`: Location of the Knowledge Base contents (by default library
   expects them in src/kb/gprconfig_kb).

To use the default options:

```sh
$ make setup
```

For example, to setup LibGPR2 to install a debug version on `/opt/libgpr2`:

```sh
$ make setup prefix=/opt/libgpr2 GPR2_BUILD=debug install
```

To point out the location of the Knowledge Base (assuming it has been checked
out to /usr/share/gprconfig_kb):

```sh
$ make setup GPR2KBDIR=/usr/share/gprconfig_kb/db
```

BUILDING LibGPR2
----------------

To build all versions of the library (static, relocatable and
static-pic) plus the associated tools use the provided Makefile:

```sh
$ make
```

Then, to install it:

```sh
$ make install
```


USING THE LIBRARY
-----------------

See the LibGPR2 [examples](examples).


TESTING THE LIBRARY
-------------------

To run the testsuite [e3-core](https://github.com/AdaCore/e3-core) and
[e3-testsuite](https://github.com/AdaCore/e3-testsuite) are required:

```sh
$ cd testsuite
$ ./testsuite.py
```


BUG REPORTS
-----------

If you got this source file from GNATtracker, please send questions and bug
reports to report@gnat.com following the same procedures used to submit reports
with the GNAT toolset itself.

If you read this from the [GitHub repository](https://github.com/AdaCore/gpr),
please [open issues](https://github.com/AdaCore/gpr/issues) to send questions
and bug reports.
