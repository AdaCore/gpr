LibGPR2 - Parser for GPR Project files
======================================


INSTALLING THIS LIBRARY
-----------------------

The LibGPR2 library depends on some other external libraries:

* [GNATcoll](https://github.com/AdaCore/gnatcoll-core)

* [Langkit](https://github.com/AdaCore/langkit)

Both must be installed on the system to be able to compile LibGPR2.

To regenerate the GPR parser from the Langkit grammar the following additional
external modules are required:

* Python

* Langkit (build directory)


CONFIGURING LibGPR2
-------------------

The following variables can be used to configure the libGPR2 library:

* `prefix`: Location of the installation, the default is the running GNAT
  installation root.

* `BUILD`: Control the build options: `release` (default) or `debug`.

* `PROCESSORS`: Parallel compilation (default is 0, depends on the number of
   cores).

* `TARGET`: For cross-compilation, auto-detected for native platforms.

* `SOURCE_DIR`: For out-of-tree build.

To use the default options:

```sh
$ make setup
```

For example, to setup LibGPR2 to install a debug version on `/opt/libgpr2`:

```sh
$ make prefix=/opt/libgpr2 BUILD=debug install
```


BUILDING LibGPR2
----------------

LibGPR2 is using project file, to build it is as simple as:

```sh
$ gprbuild gpr2.gpr
```

Though, to build all versions of the library (static, relocatable and
static-pic) plus the associated tools it is simpler to use the provided
Makefile:

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

To run the testsuite [GNATpython](https://github.com/Nikokrock/gnatpython) is
required:

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
