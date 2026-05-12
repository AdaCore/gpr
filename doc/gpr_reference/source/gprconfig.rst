.. _RM_GPRconfig:

.. index:: GPRconfig

*******************
GPRconfig Reference
*******************

GPRconfig probes the host for available compilers and generates a
configuration project (``.cgpr``) that describes the selected toolchains to
all GPR tools. It can be used interactively or in batch mode from a script.

GPRconfig is independent of any GPR project tree. It does not accept the
common project and configuration switches described in
:ref:`RM_Common_Options`; its option set is documented entirely in this
chapter.


Command Line
============

Syntax
------

::

   gprconfig [switches]


Compiler selection
------------------

GPRconfig scans ``PATH`` (and any directories added by ``--config``) for
compiler executables, cross-references them against the knowledge base, and
builds a list of available compilers. In interactive mode the user selects
which compilers to include; in batch mode (``--batch``) the selection is
driven entirely by ``--config`` options on the command line.

At most one compiler per language may be selected. The output configuration
project contains one ``package Compiler`` section per selected language.


Switches
========

.. index:: pair: GPRconfig; target, pair: switch; --target

Target and output
-----------------

``--target=``\ *name*
  Select compilers for the given target. Use ``all`` to list compilers for
  every available target. Defaults to the native target. In batch mode,
  ``--target=all`` is silently ignored.

``-o`` *file*
  Write the generated configuration project to *file*. Defaults to
  ``default.cgpr`` when no target is specified, or ``<target>.cgpr`` when a
  target is given.

``--show-targets``
  Print the list of targets for which at least one compiler is available in
  the knowledge base, then exit. The native target is marked ``(native
  target)``.

``--mi-show-compilers``
  Print a machine-readable list of all available compilers and exit. Each
  compiler is described as a set of ``key=value`` pairs; compilers already
  selected for output are prefixed with ``*``. Intended for IDE integration.

``--show-known-compilers``
  Print the names of all compiler blocks defined in the knowledge base, then
  exit.


.. index:: pair: GPRconfig; batch mode, pair: switch; --batch, pair: switch; --config

Compiler selection (batch mode)
-------------------------------

``--batch``
  Run in batch (non-interactive) mode. No prompts are displayed. The
  compilers to include must be specified entirely via ``--config`` options.

``--config=``\ *selector*
  Pre-select a compiler. May be repeated, at most once per language.

  The selector has two equivalent forms:

  **Positional:** ``language[,version[,runtime[,path[,name]]]]``

  .. code-block:: text

     ada
     ada,12.1
     ada,,zfp
     ada,,zfp,/opt/gnat/bin,gnat

  **Named:** ``key:value[,key:value...]``

  .. code-block:: text

     language:ada
     language:ada,version:12.1
     language:ada,runtime:zfp
     language:ada,runtime:zfp,path:/opt/gnat/bin,name:gnat

  Available keys/fields:

  ``language``
    Required. Language identifier (``ada``, ``c``, ``c++``, ...).

  ``version``
    Optional. Compiler version string (e.g. ``12.1``).

  ``runtime``
    Optional. Runtime variant (e.g. ``sjlj``, ``zfp``, ``ravenscar``).

  ``path``
    Optional. Directory containing the compiler executable.

  ``name``
    Optional. Compiler name as defined in the knowledge base (e.g.
    ``GCC``, ``GNAT``) or the base name of the compiler executable
    (e.g. ``gcc``, ``arm-elf-gnatmake``).

  Omitted optional fields match any value. An empty string also matches any
  value, so ``ada,,zfp`` is equivalent to ``language:ada,runtime:zfp``.

``--fallback-targets``
  When no compiler is found for the requested target, also search for native
  toolchains of a different architecture. Useful on systems where a
  cross-compiler is registered under a different target triple.


Knowledge base
--------------

``--db`` *dir*
  Parse *dir* as an additional knowledge base directory (may be repeated).

``--db-``
  Do not load the standard knowledge base. Use together with ``--db`` to
  load a completely custom knowledge base.

``--validate``
  Validate the knowledge base before use and report any schema errors, then
  proceed normally.


Diagnostics
-----------

``-v``
  Verbose output. Repeat (``-v -v``) for even more detail, including
  internal knowledge base traces.

``-q``
  Quiet: suppress all output except errors.


.. index:: pair: GPRconfig; interactive mode

Interactive mode
================

When run without ``--batch``, GPRconfig displays the list of compilers found
for the selected target (one per line, with an index), prompts the user to
select or deselect entries by typing their index number, and writes the
configuration when the user types ``s``. Selecting a compiler for a language
automatically deselects any previously selected compiler for the same
language.

After saving, GPRconfig prints the equivalent ``--batch`` command line so
that the same selection can be reproduced in scripts.


.. index:: pair: GPRconfig; configuration file format, cgpr

Configuration file format
=========================

The generated file is a valid GPR configuration project (``configuration``
qualifier, ``.cgpr`` extension). It defines:

- ``package Compiler`` - compiler driver, default switches, dependency
  file format, and naming conventions for each selected language.
- ``package Linker`` - linker driver and default flags.
- ``package Binder`` - binder driver (for Ada).
- ``package Archive_Builder`` - archiver command.
- Top-level attributes - ``Target``, ``Canonical_Target``,
  ``Runtime_Library_Dir``, ``Object_Generated``, ``Library_Support``,
  ``Shared_Library_Prefix``, ``Shared_Library_Suffix``, and related
  platform attributes.

The file is consumed by GPRbuild and the other project-based tools via
``--config`` or ``--autoconf``. It is not a user project file and is not
referenced by ``with`` clauses.


Exit Codes
==========

``0``
  Success; configuration file written (or information displayed).

``1``
  No compiler found for the requested target or language, knowledge
  base error, or configuration generation failure.

``7``
  Invalid command-line option.
