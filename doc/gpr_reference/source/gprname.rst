.. _RM_GPRname:

.. index:: GPRname

*****************
GPRname Reference
*****************

GPRname scans source directories for Ada and non-Ada source files, identifies
the Ada units they contain, and generates a GPR project file with a ``Naming``
package that maps every discovered unit to its source file. It is the standard
tool for projects where source file names do not follow the default GNAT
naming conventions.


Command Line
============

Syntax
------

::

   gprname -P<proj>.gpr [switches] [patterns]

GPRname requires a project file name (``-P``). It does not use the common
project-loading infrastructure - it creates or updates the named file rather
than loading it. The common switches documented in :ref:`RM_Common_Options`
do not apply, with the exception of ``--target`` and ``--RTS``.

Patterns
--------

One or more filename patterns may be given on the command line. Each pattern
specifies the set of files that GPRname should scan for Ada units. Patterns
use glob syntax (``*``, ``?``, ``[...]``). If no pattern is given, GPRname
scans for nothing and produces an empty naming project.

Multiple independent sets of patterns with different source directories can
be defined using ``--and`` to separate them into sections (see below).


Switches
========

Project
-------

``-P`` *proj*
  Create or update *proj* as the main project file. Required.

``--no-backup``
  Do not create a numbered backup (``.saved_0``, ``.saved_1``, ...) of the
  existing project file before overwriting it.


Source directories
------------------

``-d`` *dir*
  Add *dir* as a source directory for the current section. May be repeated.
  Append ``/**`` to search *dir* recursively.

``-D`` *file*
  Read source directories from *file* (one directory per line).

``--minimal-dirs``
  After scanning, retain in ``Source_Dirs`` only the directories that
  contain at least one source file. Directories specified with ``/**`` are
  expanded to an explicit list of matching subdirectories.


.. index:: pair: GPRname; naming patterns, naming convention

Naming patterns
---------------

``-f`` *pattern*
  Add *pattern* as a C-language filename pattern for the current section.

``-f:``\ *lang* *pattern*
  Add *pattern* as a filename pattern for language *lang*.

``-x`` *pattern*
  Exclude files matching *pattern* from the Ada patterns of the current
  section.

``-xf`` *pattern*
  Exclude files matching *pattern* from the C patterns of the current
  section.

``-xf:``\ *lang* *pattern*
  Exclude files matching *pattern* from the *lang* patterns of the current
  section.

``--and``
  Begin a new section. Each section has its own set of source directories
  and patterns. Use this when different subtrees of a project have different
  naming conventions.


Preprocessing
-------------

``-gnateD``\ *sym*\ ``=``\ *val*
  Define preprocessor symbol *sym* to *val* when parsing Ada sources.

``-gnatep=``\ *file*
  Use *file* as a preprocessor data file when parsing Ada sources.


Ada source parser
-----------------

``--libadalang-parser``
  Use Libadalang to parse Ada source files (default).

``--gcc-parser``
  Use the GCC Ada compiler to parse Ada source files.


Other
-----

``--target=``\ *name*
  Target for the Ada compiler used to parse sources.

``--RTS=``\ *dir*
  Runtime for the Ada compiler used to parse sources.

``--ignore-duplicate-files``
  Silently skip a file if its basename has already been seen. Without this
  switch, a duplicate basename produces a warning.

``--ignore-predefined-units``
  Do not record predefined Ada units (``Ada``, ``System``, ``Interfaces``,
  etc.) in the naming project.

``-v``
  Verbose output. Repeat for more detail.


.. index:: pair: GPRname; generated files

Generated files
===============

GPRname creates or updates three files named after the project (e.g. for
``-P my_proj.gpr``):

:file:`my_proj.gpr`
  The main project file. GPRname inserts or updates:

  - ``with "my_proj_naming.gpr";`` - import of the naming project.
  - ``for Languages use (...);`` - languages discovered.
  - ``for Source_Dirs use (...);`` - source directories scanned.
  - ``for Source_List_File use "my_proj_source_list.txt";``
  - ``package Naming renames My_Proj_Naming.Naming;``

:file:`my_proj_naming.gpr`
  A separate project containing only the ``Naming`` package, with one
  ``for Spec`` / ``for Body`` entry per discovered Ada unit, and
  ``for Implementation_Exceptions`` entries for non-Ada languages.

:file:`my_proj_source_list.txt`
  A plain-text list of all discovered source files, consumed by
  ``Source_List_File`` in the main project.

A numbered backup of any pre-existing project file is created before the
file is overwritten (unless ``--no-backup`` is given).


Exit Codes
==========

``0``
  Success.

``1``
  Error (invalid option, source file not found, etc.).
