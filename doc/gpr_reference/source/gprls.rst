.. _RM_GPRls:

.. index:: GPRls

****************
GPRls Reference
****************

GPRls lists the sources, units, objects, and dependencies of a GPR project
tree. It reads the build database produced by GPRbuild to report the
up-to-date status of each artifact.


Command Line
============

Syntax
------

::

   gprls [-P<proj>.gpr] [switches] [files]

Project file and common switches
----------------------------------

See :ref:`RM_Common_Options` for project file discovery rules, project and
configuration switches, and common diagnostic switches.

Files
-----

Zero or more source file names may be given on the command line to restrict
output to those files only. Simple names (without directory) are matched
against source basenames; full paths are also accepted. If no files are
given, GPRls lists all sources of the project (or of the entire project tree
with ``-U``).


Switches
========

.. index:: pair: GPRls; output selection, pair: switch; --closure

Output selection
----------------

When none of ``-u``, ``-s``, or ``-o`` is given, GPRls prints all three
categories for each compilation unit.

``-u``
  Print the unit name for each Ada compilation unit.

``-s``
  Print the source file for each compilation unit.

``-o``
  Print the object file for each compilation unit.

``-d``
  For each source file, also list the source files it depends on, together
  with their status. Superseded by ``--closure``.

``--closure``
  Compute the transitive compilation closure of the named files (or of the
  project's main units if no files are given) and list every source file
  required to compile them.

``-U``
  List sources from the entire project tree, not only the root project.

``-a``
  Include predefined (runtime) units in the output and in dependency lists.

``-a0``
  Same as ``-a``, but print only the simple file name for runtime sources,
  hiding the runtime directory path.

``--hide-status``
  Suppress the ``OK``/``DIF`` status indicator from the output.

``--source-parser``
  Derive dependency information by parsing Ada source files directly rather
  than reading ALI files. Useful when ALI files are absent or stale.

``-files=``\ *file*
  Read the list of source files to process from *file* (one name per line,
  blank lines ignored). The resulting list is merged with any files given on
  the command line.


.. index:: pair: GPRls; status indicators

Status indicators
=================

Unless ``--hide-status`` is given, each artifact is prefixed with a status
tag:

``OK``
  The artifact is up to date: its timestamp matches the build database.

``DIF``
  The artifact has been modified since the last build.


Output layout
=============

Entries are grouped by compilation action. Within each group, the default
indentation is:

- **Objects** - no indentation.
- **Unit names and sources** - indented by 3 spaces.
- **Dependencies** (with ``-d``) - indented by 3 spaces.

Example output (all three categories, with status):

.. code-block:: text

   OK  obj/pack.o
      pack
      OK  src/pack.ads
      OK  src/pack.adb


Exit Codes
==========

``0``
  Success.

``1``
  General error (invalid option, project error, etc.).
