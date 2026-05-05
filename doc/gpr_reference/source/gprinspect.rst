.. _RM_GPRinspect:

.. index:: GPRinspect

*********************
GPRinspect Reference
*********************

GPRinspect loads a GPR project tree and displays its structure and contents:
project relationships, source directories, attributes, packages, variables,
and type definitions. It is primarily a diagnostic tool for understanding how
a project tree is interpreted by the GPR project model.


Command Line
============

Syntax
------

::

   gprinspect [-P<proj>.gpr] [switches]

Project file and common switches
----------------------------------

See :ref:`RM_Common_Options` for project file discovery rules, project and
configuration switches, and common diagnostic switches.


Switches
========

.. index:: pair: GPRinspect; output format, pair: switch; --display

Output format
-------------

``--display=``\ *format*
  Select the output format. Accepted values:

  ``textual`` (default)
    Human-readable indented text.

  ``json``
    Pretty-printed JSON.

  ``json-compact``
    Compact JSON with no extra whitespace. Suitable for machine
    processing.


Project scope
-------------

``-r`` / ``--recursive``
  Display all projects in the tree, not only the root project.

``--views=``\ *name*\ [,\ *name*\ …]
  Restrict display to the named project views (comma-separated). Implies
  ``-r``. Only available with ``--display=textual``.


.. index:: pair: GPRinspect; content selection, pair: switch; --all, pair: switch; --attributes

Content selection
-----------------

With no content switch, GPRinspect shows only the structural information for
each project (directories, relationships, library properties).

``--all``
  Display everything: attributes, packages, variables, and type definitions.

``--attributes``
  Display project-level and package-level attributes and their values.

``-c`` / ``--from-config``
  Include attributes inherited from the active configuration project in the
  attribute display. Requires ``--attributes`` or ``--all``.

``--packages``
  Display the packages declared in each project and their attributes.

``--variables``
  Display variables and type definitions declared in each project.


Registry
--------

``--gpr-registry-file=``\ *file*
  Load additional attribute definitions from *file* before loading the
  project tree. Use this to recognise attributes defined by external tools
  (such as GNATcheck or GNATprove) when inspecting projects that use them.


.. index:: pair: GPRinspect; output structure

Output structure
================

Textual output is divided into sections:

**Header**
  Timestamp and GPR2 library version.

**Project tree**
  Summary of the loaded tree: project count, search paths.

**Per-project blocks**
  One block per project in scope. Each block reports:

  - Project name, kind, and file path.
  - Object, source, library, and ALI directories.
  - ``extends`` and ``with`` relationships.
  - Attributes (with ``--attributes`` or ``--all``), grouped by package.
  - Variables and type definitions (with ``--variables`` or ``--all``).

**Messages**
  Any errors, warnings, or hints produced during loading.

JSON output wraps the same information under ``"projects"`` and
``"messages"`` keys.


Exit Codes
==========

``0``
  Success.

``1``
  General error (invalid option, project loading failure, etc.).

``5``
  Project parsing error.
