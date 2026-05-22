.. _RM_Project_Extension:

.. index:: project extension, extends keyword

*****************
Project Extension
*****************

Most project kinds may extend another project using the ``extends`` clause.
The extending project inherits sources and settings from the base and may
selectively override them. An extending project can also import other projects
alongside its base.

Aggregate projects and aggregate library projects may extend abstract projects
to inherit their attributes and package settings.

.. code-block:: gpr

   project Patched extends "original/original.gpr" is
      --  Source files here shadow those in original.gpr.
      --  All other sources and settings are inherited.
   end Patched;


Simple Extension
================

What is inherited
-----------------

- **Source files** - All source files from the base are implicitly present.
  A source file in the extending project whose basename matches one from the
  base shadows the base version. See :ref:`Source_Resolution` for the full
  shadowing rules.

- **Attribute values** - Project-level attributes not declared in the
  extending project are inherited from the base.

- **Packages** - Any package not declared in the extending project is
  inherited in full from the base. A package that *is* declared in the
  extending project replaces the entire base package; there is no
  per-attribute inheritance within a package. Use package extension
  (``package Compiler extends Base.Compiler is ...``) to inherit a base
  package and add or override individual attributes.

**Exception:** ``Linker'Linker_Options`` is never inherited.


.. index:: source shadowing in extension

Excluding inherited sources
---------------------------

To remove an inherited source without providing a replacement, list its
file name in ``Excluded_Source_Files``:

.. code-block:: gpr

   project Work extends "../base/base.gpr" is
      for Source_Files use ("pack.ads");
      --  New spec does not require a body
      for Excluded_Source_Files use ("pack.adb");
   end Work;

``Excluded_Source_List_File`` accepts the same information as a path to a
text file with one file name per line.


Extension Hierarchies
=====================

Extension may be chained: an extending project may itself be extended
further. The ``extends`` relationship forms a linear chain (each project
has at most one direct base). A project that extends another may also import
additional projects normally.


Import redirection
------------------

When loading a project tree, the build system automatically redirects imports
to their extending versions. If:

- project *A* extends *B* and imports *C*, and
- *C* extends *D*, and
- *B* imports *D*,

then in *A*'s context, *B*'s import of *D* is transparently replaced by an
import of *C*. Any reference to *D* anywhere in the base hierarchy is
similarly redirected, ensuring that the entire tree consistently uses *C*
instead of *D*.

This redirection applies transitively: if the base hierarchy has further
imports of *D* through intermediate projects, those are all replaced by *C*
as well.


.. index:: extends all

``extends all``
===============

When working with a large project hierarchy, replacing sources in a single
constituent project normally requires extending every project along the import
path up to the root (so that import redirection keeps the tree consistent).
``extends all`` eliminates this overhead: it implicitly extends every project
in the import closure of the named base, with import redirection applied
throughout, so the whole subtree is immediately available for targeted
amendment.

.. code-block:: gpr

   project Full_Override extends all "original/original.gpr" is
   end Full_Override;

To replace a specific constituent - for example to substitute instrumented
sources for a particular library - import an explicit extending project for
that constituent from within the ``extends all`` project. The explicit
extension replaces the corresponding implicit one, and import redirection
ensures all other projects in the closure consistently reference the new
version instead of the original.


Restriction
===========

A project may not import, directly or indirectly, both an extending project
*P* and any project that *P* extends (directly or indirectly). Without this
restriction, two import paths could reach different versions of the same
source file.

The standard solution is to introduce an empty intermediate extension so
that all import paths go through the extending project:

.. code-block:: gpr

   --  b_ext.gpr - bridges the gap so no path reaches the original b.gpr
   with "a_ext.gpr";
   project B_Ext extends "b.gpr" is
   end B_Ext;
