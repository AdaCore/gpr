.. _Source_Resolution:

.. index:: source resolution

*****************
Source Resolution
*****************

This chapter describes how the GPR project model resolves source files: how
sources are discovered, which basenames are valid, how conflicts are handled,
and how project extension overrides inherited sources.


Source discovery
================

For each non-abstract, non-aggregate project view the build system scans
every directory listed in ``Source_Dirs`` in declaration order. The resulting
candidate set is then filtered by:

* ``Source_Files`` / ``Source_List_File`` — restrict to an explicit list.
* ``Excluded_Source_Files`` / ``Excluded_Source_List_File`` — remove specific
  files.
* The ``Naming`` package — controls which file name extensions and patterns
  identify sources of each language.

Sources inherited from an extended project are added to this set before
filtering.


.. index:: basename, naming convention

Basename uniqueness
===================

Every source file is identified by its *basename* — the simple file name
without directory components. When the same basename appears in more than one
source directory, the outcome depends on how those directories entered the
project:

* **Different values in ``Source_Dirs``** — the directory corresponding to the
  earlier value takes precedence; no error is reported. For example, if
  ``Source_Dirs`` is ``("src/a", "src/b")`` and both contain ``util.adb``,
  the file from ``src/a`` is used.

* **Same value in ``Source_Dirs``** — an error is reported. This can occur
  when a single value is a recursive pattern such as ``"src/**"``: if two
  subdirectories matched by that pattern both contain a file with the same
  basename, the conflict cannot be resolved by ordering and is flagged as an
  error.


.. index:: source shadowing

Extending projects and source shadowing
========================================

When project *B* extends project *A*, *B* inherits all of *A*'s sources. If
*B* also declares a source whose basename matches one inherited from *A*,
*B*'s version shadows the inherited one — the extending project's source
always takes precedence. This is not treated as a basename clash: overloading
an inherited source is the intended mechanism for selectively replacing
individual source files in an extension without copying the entire project.

To remove an inherited source without providing a replacement, list it in
``Excluded_Source_Files`` or ``Excluded_Source_List_File``.


``extends all``
===============

With ``extends all``, the extending project inherits sources from the full
transitive import closure of the extended project, not only from the immediate
extended project. The same shadowing rules apply: a source declared in the
extending project overrides the corresponding inherited source from any project
in the closure.

See :ref:`RM_Project_Extension` for a full description of ``extends all``
semantics.


Multi-unit Ada sources
======================

A single Ada source file may contain more than one compilation unit (the
``separate`` construct, or an explicit ``Naming`` mapping via
``Spec`` / ``Body``). The project model tracks sources at the basename level;
unit-to-file mappings are resolved during source analysis. Basename uniqueness
rules still apply at the file level regardless of how many units a file
contains.
