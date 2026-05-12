.. _RM_Project_Tree:

************
Project Tree
************

A GPR project file rarely exists in isolation. A system is typically split
across several project files, each responsible for one component. This chapter
describes how multiple project files are connected into a project tree.


.. index:: project tree, root project

Project tree structure
======================

A GPR project file rarely exists in isolation. A system is typically split
across several project files, each responsible for one component. The
connected graph of project files forms the **project tree**.

The project tree is a directed acyclic graph (DAG) rooted at the **root
project** - the project passed directly to the build tool (for example
``gprbuild -P root.gpr``). Two kinds of edges connect projects in the tree:

**Import** (``with`` clause)
  The importing project depends on the imported project for compilation
  and for attribute visibility.

**Extension** (``extends`` clause)
  The extending project inherits sources and attributes from a base project.
  See :ref:`RM_Project_Extension`.

Aggregation (``Project_Files``) is a third relationship specific to aggregate
and aggregate library projects; see :ref:`Aggregate_Project_Kind`.

Every project in the tree is loaded once and shared, regardless of how many
other projects import it.


.. index:: with clause

The ``with`` clause
===================

A project declares its imports at the top of the file, before the project
declaration, using ``with`` clauses:

.. code-block:: gpr

   with "libs/utils/utils.gpr";
   with "libs/crypto/crypto.gpr";

   project My_App is
      for Source_Dirs use ("src");
      for Main        use ("main.adb");
   end My_App;

Multiple paths may appear in a single clause, separated by commas:

.. code-block:: gpr

   with "utils.gpr", "crypto.gpr";

A given project may appear at most once across all ``with`` clauses of the
same importing project. Duplicates are an error.


What a plain ``with`` makes visible
-----------------------------------

From within the body of the importing project, a plain ``with`` exposes all
of the following from the imported project:

**Types and variables**
  Typed string type definitions and variable declarations are accessible via
  the ``Imported_Project.Name`` notation.

**Attributes and packages**
  Attribute values may be read using ``Imported_Project'Attribute`` (or
  ``Imported_Project.Package'Attribute``). Packages may be renamed into the
  importing project with a ``package P renames Imported_Project.P``
  declaration.

**Source files**
  Sources of the imported project are compiled as needed; the resulting
  object files and Ada Library Information files (``*.ali``) are made
  available to the importing project's sources.

Example - sharing settings from an abstract project:

.. code-block:: gpr

   with "common.gpr";

   project My_App is
      --  Read a typed variable from Common
      Mode : Common.Build_Mode_Type := Common.Build_Mode;

      --  Alias the whole Compiler package from Common
      package Compiler renames Common.Compiler;

      --  Extend the linker switches defined in Common
      package Linker is
         for Switches ("Ada") use
            Common.Linker'Switches ("Ada") & ("-lm");
      end Linker;
   end My_App;


.. index:: path resolution, GPR_PROJECT_PATH, ADA_PROJECT_PATH

Path resolution
---------------

The path in a ``with`` clause is a string naming a ``.gpr`` file. The
``.gpr`` extension is optional and appended automatically if absent. The
directory separator is always ``/``, even on Windows.

Paths are resolved in the following order:

1. If the path is absolute, it is used directly.
2. Otherwise it is resolved relative to the directory of the importing project
   file.
3. If no file is found there, each directory listed in
   :envvar:`GPR_PROJECT_PATH` is searched in order.
4. Then the directories in :envvar:`ADA_PROJECT_PATH` are searched.
5. Finally the default project paths provided by the selected toolchain(s)
   are searched.

The first match terminates the search. Installed libraries typically rely on
steps 3-5, so they can be imported by bare name without a path:

.. code-block:: gpr

   with "gnatcoll.gpr";   -- resolved via GPR_PROJECT_PATH or toolchain paths


.. index:: limited with clause

The ``limited with`` clause
===========================

Because attribute evaluation must be acyclic, a cycle formed entirely from
plain ``with`` edges is an error. The ``limited with`` variant relaxes this
constraint by restricting what is visible across the import:

.. code-block:: gpr

   limited with "component_b.gpr";

   project Component_A is
      for Source_Dirs use ("src_a");
   end Component_A;

With a ``limited with``:

**Visible**
  Source files of the imported project are compiled as part of the tree;
  their object files and ALI files are available to the importing project's
  sources.

**Not visible**
  The imported project's types, variables, attributes, and packages cannot
  be referenced inside the importing project's body.

The ``limited`` modifier applies to the *attribute-visibility* graph only; it
has no effect on the *source-dependency* graph. Two components whose Ada units
are mutually recursive can therefore coexist in the same tree:

.. code-block:: gpr

   --  component_a.gpr
   limited with "component_b.gpr";
   project Component_A is
      for Source_Dirs use ("src_a");
   end Component_A;

   --  component_b.gpr
   with "component_a.gpr";
   project Component_B is
      for Source_Dirs use ("src_b");
   end Component_B;

Here ``Component_B`` may reference attributes of ``Component_A``, but
``Component_A`` may not reference attributes of ``Component_B``.

A cycle requires at least one ``limited with`` edge to break the attribute
cycle; without one the project loader reports an error.

.. note::

   A need for ``limited with`` sometimes signals that the two components
   share a common abstraction that should be factored into a third project on
   which both depend. Consider restructuring before reaching for
   ``limited with``.
