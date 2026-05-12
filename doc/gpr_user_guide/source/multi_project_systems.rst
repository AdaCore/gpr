.. index:: project tree, project dependency

.. _Multi_Project_Systems:

********************************
Organizing Multi-Project Systems
********************************

Real-world software is rarely a single component. This chapter explains how
to split a system into multiple project files, express dependencies between
them, and control what is visible across project boundaries.


Why multiple projects?
======================

A single project file works well for a self-contained component. As a system
grows, splitting it into multiple projects brings several benefits:

- **Incremental builds**: only the components that changed are recompiled.
- **Reuse**: a library project can be imported by many applications without
  copying sources.
- **Encapsulation**: each project controls which of its sources are visible
  to importers (see :ref:`Libraries`).
- **Separate build settings**: each project can have its own compiler
  switches, object directory, and languages.


.. index:: with clause

Importing a project with ``with``
=================================

A project declares its dependencies at the top of the file using ``with``
clauses, one per imported project:

.. code-block:: gpr

   with "libs/utils/utils.gpr";
   with "libs/crypto/crypto.gpr";

   project My_App is
      for Source_Dirs use ("src");
      for Object_Dir  use "obj";
      for Exec_Dir    use ".";
      for Main        use ("main.adb");
   end My_App;

The path in a ``with`` clause is relative to the importing project file.
GPRbuild loads all projects in the tree, then builds each source in the
order required by the actual source dependency graph.

GPR also searches for project files in the directories listed in the
``GPR_PROJECT_PATH`` environment variable, so installed libraries can be
imported by name without a path:

.. code-block:: gpr

   with "gnatcoll.gpr";

Accessing attributes and variables of a withed project
------------------------------------------------------

After importing a project, you can read its attributes and variables using
the *Project*\ ``.``\ *Attribute* notation. This is commonly used to reuse
settings declared in a shared project:

.. code-block:: gpr

   with "common_settings.gpr";

   project My_App is
      --  Read a variable defined in Common_Settings
      Build_Mode : Common_Settings.Build_Mode_Type :=
         Common_Settings.Build_Mode;

      --  Reuse the Compiler package verbatim
      package Compiler renames Common_Settings.Compiler;

      --  Append to the imported switches
      package Linker is
         for Switches ("Ada") use
            Common_Settings.Linker'Switches ("Ada") & ("-lm");
      end Linker;
   end My_App;

The ``renames`` clause makes the package an alias of the original; any
change in ``Common_Settings.Compiler`` is automatically reflected.


Limited ``with``
================

A plain ``with`` gives the importing project full visibility over the withed
project's sources, attributes, variables, and packages. Circular plain
``with`` dependencies are not allowed: if project A imports B and B imports
A, GPRbuild reports a load error.

When two projects need their sources to be mutually visible - for example,
two components that are mutually recursive at the Ada unit level - a
``limited with`` can be used:

.. code-block:: gpr

   limited with "component_b.gpr";

   project Component_A is
      for Source_Dirs use ("src_a");
      ...
   end Component_A;

With a ``limited with``, the sources of the withed project are visible for
compilation, but its attributes, variables, and packages are not accessible
from the importing project.

Use ``limited with`` sparingly. A need for it often indicates that the
components involved should be reorganized into a shared lower-level library
that both can depend on.


.. index:: abstract project

The project tree
================

The set of projects reachable from the root project through ``with`` and
``limited with`` clauses forms the *project tree*. Loading the tree resolves
all source directories, attributes, and naming conventions across all
projects. The actual compilation order is then determined by the source
dependency graph, independently of the project import structure.


Typical layout
==============

A common layout for a multi-project system:

.. code-block:: none

   myproject/
   ├── myproject.gpr          <- root project (executable)
   ├── src/
   ├── obj/
   ├── libs/
   │   ├── utils/
   │   │   ├── utils.gpr      <- library project
   │   │   ├── src/
   │   │   └── obj/
   │   └── crypto/
   │       ├── crypto.gpr     <- library project
   │       ├── src/
   │       └── obj/
   └── common_settings.gpr    <- abstract project (shared settings)

Each library has its own project file, source directory, and object
directory. The root project imports all libraries it needs.
