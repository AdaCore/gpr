.. index:: aggregate project

.. _Aggregate_Projects_UG:

******************
Aggregate Projects
******************

This chapter covers *aggregate projects* - a project kind that groups
independent project subtrees for a single build invocation. The related
*aggregate library project* kind, which produces a library artifact from
constituent projects, is covered in :ref:`Libraries`.

.. note::

   Despite the similar name and the shared ``aggregate`` qualifier, aggregate
   projects and aggregate library projects are distinct and behave very
   differently: an aggregate project produces no artifact and can only be used
   as a root, while an aggregate library project produces a library and can be
   imported like any other library project. Additionally, aggregate library
   projects cannot set default external variable values via the ``External``
   attribute.

An aggregate project groups several independent project subtrees so they can
all be built with a single GPRbuild invocation. Each entry in ``Project_Files``
is the root of one such subtree; the aggregate produces no build artifact of
its own. It can also set default values for external variables (see
:ref:`Scenarios`) that apply uniformly to all constituent subtrees via the
``External`` attribute.

.. note::

   An aggregate project can only be used as a root project or as a constituent
   of another aggregate project. It cannot be imported via a ``with`` clause by
   any standard or library project.


.. index:: Project_Files, External attribute, namespace

Basic syntax
============

.. code-block:: gpr

   aggregate project All is
      for Project_Files use ("subsystem_a/a.gpr",
                             "subsystem_b/b.gpr",
                             "subsystem_c/c.gpr");
      for External ("BUILD") use "release";
   end All;

``Project_Files`` is mandatory and lists the roots of constituent subtrees as
paths to ``.gpr`` files. Paths are relative to the aggregate project file, or
absolute; they are not searched via the project search path. ``External`` sets
a default value for an external variable visible to all constituent subtrees;
an explicit ``-X`` switch on the command line takes precedence and can still
override it.

Glob patterns are supported in ``Project_Files``:

.. code-block:: gpr

   aggregate project All is
      for Project_Files use ("**/*.gpr");  --  all subtree roots recursively
   end All;

The ``**`` pattern searches subdirectories recursively and may only appear at
the last position in the directory part of the path.

Constituent entries may themselves be aggregate projects, allowing nested
groupings.


Use cases
=========

Building all outputs from multiple subtrees
-------------------------------------------

A common situation is a set of independent subtrees - some producing
executables, some producing libraries - that need to be built together. Without
an aggregate project, each subtree root requires a separate GPRbuild
invocation. Grouping them in an aggregate builds everything in a single
parallel invocation:

.. code-block:: gpr

   aggregate project All is
      for Project_Files use ("apps/app_a/app_a.gpr",
                             "apps/app_b/app_b.gpr",
                             "libs/util/util.gpr");
   end All;

GPRbuild deduplicates any sources shared across subtrees and schedules all
compilations, binds, and links in parallel.


Setting the build environment
-----------------------------

The ``External`` and ``Project_Path`` attributes let an aggregate project fix
the build environment for all constituent subtrees, replacing a long sequence
of ``-X`` and ``-aP`` command-line switches:

.. code-block:: gpr

   aggregate project All is
      for Project_Files use ("a.gpr", "b.gpr");
      for Project_Path  use ("../shared/projects");
      for External ("BUILD") use "release";

      package Builder is
         for Global_Compilation_Switches ("Ada") use ("-g");
      end Builder;
   end All;

``Project_Path`` adds directories searched when resolving ``with`` clauses
inside constituent subtrees (it does not affect the resolution of
``Project_Files`` paths themselves).


Restrictions
============

- An aggregate project may only ``with`` **abstract** projects (to share
  attribute values). It cannot ``with`` standard or library projects.
- An aggregate project cannot be extended.
- Source-related attributes (``Source_Dirs``, ``Languages``, ``Main``, etc.)
  and compilation packages (``Compiler``, ``Binder``, ``Linker``, ``Naming``)
  are not permitted. Only the ``Builder`` package is allowed.
- ``Object_Dir`` is permitted. Although an aggregate project produces no build
  artifacts, ``Object_Dir`` provides a location for the build system to store
  incremental build data outside the source tree or any VCS-controlled
  directory.
