.. _Project_Kinds:

*************
Project Kinds
*************

Every GPR project file has a **kind** that determines what the project
produces, which attributes and packages are valid within it, and how other
projects may reference it.

The kind is expressed via a **qualifier** - one or two identifiers placed
immediately before the ``project`` keyword. The qualifier may be omitted, in
which case the kind is inferred from the attributes declared inside the
project:

- A project that declares both ``Library_Name`` and ``Library_Dir`` is
  treated as a library project.
- A project that declares ``for Source_Dirs use ();``, ``for Source_Files
  use ();``, or ``for Languages use ();`` is treated as an abstract project.
- Otherwise the project is treated as standard.

For non-standard project kinds, **using an explicit qualifier is best
practice**: it documents intent clearly, GPR tools validate that the
declared attributes are consistent with the stated kind, and the kind is
immediately visible to readers without having to inspect the attribute
declarations. Omitting the qualifier for a plain standard project is common
and acceptable.


Formal syntax
=============

.. code-block:: text

   simple_project_declaration ::=
     [ qualifier ] 'project' <project_>name 'is'
       { declarative_item }
     'end' <project_>name ;

   project_extension ::=
     [ qualifier ] 'project' <project_>name 'extends' [ 'all' ] path_name 'is'
       { declarative_item }
     'end' <project_>name ;

   qualifier ::= 'abstract'
               | 'library'
               | 'aggregate'
               | 'aggregate' 'library'
               | 'configuration'
               | 'standard'

The qualifier is optional. When present, it must match the kind inferred from
the project's attributes. Omitting it for a plain standard project is common;
for all other kinds, stating the qualifier explicitly is recommended.


.. _Standard_Project:

.. index:: standard project

Standard Project
================

**Qualifier:** ``standard`` (or omitted)

A standard project contains source files and produces executables or object
files. It is the default project kind.

.. code-block:: gpr

   project My_App is
      for Source_Dirs use ("src");
      for Main use ("main.adb");
   end My_App;

**Key properties:**

- May declare any attribute or package.
- May import other projects via ``with`` clauses.
- May extend another project, either directly (``extends``) or across an
  entire project hierarchy (``extends all``). See :ref:`RM_Project_Extension`.
- May be extended by other projects.


.. _Abstract_Project_Kind:

.. index:: abstract project

Abstract Project
================

**Qualifier:** ``abstract``

An abstract project has no source files. It is used to share attributes,
packages, and typed variable definitions across multiple concrete projects.

.. code-block:: gpr

   abstract project Common_Switches is
      package Compiler is
         for Switches ("Ada") use ("-O2", "-gnat2022");
      end Compiler;
   end Common_Switches;

**Constraints:**

- Either the ``abstract`` qualifier is present, or - for backward
  compatibility - at least one of ``Source_Dirs``, ``Source_Files``, or
  ``Languages`` must be declared empty. Prefer the explicit qualifier in new
  project files.
- If it extends another project, the base project must also be abstract.
- May be imported by any other project kind, including aggregate and aggregate
  library projects.


.. _Library_Project_Kind:

.. index:: library project, Library_Name, Library_Dir, Library_Kind

Library Project
===============

**Qualifier:** ``library``

A library project builds a library - a static archive or a dynamic/shared
image - rather than executables. It is the primary mechanism for producing
reusable compiled components.

.. code-block:: gpr

   library project My_Lib is
      for Source_Dirs   use ("src");
      for Languages     use ("Ada");
      for Library_Name  use "mylib";
      for Library_Dir   use "lib";
      for Library_Kind  use "static";
   end My_Lib;

.. note::

   For backward compatibility, a project without the ``library`` qualifier is
   implicitly treated as a library project when it declares both
   ``Library_Name`` and ``Library_Dir``. Prefer the explicit qualifier in new
   project files.

**Required attributes:**

``Library_Name``
  Name of the library to build (e.g. ``"mylib"`` produces
  ``libmylib.a`` or ``libmylib.so``).

``Library_Dir``
  Directory where the built library is placed. Must differ from the
  object directory (``Object_Dir``).

**Library kinds**

Controlled by the ``Library_Kind`` attribute:

``"static"``
  Archive of object files (default). Linked directly into the final
  executable.

``"static-pic"``
  Archive built with position-independent code, suitable for later
  inclusion in a shared library.

``"dynamic"`` / ``"relocatable"``
  Shared library loaded at program start. Changes to the library
  implementation require no relink of the final executable.

By default, dynamic library projects may only import other dynamic library
projects; importing non-library or static library projects requires setting
``Shared_Library_Prefix`` and related configuration attributes.

.. _Library_Version:

.. index:: Library_Version, shared library versioning

**Shared library versioning**

On Linux, the ``Library_Version`` attribute controls shared library versioning:

.. code-block:: gpr

   library project My_Lib is
      for Library_Name    use "mylib";
      for Library_Dir     use "lib";
      for Library_Kind    use "dynamic";
      for Library_Version use "libmylib.so.1.2.3";
   end My_Lib;

The value is the full versioned file name of the library. The build tool
produces a symlink for every prefix of the version string:

.. code-block:: text

   lib/
     libmylib.so.1.2.3   -- the actual shared object (SONAME: libmylib.so.1)
     libmylib.so.1.2      -- symlink -> libmylib.so.1.2.3
     libmylib.so.1        -- symlink -> libmylib.so.1.2
     libmylib.so          -- symlink -> libmylib.so.1

The SONAME embedded in the library is the major-version form of
``Library_Version`` (e.g. ``libmylib.so.1`` from ``libmylib.so.1.2.3``).

``Library_Version`` has no effect on ``static`` or ``static-pic`` libraries.


.. index:: stand-alone library, Library_Interface, Library_Standalone

Stand-alone Library Projects
----------------------------

A **stand-alone library** (SAL) bundles its own elaboration code so that the
final program does not need to drive elaboration of each unit individually.
Depending on the platform and configuration, elaboration may be triggered
automatically at load time or may require an explicit call from the main
subprogram (see `Auto-initialization`_ below). A library becomes stand-alone
when at least one of the following attributes is set:

- ``Library_Interface`` - list of Ada unit names that form the public
  interface of the library.
- ``Interfaces`` - list of source file names (language-agnostic alternative
  to ``Library_Interface``).

Units listed in the interface are visible to importers; other units in the
library are implementation details and may not be depended upon directly.

The ``Library_Standalone`` attribute refines the elaboration model:

``"standard"``
  Default SAL: the library provides an elaboration entry point that the
  program's elaboration must call.

``"encapsulated"``
  Encapsulated SAL: the library is fully self-contained; all its
  dependencies are either encapsulated SALs themselves or are part of
  the language runtime. No external elaboration call is required.

``"no"``
  Not stand-alone (default when no interface is declared).

.. _Auto-initialization:

.. index:: auto-initialization

**Auto-initialization**

Whether elaboration is called automatically depends on the configuration.
When ``Library_Auto_Init_Supported`` is ``"true"`` in the active configuration
project, automatic initialization is available. The library project may then
set ``Library_Auto_Init`` to ``"true"`` (to opt in) or ``"false"`` (to opt
out); when not set, it defaults to ``Library_Auto_Init_Supported``.

Setting ``Library_Auto_Init`` to ``"true"`` when ``Library_Auto_Init_Supported``
is ``"false"`` triggers a warning at load time.


.. _Aggregate_Project_Kind:

.. index:: aggregate project

Aggregate Project
=================

**Qualifier:** ``aggregate``

An aggregate project groups a coherent set of related project files so that
they can be built with a single GPRbuild invocation. It does not contain
sources itself; all sources come from the constituent projects listed in
``Project_Files``. It may also set default values for external variables seen
by all constituent projects via the ``External`` attribute.

Each directly aggregated project becomes the root of its own independent
*namespace*. Ada requires compilation-unit names to be unique within a
namespace, so duplicate unit names across different namespaces are permitted
and do not cause conflicts. Components shared by multiple namespaces are
deduplicated and built only once.

.. code-block:: gpr

   aggregate project All is
      for Project_Files use ("subsystem_a/a.gpr",
                             "subsystem_b/b.gpr",
                             "subsystem_c/c.gpr");
   end All;

**Required attribute:** ``Project_Files`` - a list of paths to constituent
``.gpr`` files. Paths are relative to the aggregate project file's directory.
Glob patterns (``*`` and ``**``) are supported. Constituent projects may
themselves be aggregate projects.

**Permitted packages:** ``Builder`` only (``Switches`` and
``Global_Compilation_Switches`` sub-attributes).

**Permitted project-level attributes:**

``Project_Files``
  Constituent project files.

``Project_Path``
  Extra directories searched when resolving ``with`` clauses inside
  constituent projects.

``External``
  Replaces the default value of an external variable as seen by all
  constituent projects. An explicit ``-X`` switch on the command line
  takes precedence. Only meaningful when this project is the root.

``Object_Dir``
  Directory where GPR-aware tools may store per-project data, such as
  the file index used to speed up reloading of a previously built
  project. Only meaningful when this project is the root.

**Import and use restrictions:**

- May only ``with`` abstract or other aggregate projects.
- Cannot be imported by non-aggregate projects; it is valid only as a root
  project or as a constituent (directly or transitively) of another aggregate
  project.
- Cannot be extended.
- When used as a constituent of another aggregate, only its ``Project_Files``
  list is taken into account: ``External`` and ``Object_Dir`` are ignored,
  and the aggregated sub-projects are folded directly into the root
  aggregate's namespace structure.


.. _Aggregate_Library_Project_Kind:

.. index:: aggregate library project

Aggregate Library Project
=========================

**Qualifier:** ``aggregate library``

An aggregate library project builds a single library by collecting the object
files produced by a set of constituent projects. It combines the properties of
a library project and an aggregate project: it has ``Library_Name`` and
``Library_Dir``, and it lists its constituents via ``Project_Files``.

.. warning::

   Despite sharing the ``aggregate`` qualifier and the ``Project_Files``
   attribute with aggregate projects, aggregate library projects are a
   fundamentally different construct. An aggregate library project is an
   ordinary library project that collects object files from its constituents
   into a single library: it can be ``with``-ed by any other project and may
   appear anywhere in the dependency graph. It does not support the
   ``External`` attribute, and constituent subtrees are not isolated - Ada
   unit names must be unique across all constituents.

.. code-block:: gpr

   aggregate library project Full_Lib is
      for Project_Files use ("module_a/a.gpr",
                             "module_b/b.gpr");
      for Library_Name use "full";
      for Library_Dir  use "lib";
   end Full_Lib;

The resulting library may be of any kind (``static``, ``static-pic``,
``dynamic``), independently of the library kinds of the constituent projects.

An aggregate library project is designed to incorporate its constituent
projects without forcing them to be rebuilt. To enforce this, the ``Compiler``
package cannot be declared in an aggregate library project: constituent objects
are used in the form already produced by each constituent project.

Declaring ``Object_Dir`` is strongly recommended. It serves two purposes:
GPR-aware tools may store per-project metadata there, and when the aggregate
library's kind differs from its constituents' - for example, building a dynamic
library from static constituent projects - the toolchain recompiles affected
sources with position-independent code (``-fPIC``) and stores the results in
``Object_Dir``, keeping constituent directories untouched. If not declared,
the project directory is used as default.

**Required attributes:** ``Library_Name``, ``Library_Dir``,
``Project_Files``.

**Stand-alone aggregate library projects:** ``Library_Interface`` or
``Interfaces`` may be declared to make the aggregate library stand-alone,
including the encapsulated variant (``Library_Standalone => "encapsulated"``).

**Library compatibility:** As with any library project, the standard
compatibility rules apply: a dynamic aggregate library cannot import
non-library or static library projects. It may also extend an abstract
project.


.. _Configuration_Project_Kind:

.. index:: configuration project

Configuration Project
=====================

**Qualifier:** ``configuration``

A configuration project describes the tools available for a given target and
runtime - compilers, linkers, and so on - as well as the switches required to
invoke each of them. It is used internally by all GPR tools (GPRbuild,
GPRclean, GPRinstall, GPRls, etc.) and by any tool built on top of the GPR2
library. Any such tool can generate a configuration project automatically;
GPRconfig is also available as a standalone tool to produce one independently.
The file extension is ``.cgpr``.

Configuration projects are part of the project tree in a special role: they
supply the toolchain attributes that all other projects in the tree rely on.
They can be loaded separately from the user project tree - via ``--config`` or
``--autoconf`` - and are never referenced by ``with`` clauses in regular
project files.

For the format and contents of configuration projects, see
:ref:`RM_GPRconfig`.
