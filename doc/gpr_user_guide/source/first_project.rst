.. index:: first project, GPRbuild

.. _First_Project:

******************
Your First Project
******************

This chapter walks you through creating a minimal GPR project file, building
it with GPRbuild, and understanding what each part of the project file means.
By the end you will have a working project structure that you can use as a
starting point for your own work.


A minimal project
=================

Create a directory for your project and add a project file:

.. code-block:: none

   hello/
   ├── hello.gpr
   ├── src/
   │   └── hello.adb
   └── obj/        <- created automatically on first build

The project file ``hello.gpr``:

.. code-block:: gpr

   project Hello is
      for Source_Dirs use ("src");
      for Object_Dir   use "obj";
      for Exec_Dir     use ".";
      for Main         use ("hello.adb");
   end Hello;

And a minimal source file ``src/hello.adb``:

.. code-block:: ada

   with Ada.Text_IO;
   procedure Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello, world!");
   end Hello;

To build and run:

.. code-block:: shell

   $ gprbuild -P hello.gpr
   $ ./hello
   Hello, world!

That is the complete workflow. The sections below explain each part of the
project file.


Project declaration
===================

.. code-block:: gpr

   project Hello is
      ...
   end Hello;

A project file begins with ``project`` *Name* ``is`` and ends with ``end``
*Name* ``;``. The name must match the file name (case-insensitively):
``Hello`` lives in ``hello.gpr``.

The ``project`` keyword alone produces a *standard* project - the default
kind, which can hold sources and produce executables or object files. Other
kinds (``library``, ``abstract``, ``aggregate``) are covered in later
chapters.


.. index:: Source_Dirs

Source directories
==================

.. code-block:: gpr

   for Source_Dirs use ("src");

``Source_Dirs`` is a list of directories, relative to the project file,
where GPR tools search for source files. You can list multiple directories:

.. code-block:: gpr

   for Source_Dirs use ("src", "gen/src", "platform/src");

Glob patterns are also accepted:

.. code-block:: gpr

   for Source_Dirs use ("src/**");   --  recursive

If ``Source_Dirs`` is omitted, the directory containing the project file
itself is used.

.. tip::

   Keep source directories distinct across projects in the same project tree.
   Two projects sharing a directory is a common source of hard-to-diagnose
   duplicate-unit errors.


Languages
=========

GPR is multi-language. By default, only Ada sources are recognised. To add
other languages, list them explicitly:

.. code-block:: gpr

   for Languages use ("Ada", "C");

GPRbuild compiles only the languages declared here; sources in other
languages are ignored.

To build a project that contains only C sources, set ``Languages`` to
``("C")``:

.. code-block:: gpr

   for Languages use ("C");


.. index:: Object_Dir

Object and executable directories
==================================

.. code-block:: gpr

   for Object_Dir use "obj";
   for Exec_Dir   use ".";

``Object_Dir`` is where compiler-generated files (object files, ALI files,
dependency files) are placed. GPRbuild creates this directory automatically
before the first build, as long as the path is relative and located under
the project directory.

``Exec_Dir`` is where linked executables are placed. Using ``"."`` puts them
next to the project file, which is convenient for running them from the
project root.

Both attributes default to the project directory if omitted. Declaring them
explicitly is strongly recommended so that build products are kept separate
from source files.


.. index:: Main

Main subprograms
================

.. code-block:: gpr

   for Main use ("hello.adb");

``Main`` lists the source files that contain program entry points: Ada main
subprograms or C ``main()`` functions. GPRbuild compiles all sources in the
project tree and then links a separate executable for each entry point.

The value is a list, so you can declare multiple entry points:

.. code-block:: gpr

   for Main use ("hello.adb", "test_suite.adb");

Each produces its own executable. The executable name is derived from the
main source file name; it can be overridden with the ``Executable`` attribute
in the ``Builder`` package.


Compiler switches
=================

To specify compiler switches, declare a ``Compiler`` package:

.. code-block:: gpr

   package Compiler is
      for Switches ("Ada") use ("-gnat2022", "-O1", "-gnatwa");
      for Switches ("C")   use ("-O2", "-Wall");
   end Compiler;

The index selects which sources the switches apply to. For each source,
GPRbuild looks up ``Switches`` in this order and uses the **first match
only** - switches do not accumulate:

1. The source file name (exact match or glob pattern).
2. The source's language name.
3. ``others`` - the catch-all.

This makes it easy to apply special treatment to individual files while
keeping a common baseline for everything else:

.. code-block:: gpr

   package Compiler is
      for Switches ("Ada")             use ("-gnat2022", "-gnatwa");
      for Switches ("generated_*.adb") use ("-gnat2022");  --  no warnings
      for Switches ("big_table.adb")   use ("-gnat2022", "-O3");
      for Switches (others)            use ("-O2");        --  C, C++, ...
   end Compiler;

Here ``generated_*.adb`` and ``big_table.adb`` each get their own switch
set; all other Ada sources use the ``"Ada"`` entry; everything else falls
through to ``others``.

.. tip::

   During development, ``-gnatwa`` (all Ada warnings) and ``-gnatVa`` (all
   Ada validity checks) catch many bugs early. Use scenario variables
   (see :ref:`Scenarios`) to select different switch sets for debug and
   release builds.


.. index:: pair: GPRbuild; invocation

What GPRbuild does
==================

When you run ``gprbuild -P hello.gpr``, GPRbuild:

1. Loads ``hello.gpr`` and any projects it depends on (none here).
2. Determines which sources need (re-)compilation by comparing source
   content against the recorded build signatures.
3. Compiles each out-of-date source in parallel into ``Object_Dir``.
4. Links an executable for each ``Main`` source into ``Exec_Dir``.

Subsequent builds skip unchanged sources, so incremental builds are fast
even in large projects.


Next steps
==========

- :ref:`Managing_Sources` - source directory patterns, exclusions, and
  language-specific naming conventions.
- :ref:`Multi_Project_Systems` - splitting a system into multiple project
  files connected by ``with`` clauses.
- :ref:`Scenarios` - ``type`` declarations and ``case`` statements for
  debug/release builds and platform variants.
- :ref:`Libraries` - the ``library`` project kind.
