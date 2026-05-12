.. index:: executable

.. _Building_Executables:

********************
Building Executables
********************

This chapter covers how GPRbuild compiles sources and links executables:
declaring entry points, naming outputs, controlling linker and binder
switches, and common build options.


.. index:: Main

Entry points
============

An executable is produced for each source file listed in the ``Main``
attribute of the root project:

.. code-block:: gpr

   project Hello is
      for Source_Dirs use ("src");
      for Object_Dir  use "obj";
      for Exec_Dir    use ".";
      for Main        use ("hello.adb", "gen.adb");
   end Hello;

``Main`` takes simple file names - the base name only, no directory
component. GPRbuild searches for each main among the sources of the project
tree. It compiles the transitive closure of sources required by each entry
point and links one executable per entry point, placed in ``Exec_Dir``.
Sources not reachable from any entry point are not compiled.

To build only a subset of the declared mains, name them on the command line:

.. code-block:: shell

   $ gprbuild -P hello.gpr hello.adb


.. index:: Exec_Dir, pair: Builder; package

Executable names
================

By default the executable name is derived from the main source file name:
``hello.adb`` produces ``hello`` (complemented by platform-specific extension,
such as ``hello.exe`` on Windows).

To override the name permanently, use the ``Executable`` attribute in the
``Builder`` package:

.. code-block:: gpr

   package Builder is
      for Executable ("hello.adb") use "greet";
      for Executable ("gen.adb")   use "codegen";
   end Builder;

When building a single main, the output name can also be overridden on the
command line with ``-o``:

.. code-block:: shell

   $ gprbuild -P hello.gpr hello.adb -o test.exe

``-o`` is only valid when exactly one main is being built.


.. index:: pair: Compiler; package

Compiler switches
=================

Compiler switches are declared in the ``Compiler`` package, indexed by
language name or source file name:

.. code-block:: gpr

   package Compiler is
      for Switches ("Ada") use ("-gnat2022", "-O1", "-gnatwa");
      for Switches ("C")   use ("-O2", "-Wall");
   end Compiler;

For each source, GPRbuild looks up ``Switches`` in this order and uses
the **first match only** - switches do not accumulate:

1. The source file name (exact match or glob pattern).
2. The source's language name.
3. ``others`` - the catch-all.

This makes it straightforward to apply special treatment to individual
files while keeping a common baseline for everything else:

.. code-block:: gpr

   package Compiler is
      for Switches ("Ada")             use ("-gnat2022", "-gnatwa");
      for Switches ("generated_*.adb") use ("-gnat2022", "-gnatws");  --  no warnings
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


.. index:: pair: Linker; package, switches

Linker switches
===============

Linker switches are declared in the ``Linker`` package:

.. code-block:: gpr

   package Linker is
      for Switches ("Ada") use ("-lm", "-lpthread");
   end Linker;

The index follows the same lookup order as ``Compiler'Switches``: source
file name or glob first, then language name, then ``others``.

Additional link libraries can also be declared with ``Linker_Options`` in
any source file (Ada pragma or GNAT-specific source annotation); GPRbuild
collects these automatically from ALI files.


Binder switches
===============

For Ada programs, GPRbuild invokes the binder (``gnatbind``) before
linking. Binder switches go in the ``Binder`` package:

.. code-block:: gpr

   package Binder is
      for Switches ("Ada") use ("-Es");  --  symbolic traceback
   end Binder;

Global build switches
=====================

The ``Builder`` package accepts a ``Global_Compilation_Switches`` attribute
that prepends switches to every compilation in the project tree, regardless
of language:

.. code-block:: gpr

   package Builder is
      for Global_Compilation_Switches ("Ada") use ("-gnatwa");
      for Global_Compilation_Switches ("C")   use ("-Wall");
   end Builder;

Unlike ``Compiler'Switches``, these are applied globally - including to
imported projects - and cannot be overridden per source file. Use them
sparingly; prefer ``Compiler'Switches`` for project-local settings.


Common GPRbuild options
=======================

``-X``\ *name*\ ``=``\ *value*
  Set the value of an external variable. This is the primary way to select
  build configurations such as debug or release. See :ref:`Scenarios` for
  how external variables are declared and used in project files.

``-j``\ *N*
  Use *N* parallel compilation jobs. ``-j0`` uses all available cores
  (the default). ``-j1`` forces sequential builds, which is useful for
  reading diagnostics in order.

``-f``
  Force recompilation of all sources regardless of their build signatures.

``-c``
  Compile only; do not bind or link.

``-v``
  Verbose output: print each compilation and link command as it is
  executed.

``-q``
  Quiet output: suppress informational messages; show only warnings and
  errors.

``-k``
  Keep going after a compilation error: compile as many sources as
  possible before stopping.


.. index:: out-of-tree build, pair: switch; --relocate-build-tree

Out-of-tree builds
==================

By default, build artifacts are written to the directories declared in
the project file. To redirect all artifacts to a separate directory
without modifying the project file, use ``--relocate-build-tree``:

.. code-block:: shell

   $ gprbuild -P my_proj.gpr --relocate-build-tree=/tmp/build

Each artifact directory is mirrored under the given path, keeping the
source tree untouched. See the *GPR Reference Manual*
(:ref:`RM_Out_Of_Tree_Builds`) for the full description, including the
``--root-dir`` option for project trees that span multiple directories.
