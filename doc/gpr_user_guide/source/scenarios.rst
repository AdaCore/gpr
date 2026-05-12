.. index:: scenario

.. _Scenarios:

*********
Scenarios
*********

A *scenario* is a named configuration of a project - for example, a debug
build, a release build, or a platform-specific variant. GPR expresses
scenarios through *typed string variables* whose values are supplied from
outside the project file, allowing a single project to describe multiple
build configurations without duplication.


.. index:: external variable, External function

External variables
==================

An external variable is a string value passed to GPR tools via the ``-X``
command-line switch:

.. code-block:: shell

   $ gprbuild -P my_proj.gpr -XBUILD=release

Inside the project file, the value is read with the ``external`` function:

.. code-block:: gpr

   Build : Build_Type := external ("BUILD", "debug");

The second argument is the default value used when the variable is not set
on the command line.

``External_As_List``
--------------------

When an external variable carries a list of values encoded as a single
string, ``External_As_List`` splits it on a given separator and returns a
string list:

.. code-block:: gpr

   for Source_Dirs use External_As_List ("EXTRA_DIRS", ":");

If the environment contains ``EXTRA_DIRS=/opt/include:/home/user/src``, the
attribute receives ``("/opt/include", "/home/user/src")``. This is useful
for injecting additional source directories, switches, or other list-valued
attributes from the build environment without modifying the project file.


.. index:: typed variable

Typed variables
===============

To constrain the set of accepted values and enable ``case`` statements,
declare a string type first:

.. code-block:: gpr

   type Build_Type is ("debug", "release", "release_checks");

   Build : Build_Type := external ("BUILD", "debug");

If the value passed via ``-X`` is not in the declared type, GPRbuild reports
a load error. This catches typos early.


.. index:: case construction

``case`` statements
===================

A ``case`` statement selects attribute values based on the current scenario
variable:

.. code-block:: gpr

   project My_App is
      type Build_Type is ("debug", "release");
      Build : Build_Type := external ("BUILD", "debug");

      for Source_Dirs use ("src");
      for Object_Dir  use "obj/" & Build;
      for Exec_Dir    use ".";
      for Main        use ("main.adb");

      package Compiler is
         case Build is
            when "debug" =>
               for Switches ("Ada") use ("-g", "-gnatwa", "-gnatVa");
            when "release" =>
               for Switches ("Ada") use ("-O2", "-gnatn");
         end case;
      end Compiler;
   end My_App;

A few things to note:

- The ``case`` expression must be a typed scenario variable.
- Every value of the type must be covered; use ``when others`` to provide a
  catch-all.
- ``case`` statements can appear at the project level and inside packages.
- Variable references (``& Build``) are allowed in attribute values, making
  it easy to keep debug and release objects in separate directories.

With this project, ``gprbuild -P my_proj.gpr`` builds a debug binary;
``gprbuild -P my_proj.gpr -XBUILD=release`` builds an optimized one.

Selecting platform-specific sources
-----------------------------------

When a ``case`` statement contains Ada naming exceptions (``for Body`` or
``for Spec`` clauses in the ``Naming`` package), the source files named in
the inactive branches are automatically removed from the project view. This
makes it straightforward to keep platform-specific variants in the same
source directory without triggering duplicate-unit errors:

.. code-block:: gpr

   type OS_Type is ("linux", "windows");
   OS : OS_Type := external ("OS", "linux");

   package Naming is
      case OS is
         when "linux"   => for Body ("Foo") use "foo_linux.adb";
         when "windows" => for Body ("Foo") use "foo_windows.adb";
      end case;
   end Naming;

When building with ``-XOS=linux``, ``foo_windows.adb`` is excluded from the
project view even though it lives in the source directory.

The same result can be achieved for any language using
``Excluded_Source_Files`` in a ``case`` statement:

.. code-block:: gpr

   case OS is
      when "linux"   => for Excluded_Source_Files use ("foo_windows.c");
      when "windows" => for Excluded_Source_Files use ("foo_linux.c");
   end case;

A third approach is to place target-specific sources in dedicated
subdirectories. Declare the common directory first, then extend
``Source_Dirs`` in the ``case`` statement using the project's own attribute
value:

.. code-block:: none

   src/
   ├── common/
   ├── linux/
   └── windows/

.. code-block:: gpr

   for Source_Dirs use ("src/common");

   case OS is
      when "linux"   =>
         for Source_Dirs use My_Proj'Source_Dirs & ("src/linux");
      when "windows" =>
         for Source_Dirs use My_Proj'Source_Dirs & ("src/windows");
   end case;

This layout scales well when many files differ between targets and avoids
the need for per-file exclusions or naming exceptions.


Multiple scenario variables
===========================

A project can declare any number of scenario variables, each controlling an
independent dimension of the configuration:

.. code-block:: gpr

   project My_App is
      type Build_Type is ("debug", "release");
      type OS_Type    is ("linux", "windows", "macos");

      Build : Build_Type := external ("BUILD", "debug");
      OS    : OS_Type    := external ("OS",    "linux");

      for Source_Dirs use ("src/common");
      for Object_Dir  use "obj/" & Build & "-" & OS;
      for Exec_Dir    use "bin/" & Build & "-" & OS;
      for Main        use ("main.adb");

      case OS is
         when others =>
            for Source_Dirs use My_App'Source_Dirs & ("src/" & OS);
      end case;
      ...
   end My_App;

.. tip::

   Keep the number of scenario variables small and their names consistent
   across projects in the same tree. Convention: ``BUILD`` for debug/release,
   ``TARGET`` for cross-compilation target, ``RUNTIME`` for Ada runtime
   variant. Consistent naming lets callers pass ``-X`` switches once at the
   root and have them apply everywhere.


Sharing scenario variables
==========================

When a project tree has multiple projects, all should use the same typed
variable declaration for a given scenario. The common pattern is to declare
the type and variable in an ``abstract`` project and import it everywhere:

.. code-block:: gpr

   --  config.gpr
   abstract project Config is
      type Build_Type is ("debug", "release");
      Build : Build_Type := external ("BUILD", "debug");
   end Config;

.. code-block:: gpr

   --  my_comp.gpr
   with "config.gpr";
   project My_Comp is
      package Compiler is
         case Config.Build is
            when "debug"   => for Switches ("Ada") use ("-g");
            when "release" => for Switches ("Ada") use ("-O2");
         end case;
      end Compiler;
      ...
   end My_Comp;

.. code-block:: gpr

   --  my_app.gpr
   with "config.gpr";
   with "my_comp.gpr";
   project My_App is
      package Compiler is
         case Config.Build is
            when "debug"   => for Switches ("Ada") use ("-g", "-gnatwa");
            when "release" => for Switches ("Ada") use ("-O2");
         end case;
      end Compiler;
      ...
   end My_App;

A single ``-XBUILD=release`` on the command line then applies consistently
to the whole tree.

.. warning::

   External variable names are global to the project tree: two projects that
   both read the same variable name will always receive the same value.
   If their typed declarations differ - for example one accepts
   ``("debug", "release")`` and another accepts ``("debug", "release",
   "release_checks")`` - then passing a value valid for the wider type but
   not the narrower one causes a load error. To avoid this, share a single
   type declaration (as shown above) so that all projects in the tree agree
   on the set of accepted values.
