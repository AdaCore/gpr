.. index:: project extension, extends

.. _Project_Extension_UG:

*****************
Project Extension
*****************

Project extension lets one project inherit sources and settings from another
and selectively override them. A common use case is patching a third-party
library: the extending project replaces only the files that need to change
while reusing everything else unchanged.


Basic syntax
============

.. code-block:: gpr

   project Patched extends "original/original.gpr" is
      --  Source files here shadow those in original.gpr.
      --  All other sources and settings are inherited.
   end Patched;

The path in ``extends`` is relative to the extending project file.


What is inherited
=================

An extending project inherits:

- **Sources** - all source files from the base are implicitly present. A
  source file in the extending project whose *base name* matches a base source
  shadows it.
- **Attribute values** - project-level attributes not declared in the extending
  project are inherited from the base.
- **Packages** - any package not declared in the extending project is inherited
  in full from the base.

**Exception:** ``Linker'Linker_Options`` is never inherited.

``Object_Dir`` and ``Library_Dir`` must always be overridden in the extending
project; the two projects cannot share the same output directories.

.. code-block:: gpr

   project Patched extends "original/original.gpr" is
      for Object_Dir  use "obj";
      for Library_Dir use "lib";  -- only for library projects
   end Patched;


.. index:: source shadowing

Overriding sources
==================

Place a source file with the same base name in the extending project's source
directory. GPRbuild uses the extending project's version and ignores the base
version entirely.

To remove an inherited source without providing a replacement, list it in
``Excluded_Source_Files``:

.. code-block:: gpr

   project Patched extends "original/original.gpr" is
      for Object_Dir use "obj";
      for Excluded_Source_Files use ("obsolete.adb");
   end Patched;


Overriding attributes and packages
====================================

Attribute values are overridden by simply re-declaring them. Package
declarations, however, **replace the inherited package entirely** - no
per-attribute inheritance occurs within a package:

.. code-block:: gpr

   project Patched extends "original/original.gpr" is
      for Object_Dir use "obj";

      --  This replaces Original's entire Compiler package:
      package Compiler is
         for Switches ("Ada") use ("-O2");
      end Compiler;
   end Patched;

To *amend* a package rather than replace it, use ``package X extends Base.X``:

.. code-block:: gpr

   project Patched extends "original/original.gpr" is
      for Object_Dir use "obj";

      --  Inherit Original's Compiler switches and add one more:
      package Compiler extends Original.Compiler is
         for Switches ("Ada") use Original.Compiler'Switches ("Ada") & ("-gnatwa");
      end Compiler;
   end Patched;


Abstract base projects
======================

A common pattern is to declare common settings in an ``abstract`` base project
and extend it for each concrete build variant. The concrete project still needs
to declare its own project-specific attributes (such as ``Object_Dir``) for its
own purposes:

.. code-block:: gpr

   --  base.gpr
   abstract project Base is
      type Build_Type is ("debug", "release");
      Build : Build_Type := external ("BUILD", "debug");

      package Compiler is
         case Build is
            when "debug"   => for Switches ("Ada") use ("-g", "-gnatwa");
            when "release" => for Switches ("Ada") use ("-O2");
         end case;
      end Compiler;
   end Base;

.. code-block:: gpr

   --  my_app.gpr
   project My_App extends "base.gpr" is
      for Source_Dirs use ("src");
      for Object_Dir  use "obj/" & Base.Build;
      for Exec_Dir    use ".";
      for Main        use ("main.adb");
   end My_App;


Import redirection
==================

When a project *D* extends *A*, and *D* also imports *C* which extends *B*,
and *A* imports *B* - then within *D*'s project tree, *A*'s import of *B* is
automatically redirected to *C*. The build system connects these automatically
when both extensions are present in the tree.

For example, suppose *B* defines a variable that *A* references:

.. code-block:: gpr

   --  b.gpr
   library project B is
      Build_Suffix : constant String := "debug";
      for Source_Dirs  use ("src");
      for Object_Dir   use "obj";
      for Library_Name use "b";
      for Library_Dir  use "lib";
   end B;

.. code-block:: gpr

   --  a.gpr
   with "b.gpr";
   project A is
      for Source_Dirs use ("src");
      for Object_Dir  use "obj-" & B.Build_Suffix;  --  "obj-debug"
   end A;

*C* extends *B* and overrides the variable:

.. code-block:: gpr

   --  c.gpr
   library project C extends "b.gpr" is
      Build_Suffix : constant String := "release";
      for Object_Dir  use "obj";
      for Library_Dir use "lib";
   end C;

*D* extends *A* and imports *C*. When *D* is loaded, *A*'s import of *B* is
redirected to *C*, so *A*'s ``Object_Dir`` becomes ``"obj-release"``:

.. code-block:: gpr

   --  d.gpr
   with "c.gpr";
   project D extends "a.gpr" is
      for Object_Dir use "obj";
   end D;

Since a project tree may not depend on both an extending and an extended
project, this mechanism can only be used in simple cases. That is where
``extends all`` solves the issue for more complex structures.


.. index:: extends all

``extends all``
================

``extends all`` implicitly creates an extending project for every project in
the import closure of the named base and applies import redirection throughout
the entire subtree in one step. This makes it practical to amend any
constituent of a large project hierarchy without manually extending every
project along the import path.

.. code-block:: gpr

   project Full_Override extends all "original/original.gpr" is
      for Object_Dir use "obj";
   end Full_Override;

The same inheritance rules apply as with a plain ``extends``: each constituent
inherits its base's sources, attributes, and packages with the same semantics
described above. A source file placed in ``Full_Override``'s source directory
shadows the matching file wherever it originates in the tree.

To replace a specific constituent - for example to substitute instrumented
sources for one particular library - import an explicit extending project for
that constituent from within the ``extends all`` project. The explicit
extension takes the place of the corresponding implicit one, and import
redirection propagates it consistently across the whole subtree:

.. code-block:: gpr

   with "instrumented_lib.gpr";  --  extends "libs/my_lib/my_lib.gpr"

   project Full_Override extends all "original/original.gpr" is
      for Object_Dir use "obj";
   end Full_Override;
