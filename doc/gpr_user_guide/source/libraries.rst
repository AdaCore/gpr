.. index:: library project

.. _Libraries:

*********
Libraries
*********

A *library project* builds a static or shared library from its sources
instead of producing an executable. Other projects import the library using
a ``with`` clause and link against it automatically.


.. index:: Library_Name, Library_Dir

Declaring a library project
===========================

Add the ``library`` qualifier and the two required library attributes:

.. code-block:: gpr

   library project My_Lib is
      for Source_Dirs  use ("src");
      for Object_Dir   use "obj";
      for Library_Name use "mylib";
      for Library_Dir  use "lib";
   end My_Lib;

``Library_Name``
  The base name of the library. The actual file name produced depends on
  whether the library is an archive or a shared library, and on the
  target platform - both of which are determined by the active
  configuration.

``Library_Dir``
  The directory where the library file is placed. Must be distinct from
  ``Object_Dir``.

A library project cannot declare a ``Main`` attribute; it produces a
library, not an executable.


.. index:: Library_Kind, static library, dynamic library

Library kinds
=============

The ``Library_Kind`` attribute selects the type of library to build:

.. code-block:: gpr

   library project My_Lib is
      ...
      for Library_Kind use "static";   --  default
   end My_Lib;

``"static"``
  A static archive linked directly into each executable that uses it.
  This is the default.

``"relocatable"``
  A shared library (``.so`` on Linux, ``.dll`` on Windows) loaded at
  program start. Only one copy lives in memory when multiple programs use
  it.

``"static-pic"``
  A position-independent static archive. Needed when the library will
  itself be bundled into a shared library.

.. tip::

   Use scenario variables (see :ref:`Scenarios`) to build both a static and
   a shared variant from the same project file without duplicating
   declarations.


.. index:: stand-alone library

Stand-alone libraries
=====================

A *stand-alone library* (SAL) bundles its own Ada elaboration code so that
it can be loaded and initialized independently of the main program's
elaboration sequence. This is required for plugins and shared libraries
loaded at runtime.

``Library_Interface`` lists the Ada *unit names* that form the library's
public API. ``Library_Standalone`` controls the degree of self-containment:

``"standard"``
  The library has its own elaboration code for the units listed in
  ``Library_Interface``. Ada dependencies from the rest of the
  program's elaboration are still expected to be present at load time.
  This is the default when ``Library_Interface`` is set.

``"encapsulated"``
  Like ``"standard"``, but all Ada library dependencies must
  themselves be standalone libraries. This produces a fully
  self-contained library that includes the Ada runtime, with no
  external Ada elaboration dependencies.

``"no"``
  No standalone elaboration is generated. ``Library_Interface`` is
  used only to declare which units form the public API (for
  visibility purposes), without making the library self-elaborating.

Example - standard SAL:

.. code-block:: gpr

   library project My_Lib is
      for Source_Dirs        use ("src");
      for Object_Dir         use "obj";
      for Library_Name       use "mylib";
      for Library_Dir        use "lib";
      for Library_Kind       use "relocatable";
      for Library_Interface  use ("My_Lib", "My_Lib.Utils");
      for Library_Standalone use "standard";
   end My_Lib;

Example - encapsulated SAL (fully self-contained plugin):

.. code-block:: gpr

   library project My_Plugin is
      for Source_Dirs        use ("src");
      for Object_Dir         use "obj";
      for Library_Name       use "myplugin";
      for Library_Dir        use "lib";
      for Library_Kind       use "relocatable";
      for Library_Interface  use ("My_Plugin");
      for Library_Standalone use "encapsulated";
   end My_Plugin;

.. warning::

   Because an encapsulated library includes the Ada runtime, loading two
   encapsulated libraries in the same process results in duplicate runtime
   state, which leads to undefined behavior. A program should load at most
   one encapsulated library at a time.


Controlling source visibility
=============================

The ``Interfaces`` attribute restricts which source files of a library
project are visible to importing projects. It takes a list of source file
names:

.. code-block:: gpr

   library project My_Lib is
      for Source_Dirs  use ("src");
      for Object_Dir   use "obj";
      for Library_Name use "mylib";
      for Library_Dir  use "lib";
      for Interfaces   use ("my_lib.ads", "my_lib-utils.ads");
   end My_Lib;

Units whose source files are not listed in ``Interfaces`` can still be used
internally by the library but cannot be depended on from importing projects.
For shared libraries, only the symbols from the interface units are exported;
all other symbols are kept internal to the library.

``Interfaces`` and ``Library_Interface`` serve different purposes and can
be used together: ``Interfaces`` controls source-level visibility and symbol
export for shared libraries, while ``Library_Interface`` declares the Ada
units forming the public API and drives standalone elaboration behavior via
``Library_Standalone``.


.. index:: aggregate library project

Aggregate library projects
==========================

An *aggregate library project* builds a single library by collecting the
object files from a set of constituent projects. It combines the
``aggregate`` and ``library`` qualifiers and requires ``Library_Name``,
``Library_Dir``, and ``Project_Files``:

.. code-block:: gpr

   aggregate library project Full_Lib is
      for Project_Files use ("module_a/a.gpr",
                             "module_b/b.gpr");
      for Library_Name use "full";
      for Library_Dir  use "lib";
   end Full_Lib;

The resulting library can be of any kind (``static``, ``relocatable``,
``static-pic``), independently of the library kinds of the constituent
projects. Unlike a plain aggregate project, an aggregate library project
**can** be ``with``-ed by any other project and may appear anywhere in the
dependency graph.

.. note::

   An aggregate library project is distinct from a regular aggregate project
   (see :ref:`Aggregate_Projects_UG`). In particular, it cannot set external
   variable values via ``External`` - only plain aggregate projects support
   that attribute.

It is advisable to declare ``Object_Dir`` when constituent sources may need
to be recompiled with different flags. GPRbuild stores the recompiled objects
there, keeping the constituent projects' own directories untouched.

``Library_Interface``, ``Interfaces``, and ``Library_Standalone`` work as
usual, referring to sources from the constituent projects.
