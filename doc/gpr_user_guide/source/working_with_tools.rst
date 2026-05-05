.. index:: GPRbuild, GPRclean, GPRinstall, GPRls

.. _Working_With_Tools:

******************
Working with Tools
******************

The GPR tool suite includes several companion tools that share the same project
model as GPRbuild. Tools that operate on existing project trees all accept the
standard project options (``-P``, ``-X``, ``-aP``). A separate group of tools
generates GPR project files as output rather than consuming them.

Each tool is fully documented in the *GPR Reference Manual*; this chapter
provides a task-oriented overview and the most commonly used options.


Tools that operate on project trees
=====================================

gprclean
---------

``gprclean`` removes the build artifacts produced by GPRbuild: object files,
ALI files, libraries, executables, and binder-generated files. It reads the
same project tree to know exactly what was produced and where.

.. code-block:: shell

   $ gprclean -P myproject.gpr        # clean root project only
   $ gprclean -P myproject.gpr -r     # clean entire project tree

Key options:

``-r``
  Clean all projects in the tree, not just the root.

``-c``
  Delete only compiler-generated files (object files, ALI files). Skip
  executables and libraries.

``-n``
  Dry run: list the files that would be deleted without removing them.

``-p``
  Remove empty object, library, and executable directories after cleaning.

See the *GPR Reference Manual*, chapter *gprclean*, for the complete option
reference.


gprinstall
-----------

``gprinstall`` copies build results into a target installation prefix. It
records every installed file in a manifest, enabling precise uninstall later.

.. code-block:: shell

   $ gprinstall -P mylib.gpr --prefix=/usr/local
   $ gprinstall -P mylib.gpr --prefix=/usr/local --uninstall
   $ gprinstall --list

Key options:

``--prefix=<dir>``
  Root installation directory. Defaults to the active toolchain prefix.

``--mode=<dev|usage>``
  ``dev`` (default) installs sources, ALI files, and libraries for use as a
  development dependency. ``usage`` installs only what end users need
  (shared libraries, executables).

``--build-name=<name>``
  Tag the installation with a build variant name (e.g. ``debug``,
  ``production``). Allows multiple variants to coexist under the same prefix.

``-r``
  Install all imported projects, not just the root.

``--uninstall``
  Remove files recorded in the manifest for the named project.

``--list``
  Print all installed packages found under the prefix.

See the *GPR Reference Manual*, chapter *gprinstall*, for the complete option
reference.


gprls
------

``gprls`` lists the sources, units, objects, and dependencies of a project
tree. It reads GPRbuild's build database to report the up-to-date status of
each artifact.

.. code-block:: shell

   $ gprls -P myproject.gpr -s        # list source files
   $ gprls -P myproject.gpr -o        # list object files
   $ gprls -P myproject.gpr -d        # list dependencies with status

Key options:

``-s``
  Print source file for each compilation unit.

``-o``
  Print object file for each compilation unit.

``-u``
  Print unit name for each Ada compilation unit.

``-d``
  List source file dependencies with their up-to-date status (``OK`` or
  ``DIF``).

``-U``
  Include sources from all projects in the tree, not just the root.

``--closure``
  Compute the transitive compilation closure of the named source files.

See the *GPR Reference Manual*, chapter *gprls*, for the complete option
reference.


gprinspect
-----------

``gprinspect`` is a diagnostic tool that loads a project tree and displays its
structure: project relationships, source directories, attributes, packages,
variables, and type definitions. It is useful for understanding how a project
tree is resolved and for debugging unexpected attribute values.

.. code-block:: shell

   $ gprinspect -P myproject.gpr --all -r

Key options:

``--display=<textual|json|json-compact>``
  Output format. ``textual`` (default) is human-readable; ``json`` is suited
  for tool integration.

``-r``
  Display all projects in the tree, not just the root.

``--all``
  Display attributes, packages, variables, and type definitions.

``-c``
  Include attributes inherited from the active configuration project.

See the *GPR Reference Manual*, chapter *gprinspect*, for the complete option
reference.


Tools that generate project files
====================================

Unlike the tools above, the following tools produce GPR files as their output
rather than loading and acting on an existing project tree.


gprconfig
----------

``gprconfig`` probes the host for available compilers and generates a
configuration project (``.cgpr``) describing the selected toolchains to all
GPR tools. It is normally invoked automatically by GPRbuild when no
configuration project is found, but can also be run manually to produce a
reusable or script-driven configuration.

.. code-block:: shell

   $ gprconfig                              # interactive selection
   $ gprconfig --batch --config=Ada         # non-interactive, Ada compiler only
   $ gprconfig --batch --config=Ada --config=C -o my.cgpr

Key options:

``--target=<name>``
  Select compilers for the given target. Defaults to native. Use ``all`` to
  list compilers for all known targets.

``--batch``
  Non-interactive mode; compiler selection is driven entirely by ``--config``
  options.

``--config=<selector>``
  Pre-select a compiler. The selector is a comma-separated list of fields:
  ``language,version,runtime,path,name``. Only the language field is required;
  trailing empty fields may be omitted. May be repeated (at most once per
  language).

  .. code-block:: shell

     $ gprconfig --batch --config=Ada                    # any Ada compiler
     $ gprconfig --batch --config=Ada,,native            # Ada, native runtime
     $ gprconfig --batch --config=Ada,14,,/opt/gnat/bin  # Ada 14, specific path

``-o <file>``
  Write the configuration project to the given file. Defaults to
  ``default.cgpr`` in the GPRbuild configuration directory.

``--show-targets``
  Print the list of targets for which compilers are available, then exit.

See the *GPR Reference Manual*, chapter *gprconfig*, for the complete option
reference.


gprname
--------

``gprname`` scans source directories for Ada source files, identifies the Ada
units they contain, and generates a GPR project file with a ``Naming`` package
that maps each unit to its source file. It is the standard tool for setting up
a project whose sources do not follow the default GNAT naming conventions.

.. code-block:: shell

   $ gprname -P myproject.gpr -d src/

This creates (or updates) ``myproject.gpr`` and a companion
``myproject_naming.gpr`` containing the ``Naming`` package. A source list file
``myproject_source_list.txt`` is also written.

Key options:

``-d <dir>``
  Add a source directory to scan. Append ``/**`` to search the directory and
  all its subdirectories recursively. May be repeated.

``-f <pattern>``
  Add a filename pattern for C sources.

``-x <pattern>``
  Exclude files matching a pattern from Ada source scanning.

``--and``
  Begin a new section with different source directories or patterns.

See the *GPR Reference Manual*, chapter *gprname*, for the complete option
reference.
