.. _RM_GPRclean:

.. index:: GPRclean

******************
GPRclean Reference
******************

GPRclean removes the build artifacts produced by GPRbuild: object files, ALI
files, libraries, executables, and binder-generated files. It reads the same
project tree as GPRbuild so it knows exactly what was produced and where.


Command Line
============

Syntax
------

::

   gprclean [-P<proj>.gpr] [switches] [mains]

Project file and common switches
--------------------------------

See :ref:`RM_Common_Options` for project file discovery rules, project and
configuration switches, and common diagnostic switches.

Main sources
------------

Zero or more main file names may be listed on the command line to restrict
cleaning to the artifacts of those entry points only.

GPRclean applies the same source resolution rules as GPRbuild (see
:ref:`RM_GPRbuild`): a simple name is looked up across the entire project
tree, and an error is reported if it matches sources in more than one
project. This ensures GPRclean removes exactly the artifacts that GPRbuild
produced.


.. index:: pair: GPRclean; switches, pair: switch; -r, pair: switch; -n

Switches
========

``-r``
  Clean all projects in the project tree, not only the main project.

``-c``
  Delete only compiler-generated files (object files, ALI files, and their
  associated artifacts). Skip link outputs (executables, libraries).

``-p``
  After removing artifacts, delete any object, library, or executable
  directories that are now empty.

``-f``
  Force deletion of files that are not owner-writable. GPRclean will attempt
  to set write permission on the file and retry the deletion.

``-n``
  Dry-run mode. List the files that would be deleted without actually removing
  them.

``--autoconf=``\ *file.cgpr*
  In addition to the common ``--autoconf`` behavior, also schedule
  *file.cgpr* itself for deletion after cleaning.

``--remove-cargo-build-dir``
  Remove the whole Cargo target directory of each Rust project cleaned,
  that would be kept otherwise. For more details, see
  :ref:`RM_GPRclean_Rust` below.


Project package
===============

The ``Clean`` package of the main project may declare a ``Switches`` attribute
whose value is a list of GPRclean switches. These are processed before
command-line switches, so command-line switches take precedence. See
:ref:`Package_Clean_Attributes` in the Attributes Reference for the full
attribute list and descriptions.

The ``--autoconf`` switch and all switches from :ref:`RM_Common_Options`
must be given on the command line and are not accepted in
``Clean'Switches``.

.. code-block:: gpr

   project My_App is
      package Clean is
         for Switches use ("-r", "-p");
      end Clean;
   end My_App;


.. index:: pair: GPRclean; artifacts cleaned

What is cleaned
===============

For each project in scope (the main project, or all projects with ``-r``),
GPRclean deletes:

- Object files and dependency files (``*.o``, ``*.d``, etc.) from the object
  directory.
- Language-specific compilation artifacts, as defined by
  ``Source_Artifact_Extensions`` and ``Object_Artifact_Extensions`` in the
  active configuration.
- ALI files and other Ada-specific artifacts produced by the compiler and
  binder.
- The persistent build database files written by GPRbuild2.
- Files matching the ``Artifacts_In_Object_Dir`` and ``Artifacts_In_Exec_Dir``
  project attributes.
- Library files from the library directory (``Library_Dir``) and, for
  stand-alone libraries, the generated source directory
  (``Library_Src_Dir``).
- Executables listed in the ``Main`` attribute (or on the command line) from
  the executable directory.

With ``--autoconf``, the specified configuration project (``.cgpr``) file is
also deleted.


.. _RM_GPRclean_Rust:

.. index:: pair: GPRclean; Rust, pair: GPRclean; Cargo

Rust projects
=============

Rust projects are built by Cargo, so GPRclean does not delete their artifacts
itself: it runs ``cargo clean`` in the directory named by the ``Cargo.Root``
attribute.

By default the clean is scoped to what the build produced, naming the package,
the profile (``Cargo.Profile``) and the target triple (``Cargo.Rust_Target``)::

   cargo clean -p <package> --profile <profile> --target=<triple>

This is deliberately conservative. One Cargo target directory may be shared by
several packages of a workspace, by several target triples, and by both
profiles; a clean that named none of them would delete all of it. The scoped
form therefore leaves the target directory itself standing, along with its
empty subdirectories.

``--remove-cargo-build-dir`` asks for the unscoped clean instead::

   cargo clean

That removes the target directory outright. Anything else built into it goes
with it, including the artifacts of other workspace members, other target
triples and the other profile, whether or not they belong to the project tree
being cleaned. This is the only way to leave no target directory behind, which
is why it is a switch and not the default.


Exit Codes
==========

``0``
  Success.

``1``
  General error (invalid option, missing file, etc.).

``5``
  Project parsing error.
