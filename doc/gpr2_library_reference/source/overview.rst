.. index:: GPR2 library, project model

********
Overview
********

.. note::

   This document assumes familiarity with the GPR project file language. If
   you are new to GPR, read the *GPR User Guide* for a task-oriented
   introduction or the *GPR Reference Manual* for a complete language
   specification before proceeding.

The GPR2 library provides a complete parser and project model for GPR (GNAT
Project) files. It is the foundation used by all AdaCore build tools
(``gprbuild``, ``gprclean``, ``gprinstall``, ``gprconfig``, ...) and is
available to third-party tools that need to reason about Ada project structure.

Core capabilities
=================

* **Loading** a project tree, resolving all imports, aggregations, extensions,
  the Ada runtime project, and the configuration project (``.cgpr``).
* **Reading** project attributes, variables, types, and packages from any view
  in the tree, with full evaluation of default values, aliases, and
  configuration overrides.
* **Enumerating** source files and Ada compilation units, including their
  relationships (spec/body/separate, other-part links, dependencies).
* **Building incrementally** using a DAG of typed actions whose signatures
  are persisted across runs.

Design principles
=================

**Reference counting.**
All library objects (``Tree``, ``View``, ``Attribute``, ...) use reference
counting internally. There is no manual memory management; objects are safe to
copy, store in containers, and pass by value.

**No exceptions for expected conditions.**
Operations that can fail in normal use (missing attribute, source not found,
...) return an undefined object or a Boolean, rather than raising an exception.
Callers guard calls with ``Has_XXX`` or ``Is_Defined`` checks. Preconditions
on getters enforce this at runtime when the library is built with
``GPR2_BUILD=debug`` or ``GPR2_BUILD=release_checks``.
At the highest level, ``Tree.Load`` and ``Build.Tree_Db.Execute`` return a
simple success/failure status so callers do not need exception handlers for
normal error paths.

**Typed paths.**
All filesystem paths in the API use ``GPR2.Path_Name.Object`` rather than raw
strings.

**Reporter-based diagnostics.**
Diagnostic messages are automatically forwarded to a ``GPR2.Reporter`` object
during tree loading and by the incremental builder. Tools configure a reporter
to control how messages reach the end user - for example routing them to a
console, a GUI panel, or a structured logger. The underlying
``GPR2.Log.Object`` is accessible for post-processing purposes, but direct use
is rarely needed.

Package naming
==============

Ada child packages follow the GNAT file-naming convention and mirror the
directory hierarchy. The package ``GPR2.Project.Tree`` lives in
``gpr2-project-tree.ads``. Public API packages are under ``src/lib/``; build
infrastructure is under ``src/build/``.

Key packages at a glance
========================

``GPR2.Options``
  Parses common GPR tool switches and loads a tree

``GPR2.Project.Tree``
  Root object; owns all views and the build database

``GPR2.Project.View``
  One project in the tree; source of attributes, sources, units

``GPR2.Project.Attribute``
  A single evaluated attribute value

``GPR2.Project.Variable``
  A project-level or package-level variable

``GPR2.Build.Source``
  A source file as seen by the build system, with unit and dependency info

``GPR2.Build.Compilation_Unit``
  An Ada compilation unit (spec, body, separates) across the build tree

``GPR2.Path_Name``
  Value type for filesystem paths

``GPR2.Reporter``
  Configurable sink that routes diagnostics to the end user

``GPR2.Build.Tree_Db``
  Persistent build database and action DAG

``GPR2.Build.Actions``
  Abstract base type for a single build step
