.. index:: best practices

.. _Best_Practices:

**************
Best Practices
**************

This chapter collects conventions and recommendations that apply across all
project types.


Project layout
==============

Keep one project file per component. A component is a unit of reuse: a
library that other projects can import, or an executable with its own source
tree. Mixing unrelated components in a single project makes incremental builds
coarser and reuse harder.

Use dedicated directories for sources, objects, and outputs, and keep them
separate from each other:

.. code-block:: none

   mylib/
   ├── mylib.gpr
   ├── src/
   ├── obj/
   └── lib/

Never place generated artifacts (object files, libraries, executables) inside
the source tree. Mixing them with sources complicates version control, as
generated files appear as untracked changes and require explicit exclusion.


Scenario variables
==================

Keep the number of scenario variables small. Each variable multiplies the
number of distinct project configurations; a tree with four binary variables
has sixteen possible configurations to reason about.

For variable names, two strategies work well depending on the context:

- **Consistent names** across all projects in a tree, so that a single ``-X``
  switch on the command line applies everywhere. The conventional names are
  ``BUILD`` (debug/release), ``TARGET`` (cross-compilation target), and
  ``RUNTIME`` (Ada runtime variant). Use this approach for variables that
  genuinely apply to the whole tree.

- **Distinctive names** for variables that are local to a specific component
  and should not accidentally match a variable in another project. A
  component-specific prefix (e.g. ``MYLIB_FLAVOR``) prevents unintended
  coupling.

Declare scenario types and variables in a shared ``abstract`` project and
import it everywhere (see :ref:`Scenarios`). This guarantees that all projects
agree on the accepted value set and avoids load errors when a value valid for
one project is rejected by another.


Object and output directories
==============================

Never share ``Object_Dir`` or ``Library_Dir`` between two projects. If two
projects write build artifacts to the same directory, a filename collision is
reported as an error.

When scenario variables produce multiple build configurations from the same
project file, include the variable value in the directory path so each
configuration writes to its own location:

.. code-block:: gpr

   for Object_Dir  use "obj/" & Build;
   for Library_Dir use "lib/" & Build;

If two configurations share the same ``Object_Dir``, switching between them
forces a full rebuild because all objects from the previous configuration are
present and must be replaced.


Multi-project hygiene
======================

Declare shared compiler switches, scenario variables, and naming conventions
in a single ``abstract`` project and import it from every project in the tree.
This is the primary mechanism for keeping a large project tree consistent.

Use ``Compiler'Switches`` (in individual projects) rather than
``Builder'Global_Compilation_Switches`` for project-local settings.
``Global_Compilation_Switches`` propagates to all imported projects, which
is rarely the intended effect and makes it harder to reason about what flags
are applied to each source.

Avoid ``limited with`` unless genuinely necessary (see
:ref:`Multi_Project_Systems`). A need for it often signals that two components
should be refactored into a shared lower-level dependency rather than kept
mutually visible.


Performance
===========

For large project trees with many independent components, use an aggregate
project as the root. Shared sources are deduplicated across subtrees, and
GPRbuild maximises parallelism across the whole group in a single invocation
(see :ref:`Aggregate_Projects_UG`).

Declare ``Object_Dir`` on aggregate projects so that incremental build data is
stored in a well-known location outside the source tree and outside any
VCS-controlled directory.

For stable subcomponents that change infrequently, consider installing them
with ``gprinstall`` (see :ref:`Working_With_Tools`) and referencing them from
the project search path. Installed projects are treated as *externally built*
- their sources are considered read-only and their pre-built artifacts are
consumed directly - which significantly reduces build times for large trees.

Consider using a static configuration project via ``--config`` or
``--autoconf``. The default automatic configuration probes the host for many
potential toolchain candidates and this may generate a significant overhead.
Note that with a static configuration, it must be updated whenever the
toolchain changes or new languages are used in the project.
