.. _RM_Common_Options:

.. index:: common options, command-line options

***************************
Common Command-Line Options
***************************

GPRbuild, GPRclean, GPRinstall, GPRname, GPRls, and GPRinspect all operate on
a GPR project tree and share a common set of command-line options for project
loading, configuration, and diagnostics. Tool-specific options are documented
in each tool's own chapter.

GPRconfig is not covered here: it works directly with the compiler knowledge
base rather than a project tree and has its own independent option set (see
:ref:`RM_GPRconfig`).

.. tip::

   When developing a custom project-aware tool, support most if not all of
   these options to ensure a consistent user experience across the GPR tool
   suite.


.. index:: pair: switch; -P

Project file
============

Every project-based GPR tool requires a project file. It may be supplied
directly with ``-P`` or located automatically: the tool uses
:file:`default.gpr` in the current directory, or the sole ``.gpr`` file
present if there is exactly one. Relative paths are searched against the
current directory, then against :envvar:`GPR_PROJECT_PATH_FILE`,
:envvar:`GPR_PROJECT_PATH`, and :envvar:`ADA_PROJECT_PATH` (in that order).

:envvar:`GPR_PROJECT_PATH_FILE`, when set, names a text file containing one
project directory per line. :envvar:`GPR_PROJECT_PATH` and
:envvar:`ADA_PROJECT_PATH` contain colon-separated directory lists.


.. index:: pair: switch; -X, pair: switch; --config, pair: switch; --autoconf, pair: switch; --target, pair: switch; --RTS

Project and configuration switches
==================================

``-P`` *proj*
  Specify the main project file. The space between ``-P`` and the name is
  optional.

``-aP`` *dir*
  Add *dir* to the project search path.

``-X``\ *NAME*\ ``=``\ *value*
  Set external reference *NAME* to *value* for use in project files.

``--no-project``
  Do not use any project file in the current directory; use the default
  project from :file:`<prefix>/share/gpr`.

``--implicit-with=``\ *proj.gpr*
  Add the given project as an implicit ``limited with`` to every project in
  the tree.

``--config=``\ *file.cgpr*
  Use *file.cgpr* as the configuration project. The file must exist.

``--autoconf=``\ *file.cgpr*
  Use *file.cgpr* as the configuration project, invoking GPRconfig to create
  it if it does not yet exist.

``--target=``\ *name*
  Specify the target for cross-platform builds. Mutually exclusive with
  ``--config`` and ``--autoconf``.

``--RTS[``\ ``:``\ *lang*\ ``]=``\ *runtime*
  Specify the runtime directory for *lang* (or Ada if *lang* is omitted).

``--db`` *dir*
  Parse *dir* as an additional knowledge base directory.

``--db-``
  Do not load the standard knowledge base.

``--relocate-build-tree[=``\ *dir*\ ``]``
  Relocate all object, library, and executable directories under the current
  working directory (or *dir*). See :ref:`RM_Out_Of_Tree_Builds`.

``--root-dir=``\ *dir*
  Root directory for ``--relocate-build-tree`` relocation. Defaults to the
  main project directory; override when artifact directories lie outside it.
  See :ref:`RM_Out_Of_Tree_Builds`.

``--subdirs=``\ *dir*
  Append *dir* to all object, library, and executable directories, creating
  them as needed.

``--src-subdirs=``\ *dir*
  For each project, prepend *obj-dir*\ ``/``\ *dir* to the source directories,
  where *obj-dir* is the project's object directory.
  Useful for temporary source overrides such as instrumentation.


.. index:: pair: switch; -q, pair: switch; -v, pair: switch; -we, pair: switch; -ws

Output and diagnostics
======================

``-q``
  Quiet: display only errors.

``-v``
  Verbose: display full paths and all spawned-process options.

``-F``
  Use full project path names in brief error messages.

``-we``
  Treat project-file warnings as errors.

``-ws``
  Suppress project-file warnings. Does not affect compiler warnings.

``-wn``
  Restore default warning behavior (cancels ``-we`` or ``-ws``).
