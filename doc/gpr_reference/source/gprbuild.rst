.. _RM_GPRbuild:

.. index:: GPRbuild

********************
GPRbuild Reference
********************

GPRbuild is a multi-language build tool for GPR project trees. It compiles
sources, binds Ada programs, builds libraries, and links executables, using
a persistent build database and checksum-based change detection for reliable
incremental builds.

GPRbuild2, the current build engine, models the build as a directed acyclic
graph (DAG) of actions and is the recommended engine.


.. index:: pair: GPRbuild; build engine selection, GNAT_GPR_ENGINE

Build Engine Selection
======================

Two build engines are available. Set the :envvar:`GNAT_GPR_ENGINE` environment
variable, or pass ``--gpr=<n>`` on the command line (takes precedence):

``1`` / ``legacy``
  The original GPRbuild engine. Current default.

``2`` / ``new``
  GPRbuild2: DAG-based execution, checksum-based change detection,
  persistent build database. The recommended engine.

.. code-block:: shell

   GNAT_GPR_ENGINE=2 gprbuild -P my_proj.gpr
   gprbuild --gpr=2 -P my_proj.gpr

The rest of this chapter documents GPRbuild2 behavior and switches.


Command Line
============

Syntax
------

::

   gprbuild [<proj>.gpr] [switches] [mains]
    {[-cargs[:<lang>] opts] [-bargs[:<lang>] opts]
     [-largs opts] [-kargs opts] [-gargs opts]}

Project file
------------

See :ref:`RM_Common_Options` for project file discovery rules and the
project search path environment variables.

Main sources
------------

Main source files to build may be listed on the command line. If omitted,
GPRbuild uses the ``Main`` attribute of the main project. If that is also
absent, no executable is built.

Pass-through option groups
--------------------------

Options may be forwarded to individual tools by preceding them with a section
switch. Each section switch introduces options that apply up to the next
section switch or end of the command line.

``-cargs``
  All compilers (all languages).

``-cargs:``\ *lang*
  The compiler for *lang* only.

``-bargs``
  All binder drivers.

``-bargs:``\ *lang*
  The binder driver for *lang* only.

``-largs``
  The linker.

``-kargs``
  GPRconfig (auto-configuration invocation).

``-gargs`` / ``-margs``
  GPRbuild itself (useful after other section switches).


Switches
========

Project, configuration, and common diagnostics switches are documented in
:ref:`RM_Common_Options`. The sections below cover GPRbuild-specific switches.


.. index:: pair: GPRbuild; phase selection, pair: switch; -c, pair: switch; -b, pair: switch; -l

Phase selection
---------------

``-c``
  Include only compile actions in the DAG.

``-b``
  Include only bind actions in the DAG.

``-l``
  Include only link actions in the DAG.

``-u``
  Compile only the source files named on the command line (or all sources of
  the main project if none named). No binding or linking.

``-U``
  Compile only the source files named on the command line (or all sources of
  all projects if none named). No binding or linking.

``-r``
  With ``-c`` or ``-u`` and no entry points: compile all sources of all
  projects recursively.

``-z``
  Build without a main subprogram (zero-main mode).


.. index:: pair: GPRbuild; build behavior, pair: switch; -f, pair: switch; -j, pair: switch; -k

Build behavior
---------------

``-f``
  Force re-execution of all actions, ignoring stored signatures.

``-j``\ *num*
  Maximum number of actions to run in parallel. ``-j0`` uses one job per
  CPU core. GPRbuild2 automatically coordinates with a GNU make jobserver
  when one is present.

``-k``
  Keep executing independent actions after a failure. Actions that depend on
  a failed action are skipped.

``-p`` / ``--create-missing-dirs``
  Create missing object, library, and executable directories, including
  absolute paths.

``-R``
  Do not pass a run-path option to the linker, even when ``Run_Path_Option``
  is declared.

``-o`` *name*
  Name the output executable *name*. Requires exactly one entry point.

``-eI``\ *nn*
  Index of the main unit in a multi-unit source file.

``--no-indirect-imports``
  After each successful compilation, verify that the source only uses files
  from directly imported projects. A violation invalidates the compilation
  artifacts.

``--indirect-imports``
  Allow use of files from directly or indirectly imported projects (default).
  Cancels a preceding ``--no-indirect-imports``.

``--no-object-check``
  Do not verify that an object file was produced after compilation.

``--no-split-units``
  Require all parts of an Ada compilation unit to come from the same project
  view.

``--no-sal-binding``
  Reuse binder artifacts from a previous build for Stand-Alone Libraries.
  Unsafe; may produce incorrect results if sources have changed.

``--restricted-to-languages=``\ *lang1*\ ``,``\ *lang2*
  Include only compile actions for the listed languages in the DAG.

``--compiler-subst=``\ *lang*\ ``,``\ *tool*
  Use *tool* instead of the configured compiler for *lang*.

``--temp-dir=[os|obj]``
  Control where GPRbuild places temporary files: ``os`` uses the system
  temporary directory; ``obj`` uses the object directory of the owning
  project.

``--unchecked-shared-lib-imports``
  Allow shared library projects to import non-shared-library projects.


Output and diagnostics
-----------------------

See :ref:`RM_Common_Options` for the ``-q``, ``-v``, ``-F``, ``-we``,
``-ws``, and ``-wn`` switches.

``-d``
  Display per-action progress lines.

``--no-warnings-replay`` / ``-n``
  Do not replay the warnings of actions that were skipped because they were
  up to date. By default, GPRbuild2 replays such warnings.


Build artifacts
---------------

``--build-script=``\ *file*
  Write a shell script to *file* that reproduces the build.

``--create-map-file[=``\ *file*\ ``]``
  Ask the linker to generate a map file. Defaults to the executable name
  with a ``.map`` extension.

``--keep-temp-files``
  Do not delete temporary files created during the build.


Compiler compatibility switches
--------------------------------

The following switches are accepted for compatibility with Ada compilers and
are forwarded to the Ada compiler:

``-g[``\ *opt*\ ``]``, ``-O[``\ *level*\ ``]``, ``-fno-inline``,
``-fstack-check``, ``-nostdinc``, ``-nostdlib``


.. index:: pair: GPRbuild; build execution, DAG, incremental build

Build Execution
===============

GPRbuild2 models the build as a directed acyclic graph (DAG) of *actions*.
Each action is an atomic invocation of an external tool (compiler, binder,
archiver, linker, etc.) with a defined set of inputs and outputs. Dependency
edges encode execution order: for example, a link action depends on all
compile actions whose objects it consumes.

After loading the project tree, GPRbuild2 populates the DAG from the project
configuration and the set of *entry points* - executables declared via the
``Main`` attribute or on the command line, libraries, and any additional units
specified via the ``Roots`` attribute. The process manager then traverses the
DAG in topological order, running independent actions in parallel up to the
``-j`` limit.

Each action's up-to-dateness is determined by a *signature*: checksums over
all inputs (source files, dependency closures, switches, configuration) and
expected outputs. An action is re-executed only when its signature has changed
since the last successful run. Signatures are persisted in a build database
across invocations.

Action kinds include (non-exhaustive):

- **Compile** - invoke the language compiler on a single source file.
- **Ada bind** - invoke ``gprbind`` / ``gnatbind`` for an Ada entry point to
  produce the bind artifacts.
- **Post-bind** - compile the binder-generated Ada source.
- **Link** - invoke the linker to produce an executable or shared library.
- **Archive** - invoke the archive builder (``gprlib``) to produce a static
  library.

The ``-c``, ``-b``, and ``-l`` switches restrict which action kinds are
included in the DAG for a given invocation.


Project package
===============

The ``Builder`` package of the main project may provide default switches and
global compilation switches that apply to every invocation. See
:ref:`Package_Builder_Attributes` in the Attributes Reference for the full
attribute list and descriptions.

.. code-block:: gpr

   project My_App is
      package Builder is
         for Switches ("Ada") use ("-j4", "-k");
         for Global_Compilation_Switches ("Ada") use ("-O2");
      end Builder;
   end My_App;


Exit Codes
==========

``0``
  Success (warnings may have been issued).

``1``
  General error (invalid option, missing file, etc.).

``4``
  Underlying tool error (compiler, linker, etc.).

``5``
  Project parsing error.

``7``
  Critical internal error.
