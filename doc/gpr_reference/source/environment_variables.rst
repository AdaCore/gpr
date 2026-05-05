.. _RM_Environment_Variables:

.. index:: environment variable

*********************
Environment Variables
*********************

The following environment variables are read by GPR tools. Variables that
affect project file search paths are consulted by all project-based tools
(GPRbuild, GPRclean, GPRinstall, GPRls, GPRinspect); tool-specific variables
are noted.


Project search path
===================

.. index:: pair: environment variable; GPR_PROJECT_PATH_FILE

.. envvar:: GPR_PROJECT_PATH_FILE

   Path to a text file containing project search directories, one per line.
   Directories listed in this file are added to the project search path
   before :envvar:`GPR_PROJECT_PATH` and :envvar:`ADA_PROJECT_PATH`.

.. index:: pair: environment variable; GPR_PROJECT_PATH

.. envvar:: GPR_PROJECT_PATH

   Colon-separated (Unix) or semicolon-separated (Windows) list of
   directories to search for project files referenced by ``with`` clauses.
   Takes precedence over :envvar:`ADA_PROJECT_PATH`.

.. index:: pair: environment variable; ADA_PROJECT_PATH

.. envvar:: ADA_PROJECT_PATH

   Legacy equivalent of :envvar:`GPR_PROJECT_PATH`, accepted for backward
   compatibility. Consulted after :envvar:`GPR_PROJECT_PATH`.

The search order when resolving a project name is: the directory of the
importing project, then :envvar:`GPR_PROJECT_PATH_FILE` directories, then
:envvar:`GPR_PROJECT_PATH`, then :envvar:`ADA_PROJECT_PATH`, then the
default project path of the active toolchain.


Configuration
=============

.. index:: pair: environment variable; GPR_CONFIG

.. envvar:: GPR_CONFIG

   Path to a configuration project (``.cgpr``) file or to a directory
   containing one. When set to a directory, the tool looks for a file named
   :file:`{<target>}.cgpr` (or :file:`default.cgpr` for the native target)
   inside it. Takes precedence over the default auto-configuration mechanism
   but is overridden by an explicit ``--config`` switch on the command line.

.. index:: pair: environment variable; GPR_RUNTIME_PATH

.. envvar:: GPR_RUNTIME_PATH

   Colon-separated list of directories searched when resolving a relative
   Ada runtime path specified in the configuration project. Used by
   GPRbuild, GPRclean, GPRinstall, and GPRconfig.


Build engine
============

.. index:: pair: environment variable; GNAT_GPR_ENGINE

.. envvar:: GNAT_GPR_ENGINE

   Selects the GPRbuild engine. Accepted values: ``1`` or ``legacy`` for
   the original engine; ``2`` or ``new`` for GPRbuild2 (the DAG-based
   engine). The ``--gpr=``\ *n* command-line switch takes precedence over
   this variable. See :ref:`RM_GPRbuild` for details.


Compiler discovery
==================

.. index:: pair: environment variable; PATH

.. envvar:: PATH

   The standard executable search path. GPRconfig searches :envvar:`PATH`
   for compiler executables during auto-configuration. Any directory added
   with ``--db`` that contains compiler wrappers should be on this path.

.. index:: pair: environment variable; MAKEFLAGS

.. envvar:: MAKEFLAGS

   Parsed by GPRbuild2 to detect a GNU Make jobserver token pipe when
   GPRbuild is invoked as a sub-make. If jobserver authentication details
   are present, GPRbuild2 coordinates its parallel job limit with the
   enclosing make process automatically. Users do not normally set this
   variable directly; it is set by make.
