.. _RM_GPRinstall:

.. index:: GPRinstall

********************
GPRinstall Reference
********************

GPRinstall copies the build results of a project tree - libraries, ALI
files, sources, executables, and generated project files - into a target
prefix directory. It records every installed file in a manifest so that
``--uninstall`` can later remove them precisely.


Command Line
============

Syntax
------

::

   gprinstall [-P<proj>.gpr] [switches]

Project file and common switches
----------------------------------

See :ref:`RM_Common_Options` for project file discovery rules, project and
configuration switches, and common diagnostic switches.


.. index:: pair: GPRinstall; operating modes, pair: switch; --uninstall, pair: switch; --list

Operating modes
===============

GPRinstall operates in one of three modes, controlled by the presence (or
absence) of ``--uninstall`` and ``--list``:

**Install** (default)
  Build results from the project tree are copied to the prefix. A manifest
  recording every installed file is written to
  :file:`{<prefix>}/{project-subdir}/manifests/{install-name}`.

**Uninstall** (``--uninstall``)
  Files listed in the named manifest are removed. If any file has been
  modified since installation its checksum will differ and GPRinstall will
  refuse to delete it unless ``-f`` is given.

**List** (``--list``)
  Scan the manifests directory and print the name of every installed package.
  With ``--stat``, also show the file count, total size, and number of
  missing files for each package.


Switches
========

.. index:: pair: GPRinstall; installation paths, pair: switch; --prefix

Installation paths
------------------

``--prefix=``\ *dir*
  Root directory for the installation. Defaults to the prefix of the active
  toolchain.

``--exec-subdir=``\ *dir*
  Subdirectory under the prefix for executables. Default: ``bin/``.

``--lib-subdir=``\ *dir*
  Subdirectory for libraries. Default: ``lib/``. When ``--ali-subdir`` is
  not given separately, it defaults to the same value as ``--lib-subdir``.

``--ali-subdir=``\ *dir*
  Subdirectory for Ada ALI files. Default: ``lib/``.

``--link-lib-subdir=``\ *dir*
  Subdirectory for shared library compatibility symlinks on Unix.
  Default: ``lib/``.

``--sources-subdir=``\ *dir*
  Subdirectory for installed source files. Default: ``include/``.

``--project-subdir=``\ *dir*
  Subdirectory for the generated GPR project file and manifests. Default:
  ``share/gpr/``.

``--cross-install``
  Adjust the prefix for cross-compilation: results are placed under
  :file:`{<prefix>}/{target}/{runtime}/`.


Install behaviour
-----------------

``-r`` / ``--recursive``
  Install all projects imported by the main project, not only the main
  project itself.

``-f`` / ``--force``
  Overwrite files that already exist at the destination. During uninstall,
  delete files even if their checksum has changed.

``-p`` / ``--create-missing-dirs``
  Create destination directories if they do not exist.

``-m``
  Minimal source copy: install only the spec files and bodies strictly
  required to use the library (relevant for stand-alone libraries).
  Without ``-m`` all sources are installed in ``dev`` mode.

``-d`` / ``--dry-run``
  Print the operations that would be performed (``cp``, ``ln -s``, etc.)
  without executing them.

``--mode=``\ *value*
  Installation mode. Accepted values:

  ``dev`` (default)
    Full developer installation: sources, ALI files, and libraries.

  ``usage``
    End-user installation: shared libraries and executables only; no
    sources or static libraries.

``--install-name=``\ *name*
  Name used for the manifest file and the generated project file. Defaults
  to the project file name without extension.

``--build-name=``\ *name*
  Tag this installation with a build name (e.g. ``debug``, ``production``).
  Results are placed in a subdirectory named
  :file:`{install-name}.{build-name}` under each subdirectory. Default:
  ``default``.

``--build-var=``\ *name*
  Name of an external variable whose value selects the build at load time.
  May be repeated. The generated project file contains a case statement
  keyed on this variable.

``--no-build-var``
  Do not generate a build selection variable in the generated project file.


Source and artifact handling
-----------------------------

``--sources-only``
  Copy only source files; skip libraries, ALI files, and executables.

``--no-project``
  Do not generate or install a GPR project file.

``--minimal-project``
  Generate a project file that records only the minimum required metadata.

``--side-debug``
  For executables and shared libraries, extract debug information into a
  separate :file:`.debug` file (using ``objcopy`` and ``strip``). The
  installed binary is stripped; the ``.debug`` file is placed alongside it
  and linked via a GNU debug link.

``--no-lib-link``
  Do not create symlinks for shared libraries in the executable/lib
  directory.


Manifest
--------

``--no-manifest``
  Do not generate a manifest file. Without a manifest, ``--uninstall`` is
  not available for the installation.


Uninstall mode
--------------

``--uninstall``
  Remove a previously installed package. The install name to remove may be
  given as a trailing argument or inferred from the project file name.


List mode
---------

``--list``
  List all packages recorded in the manifests directory.

``--stat``
  Used with ``--list``: display the file count, total installed size, and
  number of missing files for each package.


.. index:: pair: GPRinstall; installation layout

Installation layout
===================

The default layout under the prefix (e.g. ``/usr/local``) is:

.. code-block:: text

   <prefix>/
   ├── bin/                          # executables
   ├── lib/                          # libraries and ALI files
   ├── include/                      # sources
   └── share/gpr/
       ├── <install-name>.gpr        # generated project file
       └── manifests/
           └── <install-name>        # manifest (MD5 hash per file)

When ``--build-name`` is used, results are placed in a build-specific
subdirectory: :file:`{install-name}.{build-name}` under each subdirectory.


Project package
===============

The ``Install`` package sets installation defaults for a project.
Command-line switches take precedence over attribute values. See
:ref:`Package_Install_Attributes` in the Attributes Reference for the full
attribute list and descriptions.

.. code-block:: gpr

   library project My_Lib is
      package Install is
         for Active      use "true";
         for Mode        use "dev";
         for Artifacts ("share/doc") use ("README.md");
      end Install;
   end My_Lib;


Exit Codes
==========

``0``
  Success.

``1``
  General error (missing file, uninstall checksum mismatch, etc.).

``5``
  Project parsing error.
