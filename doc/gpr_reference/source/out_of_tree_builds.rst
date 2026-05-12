.. index:: out-of-tree build

.. _RM_Out_Of_Tree_Builds:

*******************
Out-of-Tree Builds
*******************

By default, GPR tools write build artifacts (object files, libraries,
executables) to the directories declared in the project file
(``Object_Dir``, ``Library_Dir``, ``Exec_Dir``). An *out-of-tree build*
places all artifacts under a single external directory without modifying
the project files, using ``--relocate-build-tree``.

When ``--relocate-build-tree=``\ *dir* is given, each artifact directory
is mirrored under *dir*. For a project file at
``/src/workspace/app/app.gpr`` with ``Object_Dir = "obj"``, the
relocated object directory becomes *dir*\ ``/src/workspace/app/obj``.

This is useful to keep build artifacts separate from source files: to
maintain a clean version-controlled source tree, to build a read-only
source tree (third-party code, mounted file systems), or to run multiple
independent builds of the same sources in parallel without interference.

.. note::

   ``--relocate-build-tree`` has no effect on artifact directories
   declared with absolute paths in the project file. Only relative
   ``Object_Dir``, ``Library_Dir``, and ``Exec_Dir`` values are
   relocated.

When the project tree contains ``with`` clauses that reference projects
outside the main project's directory (for example
``with "../lib/lib.gpr"``), their artifact directories would by default
be relocated outside *dir*. Use ``--root-dir=``\ *root* to declare the
common ancestor of all project directories in the tree; the relocation
then maps every artifact directory to a path strictly under *dir*.

Example: a tree rooted at ``/src/workspace`` with two projects:

.. code-block:: none

   /src/workspace/
   ├── app/
   │   └── app.gpr      (with "../lib/lib.gpr")
   └── lib/
       └── lib.gpr

Building out-of-tree:

.. code-block:: shell

   $ gprbuild -P /src/workspace/app/app.gpr \
              --relocate-build-tree=/tmp/build \
              --root-dir=/src/workspace

Artifact directories are created under ``/tmp/build/app/`` and
``/tmp/build/lib/``, leaving the source tree untouched. Other GPR tools
(``gprclean``, ``gprls``, ``gprinstall``) accept the same switches and
operate on the relocated artifacts.

The ``--relocate-build-tree`` and ``--root-dir`` switches are documented
in :ref:`RM_Common_Options`.
