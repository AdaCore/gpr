************
Introduction
************

The GPR2 library provides an extensive interface to access project files
(``.gpr`` files).

Project files are first loaded in a project tree object.

Load functions have parameters to configure load options as it is usually done
using GPRbuild on the command line with corresponding switches.

GPR project files are loaded in a project tree using
``GPR2.Project.Tree.Load_Autoconf`` or ``GPR2.Project.Tree.Load`` functions.
Use the latter if you want to use a config project file (``.cgpr``)

If your application wants to support some GPR tools standard switches (such as
``-P``, ``-X``, ``--target``, ``--RTS``, etc), a ``GPR2.Options`` package is
provided to simplify and normalize GPR standard switches handling.

Once loaded, the root project and all imported, aggregated, extended projects
including the configuration and runtime projects can be accessed.

For each projects, all variables types, variables, attributes, packages can be
evaluated, taking into account load configuration, default values and aliases.

Projects sources (``.ads``, ``.adb`` or naming conventions defined in project
files) and units (package specification, body and subunits) can be analyzed as
well. Units dependencies are also handled.
