.. index:: GPR, project file

.. _UG_Introduction:

************
Introduction
************

This guide is for engineers who want to build, organize, and manage software
projects with the **GNAT Project** (GPR) system. It takes a task-oriented
approach: each chapter introduces a concept through working examples and
explains the reasoning behind the design choices, so you can adapt the
patterns to your own projects.

If you are completely new to GPR, start with :ref:`First_Project` and work
through the chapters in order. Later chapters build on earlier ones.
Experienced users can jump directly to the topic they need; each chapter is
largely self-contained.

For a complete specification of the project file language, attributes, and
tool command-line interfaces, refer to the **GPR Reference Manual**.

To integrate GPR support into Ada tools - loading project trees or
implementing a custom incremental builder - refer to the **GPR2 Quick Start**
and the **GPR2 Library Reference**, which document the LibGPR2 Ada library.


What is GPR?
============

A GPR *project file* (``.gpr``) is a declarative description of a software
component:

- where its source files are,
- what languages they are written in,
- how they should be compiled and linked,
- what is produced (executable, library, or nothing), and
- how this component relates to other components.

GPR is **multi-language**: Ada, C, C++, and many other languages can coexist
in the same project tree, each compiled with its own toolchain and settings.

GPR is **tool-agnostic**: the same project file is used by all GPR tools -
GPRbuild to compile, GPRclean to remove build results, GPRinstall to deploy,
GPRls to inspect sources, and more. You describe the project once; the tools
share that description.

GPR is **hierarchical**: complex systems are modeled as a graph of project
files, each responsible for one component. Dependencies are expressed as
``with`` clauses - the same concept as Ada's context clauses. A build of the
root project automatically builds everything it depends on.



Attributes
==========

Every piece of information in a project file is expressed as an **attribute** -
a named property that tools read to decide what to do. Attributes cover
everything from source directories (``Source_Dirs``) and compiler switches
(``Compiler'Switches``) to library names (``Library_Name``) and installation
prefixes (``Install'Prefix``). The tools interpret those values; the project
file itself contains no executable logic.
