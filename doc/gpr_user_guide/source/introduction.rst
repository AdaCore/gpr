.. index:: GPR, project file

.. _UG_Introduction:

************
Introduction
************

This guide is for engineers who want to build, organize, and manage software
projects with the **GNAT Project** (GPR) system. It takes a task-oriented
approach: each chapter introduces a concept through worked examples and
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


How the tools find a toolchain
==============================

GPR tools need to know which compiler to use, what default switches to apply,
and how library files should be named. This information comes from a
**configuration project** (a ``.cgpr`` file), generated automatically by
GPRconfig the first time you build. The configuration records the compilers
found on your ``PATH`` and caches the result so subsequent builds are fast.

In most situations this is entirely transparent: run ``gprbuild -P
my_proj.gpr`` and the right compiler is used. The configuration mechanisms
only become visible when you need to target a different platform, select a
specific runtime, or work in a cross-compilation environment - topics covered
in :ref:`Working_With_Tools`.
