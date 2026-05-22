.. _RM_Knowledge_Base:

.. index:: knowledge base

**************
Knowledge Base
**************

The **knowledge base** (KB) is a collection of XML files that describe the
compilers, targets, and runtimes that GPR tools can work with. It supplies the
toolchain-specific information - compiler driver names, default switches,
dependency-file formats, library conventions, target normalization rules - that
all GPR tools need to build, install, and inspect projects.


.. index:: pair: knowledge base; embedded KB

Embedded KB
===========

In all current GPR tools, the knowledge base is **embedded directly into the
tool binary**. Every tool carries a complete, self-contained copy of the KB
and can operate with no KB files present on disk.

A reference copy of the KB files is still installed under
:file:`{<prefix>}/share/gprconfig/` alongside the tools, but it is not
consulted at run time unless explicitly requested with ``--db`` (see
`Run-time KB selection`_ below).


.. index:: pair: knowledge base; KB structure

KB structure
============

The KB is a set of XML files. Each file may contain:

- **Compiler descriptions** (``<compiler_description>``) - identify a
  compiler by executable name (which may be a regular expression), specify
  how to extract its version and target, and enumerate the languages and
  runtime variants it supports.
- **Target sets** (``<targetset>``) - group target strings under a canonical
  name used throughout the project model.
- **Fallback targets** (``<fallback_targets>``) - define chains of
  alternative targets tried when no exact match is found.
- **Configuration blocks** (``<configuration>``) - conditional GPR package
  settings (``Compiler``, ``Binder``, ``Linker``, ``Naming``, etc.) emitted
  into the generated configuration project when a matching compiler/target/
  host combination is selected.

Entity files (``.ent``) factoring out common patterns are referenced by the
XML files to avoid duplication across compiler families and platform variants.
An XSD schema file is also provided for validation.


.. index:: pair: knowledge base; run-time selection, --db

Run-time KB selection
=====================

All project-based GPR tools and GPRconfig accept the following switches to
control which KB is used at run time:

``--db`` *dir*
  Parse *dir* as an additional KB directory and merge its contents with the
  embedded KB. May be repeated. Use this to add support for a proprietary
  compiler or to override specific compiler descriptions.

``--db-``
  Disable the embedded KB entirely. Only the directories supplied via
  ``--db`` are loaded. Use this when a completely custom KB is required,
  independent of the built-in one.

With neither switch, only the embedded KB is used.


.. index:: pair: knowledge base; customizing

Customizing the KB
==================

The most common reason to supply a custom KB chunk is to add a proprietary
or in-house compiler that the standard KB does not know about. To do so:

1. Create a new ``.xml`` file following the KB schema. The installed
   reference copy under :file:`{<prefix>}/share/gprconfig/` serves as a
   guide and contains representative examples for many compiler families.
2. Place it in a dedicated directory, e.g. :file:`/opt/mycompiler/gpr/`.
3. Pass ``--db /opt/mycompiler/gpr/`` to GPRconfig and any other tool that
   needs to detect the compiler.

To replace a standard compiler description rather than extending it, combine
``--db-`` with ``--db`` to load only the custom directory.


.. index:: pair: knowledge base; validation

KB validation
=============

GPRconfig accepts ``--validate`` to check all loaded KB files against the
XSD schema before use. This is useful when developing new KB chunks:

.. code-block:: shell

   gprconfig --validate --db /opt/mycompiler/gpr/ --batch \
     --config language:ada
