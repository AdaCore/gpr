.. _Introduction:

************
Introduction
************

This User's Guide describes several software tools that use the GNAT project
facility to drive their behavior. GNAT projects are stored in text files with
the extension :samp:`.gpr`, commonly called *GPR files*.

These GPR tools use a common facility, the GNAT Project Manager, that
is fully described in :ref:`GNAT_Project_Manager`.

The main GPR tool is :samp:`GPRbuild`, a multi-language builder for systems
organized into subsystems and libraries. This tool is described in
:ref:`Building with GPRbuild<Building_with_GPRbuild>`.

The other GPR tools are described in :ref:`GPRbuild_Companion_Tools`:

* :samp:`GPRconfig`

  A configuration project file generator
  (see :ref:`Configuring with GPRconfig<Configuring_with_GPRconfig>`).

* :samp:`GPRclean`

  A tool to remove compilation artifacts created by GPRbuild
  (see :ref:`Cleaning up with GPRclean<Cleaning_up_with_GPRclean>`).

* :samp:`GPRinstall`

  Executable and library installer using GPR files
  (see :ref:`Installing with GPRinstall<Installing_with_GPRinstall>`).

* :samp:`GPRname`

  Naming scheme generator
  (see :ref:`Specifying a Naming Scheme with GPRname<Specifying_a_Naming_Scheme_with_GPRname>`).

* :samp:`GPRls`

  Library browser
  (see :ref:`The Library Browser GPRls<The_Library_Browser_GPRls>`).
