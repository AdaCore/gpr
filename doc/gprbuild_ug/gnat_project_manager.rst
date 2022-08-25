.. |with| replace:: :samp:`with`
.. |withs| replace:: :samp:`with`\ s
.. |withed| replace:: :samp:`with`\ ed
.. |withing| replace:: :samp:`with`\ ing
.. |limited_with| replace:: :samp:`limited with`

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _GNAT_Project_Manager:

********************
GNAT Project Manager
********************


.. _GNAT_Project_Manager_Introduction:

Introduction
============

This chapter describes GNAT's *Project Manager*, a facility that allows
you to manage complex builds involving a number of source files, directories,
and options for different system configurations. In particular,
project files allow you to specify properties including:

* The directory or set of directories containing the source files, and/or the
  names of the specific source files themselves;
* The directory in which the compiler's output
  (:file:`ALI` files, object files, tree files, etc.) is to be placed;
* The directory in which the executable programs are to be placed;
* Switch settings, which can be applied either globally or to individual
  compilation units, for any of the project-enabled tools;
* The source files containing the main subprograms to be built;
* The source programming language(s); and
* Source file naming conventions, which can be specified either globally or for
  individual compilation units (see :ref:`Naming_Schemes`).

Project files also allow you to:

* Change any of the above settings depending on external values, thus enabling
  the reuse of the projects in various **scenarios** (see :ref:`Scenarios_in_Projects`); and
* Automatically build libraries as part of the build process
  (see :ref:`Library_Projects`).

Project files are written in an Ada-like syntax, using familiar
notions such as packages, context clauses, declarations, default values,
assignments, and inheritance (see :ref:`Project_File_Reference`).

Project files can depend upon other project files in a modular fashion,
simplifying complex system integration and project reuse.

.. index:: Importing a project

* One project can **import** other projects containing needed source files.
  More generally, the Project Manager lets you structure large development
  efforts into possibly interrelated subsystems, where build decisions are
  delegated to the subsystem level, and thus different compilation
  environments (switch settings) are used for different subsystems.
  See :ref:`Organizing_Projects_into_Subsystems`.

.. index:: Project extension

* You can organize GNAT projects in a hierarchy: a project
  can **extend** a base project, inheriting its source files and
  optionally overriding any of them with alternative versions.
  See :ref:`Project_Extension`.

.. index:: -P
.. index:: -X

Several tools support project files, generally in addition to specifying
the information on the command line itself. They share common switches
to control the loading of the project (in particular
:samp:`-P{projectfile}` to define the applicable project file and
:samp:`-X{vbl}={value}` to set the value of an external variable).

The Project Manager supports a wide range of development strategies,
for systems of all sizes.  Here are some typical practices that are
easily handled:

* Using a common set of source files and generating object files in different
  directories via different switch settings. This can be used for instance to
  generate separate sets of object files for debugging and for production.

* Using a mostly shared set of source files with different versions of
  some units or subunits. This can be used for instance to group and hide
  all OS dependencies in a small number of implementation units.

Project files can be used to achieve some of the effects of a source
versioning system (for example, defining separate projects for
the different sets of sources that comprise different releases) but the
Project Manager is independent of any source configuration management tool
that might be used by the developers.

The sections below use an example-driven approach to present and illustrate
the various concepts related to projects.

.. _Building_with_Projects:

Building with Projects
======================

In its simplest form a project may be used in a stand-alone fashion to build
a single executable, and this section will focus on such a setup in order
to introduce the main ideas.
Later sections will extend this basic model to more complex and realistic
configurations.

The following concepts are the foundation of project files, and will be further
detailed later in this documentation. They are summarized here as a reference.

.. index:: Project file

**Project file**:
  A text file expressed in an Ada-like syntax, generally with the :file:`.gpr`
  extension. It defines build-related characteristics of an application.
  The characteristics include the list of sources, the location of those
  sources, the location for the generated object files, the name of
  the main program, and the options for the various tools involved in the
  build process.

.. index:: Project attribute

**Project attribute**:
  A specific project characteristic is defined by an `attribute clause`. Its
  value is a string or a sequence of strings. All settings in a project
  are defined through a list of predefined attributes with precise
  semantics. See :ref:`Attributes`.

.. index:: Packages in project files

**Package in a project**:
  Global attributes are defined at the top level of a project.
  Attributes affecting specific tools are grouped in a
  package whose name is related to tool's function. The most common
  packages are `Builder`, `Compiler`, `Binder`,
  and `Linker`. See :ref:`Packages`.

.. index:: Project variable

**Project variables**:
  In addition to attributes, a project can use variables to store intermediate
  values and avoid duplication in complex expressions. Variables can be initialized
  with external values coming from the environment.
  A frequent use of variables is to define `scenarios`.
  See :ref:`External Values <External_Values>`, :ref:`Scenarios_in_Projects`, and :ref:`Variables`.

**Source files** and **source directories**:
  A source file is associated with a language through a naming convention. For
  instance, :file:`foo.c` is typically the name of a C source file;
  :file:`bar.ads` or :file:`bar.1.ada` are two common naming conventions for a
  file containing an Ada spec. A compilable entity is often composed of a main
  source file and potentially several auxiliary ones, such as header files in C.
  The naming conventions can be user-defined (see :ref:`Naming_Schemes`), and will
  drive the builder to call the appropriate compiler for the given source file.

  Source files are searched for in the source directories associated with the
  project through the **Source_Dirs** attribute. By default, all the files (in
  these source directories) following the naming conventions associated with the
  declared languages are considered to be part of the project. It is also
  possible to limit the list of source files using the **Source_Files** or
  **Source_List_File** attributes. Note that those last two attributes only
  accept basenames with no directory information.

**Object files** and **object directory**:
  An object file is an intermediate file produced by the compiler from a
  compilation unit. It is used by post-compilation tools to produce
  final executables or libraries. Object files produced in the context of
  a given project are stored in a single directory that can be specified by the
  **Object_Dir** attribute. In order to store objects in
  two or more object directories, the system must be split into
  distinct subsystems, each with its own project file.

The following subsections introduce the attributes of interest
for simple build needs. Here is the basic setup that will be used in the
following examples:

  The Ada source files :file:`pack.ads`, :file:`pack.adb`, and
  :file:`proc.adb` are in   the :file:`common/` directory. The file
  :file:`proc.adb` contains an Ada main subprogram ``Proc`` that
  |withs| package ``Pack``. We want to compile these source files
  with the switch :samp:`-O2`, and place the resulting files in
  the :file:`common/obj/` directory. Here is the directory structure:

  ::

      common/
        pack.ads
        pack.adb
        proc.adb
      common/obj/
        proc.ali, proc.o pack.ali, pack.o, proc.exe

Our project is to be called *Build*. The name of the
file is the name of the project (case-insensitive) with the
:file:`.gpr` extension, therefore the project file name is
:file:`build.gpr`. This is not mandatory, but a warning is issued
when this convention is not followed.

This is a very simple example, and as stated above, a single project
file is sufficient. We will thus create a new file, :file:`build.gpr`, that
initially contains an empty project declaration:

  .. code-block:: gpr

        project Build is
        end Build;

Note that repeating the project name after ``end`` is mandatory.

.. _Source_Files_and_Directories:

Source Files and Directories
----------------------------

When you create a new project, the first task is to specify where the
corresponding source files are located. These are the only settings that are needed by all
the tools that will use this project (builder, compiler, binder and linker for
the compilation, IDEs to edit the source files, etc.).

.. index:: Source directories

The first step is thus to declare the source directories, which are the directories
to be searched to find source files. In the current example,
the :file:`common` directory is the only source directory.

.. index:: Source_Dirs attribute

There are several ways to specify the source directories:

* When the attribute **Source_Dirs** is not defined, a project contains a
  single source directory which is the one where the project file itself
  resides. In our example, if :file:`build.gpr` is placed in the :file:`common`
  directory, the project will have the needed implicit source directory.

* The attribute **Source_Dirs** can be set to a list of path names, one
  for each of the source directories. Such paths can either be absolute
  names (for instance :file:`"/usr/local/common/"` on Unix), or relative to the
  directory in which the project file resides (for instance :file:`"."` if
  :file:`build.gpr` is inside :file:`common/`, or :file:`"common"` if it is one level up).
  Each of the source directories must exist and be readable.

  .. index:: Portability of path names

  The syntax for directories is platform specific. For portability, however,
  the project manager will always properly translate Unix-like path names to
  the native format of the specific platform. For instance, when the same
  project file is to be used both on Unix and Windows, :file:`"/"` should be used as
  the directory separator rather than :file:`"\\"`.

* The attribute **Source_Dirs** can automatically include subdirectories
  using a special syntax inspired by some Unix shells. If any of the paths in
  the list ends with ":file:`**`", then that path and all its subdirectories
  (recursively) are included in the list of source directories. For instance,
  ":file:`**`" and ":file:`./**`" represent the complete directory tree rooted at
  the directory in which the project file resides.

.. index:: Source_Dirs attribute
.. index:: Excluded_Source_Dirs attribute

When using the ``Source_Dirs`` construct, you may sometimes find it convenient
to also use the attribute ``Excluded_Source_Dirs``, which is also a list of
paths. Each entry specifies a directory whose immediate content, not including
subdirs, is to be excluded. It is also possible to exclude a complete
directory subtree using the ``**`` notation.

.. index:: Ignore_Source_Sub_Dirs attribute

It is often desirable to remove, from the source directories, directory
subtrees rooted at some subdirectories. An example is the subdirectories
created by a Version Control System such as Subversion that creates directory
subtrees rooted at a subdirectory named :file:`.svn`. To do that, attribute
**Ignore_Source_Sub_Dirs** can be used. It specifies the list of simple
file names or patterns for the roots of these undesirable directory subtrees.

  .. code-block:: gpr

      for Source_Dirs use ("./**");
      for Ignore_Source_Sub_Dirs use (".svn", "@*");

With the declaration of attribute Ignore_Source_Sub_Dirs above, .svn subtrees
as well as subtrees rooted at subdirectories with a name starting with '@'
are not part of the source directories of the project.

When applied to the simple example, and because we generally prefer to have
the project file at the top-level directory rather than mixed with the sources,
we will add the relevant definition for the ``Source_Dirs`` attribute to
our :file:`build.gpr` project file:

  .. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");  --  <<<<
       end Build;

Once the source directories have been specified, you may need to indicate
specific source files of interest. By default, all source files present in the source
directories are considered by the Project Manager. When this is not desired,
it is possible to explicitly specify the list of sources to consider.
In such a case, only source file base names are indicated and not
their absolute or relative path names. The project manager is in charge of
locating the specified source files in the specified source directories.

* By default, the project manager searches for all source files of all
  specified languages in all the source directories.

  Since the project manager was initially developed for Ada environments, the
  default language is usually Ada and the above project file is complete: it
  defines without ambiguity the sources composing the project: that is,
  all the sources in subdirectory :file:`common` for the default language (Ada) using
  the default naming convention.

  .. index:: Languages attribute

  However, when compiling a multi-language application, or a pure C
  application, the project manager must be told which languages are of
  interest, which is done by setting the **Languages** attribute to a list of
  strings, each of which is the name of a language.

  .. index:: Naming scheme

  Even when only Ada is used, the default naming might not be suitable. Indeed,
  how does the project manager distinguish an Ada source file from any other
  file? Project files can describe the naming scheme used for source files,
  and override the default (see :ref:`Naming_Schemes`). The default is the
  standard GNAT extension (:file:`.adb` for bodies and :file:`.ads` for
  specs), which is what is used in our example, and thus no naming scheme
  is explicitly specified.
  See :ref:`Naming_Schemes`.

  .. index:: Source_Files attribute

* `Source_Files`.
  In some cases, source directories might contain files that should not be
  included in a project. One can specify the explicit list of file names to
  be considered through the **Source_Files** attribute.
  When this attribute is defined, instead of looking at every file in the
  source directories, the project manager takes only those names into
  consideration and reports  errors if they cannot be found in the source
  directories or do not correspond to the naming scheme.

* It is sometimes useful to have a project with no
  sources (most of the time because the attributes defined in the project
  file will be reused in other projects, as explained in
  :ref:`Organizing_Projects_into_Subsystems`. To do this, the attribute
  ``Source_Files`` is set to the empty list, i.e. ``()``. Alternatively,
  ``Source_Dirs`` can be set to the empty list, with the same
  result.

  .. index:: Source_List_File attribute

* `Source_List_File`.
  If there is a large number of files, it might be more convenient to use
  the attribute **Source_List_File**, which specifies the full path of a file.
  This file must contain a list of source file names (one per line, no
  directory information) that are searched as if they had been defined
  through ``Source_Files``. Such a file can easily be created through
  external tools.

  A warning is issued if both attributes ``Source_Files`` and
  ``Source_List_File`` are given explicit values. In this case, the
  attribute ``Source_Files`` prevails.

  .. index:: Excluded_Source_Files attribute
  .. index:: Locally_Removed_Files attribute
  .. index:: Excluded_Source_List_File attribute

* `Excluded_Source_Files`.
  Specifying an explicit list of files is not always convenient. Instead it might
  be preferable to use the default search rules with specific exceptions.
  This can be done through the attribute **Excluded_Source_Files**
  (or its synonym **Locally_Removed_Files**).
  Its value is the list of file names that should not be taken into account.
  This attribute is often used when extending a project,
  see :ref:`Project_Extension`. A similar attribute
  **Excluded_Source_List_File** plays the same
  role but takes the name of file containing file names similarly to
  ``Source_List_File``.

In most simple cases, such as the above example, the default source file search
behavior provides the expected result, and we do not need to add anything after
setting ``Source_Dirs``. The Project Manager automatically finds
:file:`pack.ads`, :file:`pack.adb`, and :file:`proc.adb` as source files of the
project.

Note that by default a warning is issued when a project has no sources attached
to it and this is not explicitly indicated in the project file.

.. _Duplicate_Sources_in_Projects:

Duplicate Sources in Projects
-----------------------------

If the order of the source directories is known statically, that is if
``"/**"`` is not used in the string list for ``Source_Dirs``, then there may
be several files with the same name situated in different directories of the
project. In this case, only the file in the first directory is considered as a
source of the project and the others are hidden. If ``"/**"`` is used in the
string list for ``Source_Dirs``, it is an error to have several files with the
same name in the same directory ``"/**"`` subtree, since there would be an
ambiguity as to which one should be used.

If there are two sources with the same name in different directories of the same ``"/**"``
subtree, one way to resolve the problem is to exclude the directory of the
file that should not be used as a source of the project.

.. _Object_and_Exec_Directory:

Object and Exec Directory
-------------------------

Another consideration when designing a project is to decide where the compiler should
place the object files. In fact, the compiler and other tools might create
several different kinds of files (for GNAT, there is the object file and the ALI
file). One of the important concepts in projects is that most
tools may consider source directories as read-only and thus do not attempt to create
new or temporary files there. Instead, all such files are created in the object
directory. (This is not true for project-aware IDEs, one of whose purposes is
to create the source files.)

.. index:: Object_Dir attribute
.. index:: -p (gprbuild)

The object directory is specified through the **Object_Dir** attribute.
Its value is the path to the object directory, either absolute or
relative to the directory containing the project file. This
directory must already exist and be readable and writable, although
some tools have a switch to create the directory if needed (See
the switch :samp:`-p` for *gprbuild*).

If the attribute ``Object_Dir`` is not specified, it defaults to
the directory containing the project file.

For our example, we can specify the object directory in this way
(assuming that the project file will reside in the parent directory
of :file:`common`):

  .. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "common/obj";   --  <<<<
       end Build;

As mentioned earlier, *there is a single object directory per project*. As a
result, if you have an existing system where the object files are spread across
several directories, one option is to move all of them into the same directory if
you want to build it with a single project file. An alternative approach is
described below (see :ref:`Organizing_Projects_into_Subsystems`), allowing each
separate object directory to be associated with a corresponding subsystem
of the application.

Incidentally, the directory designated by the ``Object_Dir`` attribute may
be used by project aware tools other than the compilation toolchain to store
reports or intermediate files.

.. index:: Exec_Dir attribute

When the *linker* is called, it usually creates an executable. By
default, this executable is placed in the project's object directory.
However in some situations it may be convenient to store it in elsewhere.
This can be done through the **Exec_Dir** attribute, which, like
``Object_Dir`` contains a single absolute or relative path and must point to
an existing and writable directory, unless you ask the tool to create it on
your behalf. If neither ``Object_Dir`` nor ``Exec_Dir`` is specified then
the executable is placed in the directory containing the project file.

In our example, let's specify that the executable is to
be placed in the same directory as the project file :file:`build.gpr`.
The project file is now:

  .. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";
          for Exec_Dir use ".";  --   <<<<
       end Build;

.. _Main_Subprograms:

Main Subprograms
----------------

An important role of a project file is to identify the executable(s) that
will be built. It does this by specifying the source file for the
main subprogram (for Ada) or the file that contains the ``main`` function
(for C).

There can be any number of such main files within a given project, and thus
several executables can be built from a single project file. Of
course, a given executable might not (and in general will not) need all the
source files referenced by the project. As opposed to other build mechanisms
such as through a *Makefile*, you do not need to specify the list of
dependencies of each executable. The project-aware builder knows enough of the
semantics of the languages to build and link only the necessary elements.

.. index:: Main attribute

The list of main files is specified via the **Main** attribute. It contains
a list of file names (no directories). If a file name is specified without
extension, it is completed using the naming convention defined in the package
Naming. If a project defines this
attribute, it is not necessary to identify main files on the
command line when invoking a builder, and editors like
*GPS* will be able to create extra menus to spawn or debug the
corresponding executables.

  .. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";
          for Exec_Dir use ".";
          for Main use ("proc.adb");  --   <<<<
       end Build;

If this attribute is defined in the project, then spawning the builder
with a command such as

  .. code-block:: sh

       gprbuild -Pbuild

automatically builds all the executables corresponding to the files
listed in the *Main* attribute. It is possible to specify one
or more executables on the command line to build a subset of them.

One or more spaces may be placed between the :samp:`-P` and the project
name, and the project name may be a simple name (no file extension) or
a path for the project file.  Thus each of the following is equivalent to
the command above:

  .. code-block:: sh

       gprbuild -P build
       gprbuild -P build.gpr
       gprbuild -P ./build.gpr

.. _Tools_Options_in_Project_Files:

Tools Options in Project Files
------------------------------

We now have a project file that fully describes our environment, and it can be
used to build the application with a simple *GPRbuild* command as shown
above. In fact, the empty project that we saw at
the beginning (with no attribute definitions) could already achieve this effect
if it was placed in the :file:`common` directory.

Of course, we might want more control. This section shows you how to specify
the compilation switches that the various tools involved in the building of the
executable should use.

.. index:: Command line length

Since source names and locations are described in the project file, it is not
necessary to use switches on the command line for this purpose (such
as :samp:`-I` for gcc). This removes a major source of command line length overflow.
Clearly, the builders will have to communicate this information one way or
another to the underlying compilers and tools they call, but they usually use
various text files, such as response files, for this purpose and thus are not subject
to command line overflow.

Several tools are used to create an executable: the compiler
produces object files from the source files; the binder (when the language is Ada)
creates a "source" file that, among other things,  takes care of elaboration
issues and global variable initialization; and the linker gathers everything
into a single executable. All these tools are known to
the project manager and will be invoked with user-defined switches from the
project files. To obtain this effect, a project file feature known as
a *package* is used.

.. index:: Packages in project files

A project file contains zero or more **packages**, each of which
defines the attributes specific to one tool (or one set of tools). Project
files use an Ada-like syntax for packages. Package names permitted in project
files are restricted to a predefined set (see :ref:`Packages`), and the contents
of packages are limited to a small set of constructs and attributes
(see :ref:`Attributes`).

Our example project file below includes several empty packages. At
this stage, they could all be omitted since they are empty, but they show which
packages would be involved in the build process.

  .. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";
          for Exec_Dir use ".";
          for Main use ("proc.adb");

          package Builder is  --<<<  for gprbuild
          end Builder;

          package Compiler is --<<<  for the compiler
          end Compiler;

          package Binder is   --<<<  for the binder
          end Binder;

          package Linker is   --<<<  for the linker
          end Linker;
       end Build;

Let's first examine the compiler switches. As stated in the initial description
of the example, we want to compile all files with :samp:`-O2`. This is a
compiler switch, although it is typical, on the command line, to pass it to the
builder which then passes it to the compiler. We recommend directly using
the correct package, which will make the setup easier to understand.

Several attributes can be used to specify the switches:

.. index:: Default_Switches attribute
.. index:: Indexed attribute concept

**Default_Switches**:

  This illustrates the concept of an **indexed attribute**. When
  such an attribute is defined, you must supply an *index* in the form of a
  literal string.
  In the case of *Default_Switches*, the index is the name of the
  language to which the switches apply (since a different compiler will
  likely be used for each language, and each compiler has its own set of
  switches). The value of the attribute is a list of switches.

  In this example, we want to compile all Ada source files with the switch
  :samp:`-O2`; the resulting `Compiler` package is as follows:

  .. code-block:: gpr

       package Compiler is
         for Default_Switches ("Ada") use ("-O2");
       end Compiler;

.. index:: Switches attribute

**Switches**:

  In some cases, we might want to use specific switches
  for one or more files. For instance, compiling :file:`proc.adb` might not be
  desirable at a high level of optimization.
  In such a case, the *Switches*
  attribute (indexed by the file name) can be used and will override the
  switches defined by *Default_Switches*. The *Compiler* package in our
  project file would become:

  .. code-block:: gpr

      package Compiler is
         for Default_Switches ("Ada")
             use ("-O2");
         for Switches ("proc.adb")
             use ("-O0");
      end Compiler;


  *Switches* may take a pattern as an index, such as in:

  .. code-block:: gpr

      package Compiler is
        for Default_Switches ("Ada")
            use ("-O2");
        for Switches ("pkg*")
            use ("-O0");
      end Compiler;

  Sources :file:`pkg.adb` and :file:`pkg-child.adb` would be compiled with :samp:`-O0`,
  not :samp:`-O2`.

  *Switches* can also be given a language name as index instead of a file
  name in which case it has the same semantics as *Default_Switches*.
  However, indexes with wild cards are never valid for language name.

.. index:: Local_Configuration_Pragmas attribute

**Local_Configuration_Pragmas**:

  This attribute may specify the path
  of a file containing configuration pragmas for use by the Ada compiler,
  such as `pragma Restrictions (No_Tasking)`. These pragmas will be
  used for all the sources of the project.

.. index:: Builder package
.. index:: Binder package
.. index:: Linker package

The switches for the other tools are defined in a similar manner through the
**Default_Switches** and **Switches** attributes, respectively in the
*Builder* package (for *GPRbuild*),
the *Binder* package (binding Ada executables) and the *Linker*
package (for linking executables).


.. _Compiling_with_Project_Files:

Compiling with Project Files
----------------------------

Now that our project file is written, let's build our executable.
Here is the command we would use from the command line:

  .. index:: -P (gprbuild)

  .. code-block:: sh

       gprbuild -Pbuild

This will automatically build the executables specified in the
*Main* attribute: for each, it will compile or recompile the
sources for which the object file does not exist or is not up-to-date; it
will then run the binder; and finally run the linker to create the
executable itself.

The *GPRbuild* builder can automatically manage C files the
same way: create the file :file:`utils.c` in the :file:`common` directory,
set the attribute *Languages* to `"(Ada, C)"`, and re-run

  .. code-block:: sh

      gprbuild -Pbuild

*GPRbuild* knows how to recompile the C files and will
recompile them only if one of their dependencies has changed. No direct
indication on how to build the various elements is given in the
project file, which describes the project properties rather than a
set of actions to be executed. Here is the invocation of
*GPRbuild* when building a multi-language program:

  .. code-block:: sh

      $ gprbuild -Pbuild
      gcc -c proc.adb
      gcc -c pack.adb
      gcc -c utils.c
      gprbind proc
      ...
      gcc proc.o -o proc

Notice the three steps described earlier:

* The first three gcc commands correspond to the compilation phase.
* The gprbind command corresponds to the post-compilation phase.
* The last gcc command corresponds to the final link.


.. index:: -v (gprbuild)

The default output of *GPRbuild* is reasonably simple and easy
to understand. In particular, some of the less frequently used commands are not
shown, and some parameters are abbreviated. Thus it is not possible to rerun the
effect of the *GPRbuild* command by cut-and-pasting its output.
The :samp:`-v` option to *GPRbuild* provides a much more verbose output which includes,
among other information, more complete compilation, post-compilation and link
commands.


.. _Executable_File_Names:

Executable File Names
---------------------

.. index:: Executable attribute

By default, the executable name corresponding to a main file is
computed from the main source file name. Through the attribute
**Executable** in package ``Builder``, it is possible to change
this default.

For instance, instead of building an executable named ``"proc"`` (or ``"proc.exe"``
on Windows), we could configure our project file to build ``proc1``
(respectively ``proc1.exe``) as follows:

  .. code-block:: gpr

        project Build is
           ...  --  same as before
           package Builder is
              for Executable ("proc.adb") use "proc1";
           end Builder
        end Build;

.. index:: Executable_Suffix

Attribute **Executable_Suffix**, when specified, changes the suffix
of the executable files when no attribute ``Executable`` applies:
its value replaces the platform-specific executable suffix.
The default executable suffix is the empty string empty on Unix and ``".exe"`` on Windows.

It is also possible to change the name of the produced executable by using the
command line switch :samp:`-o`. However, when several main programs are defined in the project,
it is not possible to use the :samp:`-o` switch; then the only way to change the
names of the executable is through the attributes ``Executable`` and
``Executable_Suffix``.


.. _Using_Variables_to_Avoid_Duplication:

Using Variables to Avoid Duplication
------------------------------------

To illustrate some other project capabilities, here is a slightly more complex
project using similar sources and a main program in C:

  .. code-block:: gpr

      project C_Main is
         for Languages    use ("Ada", "C");
         for Source_Dirs  use ("common");
         for Object_Dir   use  "obj";
         for Main         use ("main.c");
         package Compiler is
            C_Switches := ("-pedantic");
            for Default_Switches ("C")   use C_Switches;
            for Default_Switches ("Ada") use ("-gnaty");
            for Switches ("main.c") use C_Switches & ("-g");
         end Compiler;
      end C_Main;

This project has many similarities with the previous one.
As expected, its ``Main`` attribute now refers to a C source file.
The attribute ``Exec_Dir`` is now omitted, thus the resulting
executable will be put in the object directory :file:`obj`.

The most noticeable difference is the use of a variable in the
``Compiler`` package to store settings used in several attributes.
This avoids text duplication and eases maintenance (a single place to
modify if we want to add new switches for C files). We will later revisit
the use of variables in the context of scenarios (see :ref:`Scenarios_in_Projects`).

In this example, we see that the file :file:`main.c` will be compiled with
the switches used for all the other C files, plus :samp:`-g`.
In this specific situation the use of a variable could have been
replaced by a reference to the ``Default_Switches`` attribute:

  .. code-block:: gpr

       for Switches ("c_main.c") use Compiler'Default_Switches ("C") & ("-g");

Note the tick character "``'``", which is used to refer to attributes defined in a package.

Here is the output of the *GPRbuild* command using this project:

  .. code-block:: sh

      $ gprbuild -Pc_main
      gcc -c -pedantic -g main.c
      gcc -c -gnaty proc.adb
      gcc -c -gnaty pack.adb
      gcc -c -pedantic utils.c
      gprbind main.bexch
      ...
      gcc main.o -o main

The default switches for Ada sources,
the default switches for C sources (in the compilation of :file:`lib.c`),
and the specific switches for :file:`main.c` have all been taken into
account.

.. index:: Naming scheme
.. index:: Naming package

.. _Naming_Schemes:

Naming Schemes
--------------

Sometimes an Ada software system needs to be ported from one compilation environment to
another (such as GNAT), but the files might not be named using the default GNAT
conventions. Instead of changing all the file names, which for a variety of
reasons might not be possible, you can define the relevant file naming scheme
in the **Naming** package of your project file.

The naming scheme has two distinct goals for the Project Manager: it
allows source files to be located when searching in the source
directories, and given a source file name it makes it possible to infer
the associated language, and thus which compiler to use.

Note that the Ada compiler's use of pragma `Source_File_Name` is not
supported when using project files. You must use the features described here.
You can, however, specify other configuration pragmas.

The following attributes can be defined in package `Naming`:

.. index:: Casing attribute

**Casing**:

  Its value must be one of ``"lowercase"`` (the default if
  unspecified), ``"uppercase"`` or ``"mixedcase"``. It describes the
  casing of file names with regard to the Ada unit name.

  Given an Ada package body My_Unit, the base file name (i.e. minus the
  extension, which is controlled by other attributes described below)
  will respectively be:

  * for "lowercase": "my_unit"

  * for "uppercase": "MY_UNIT"

  * for "mixedcase": any spelling with indifferent casing such as "My_Unit",
    "MY_Unit", "My_UnIT" etc... The case insensitive name must be unique,
    otherwise an error will be reported. For example, there cannot be two
    source file names such as "My_Unit.adb" and "MY_UnIT.adb".

  On Windows, file names are case insensitive, so this attribute is
  irrelevant.

.. index:: Dot_Replacement attribute

**Dot_Replacement**:

  This attribute specifies the string that should replace the ``"."`` in unit
  names. Its default value is ``"-"`` so that a unit
  ``Parent.Child`` is expected to be found in the file
  :file:`parent-child.adb`. The replacement string must satisfy the following
  requirements to avoid ambiguities in the naming scheme:

  * It must not be empty

  * It cannot start or end with an alphanumeric character

  * It cannot be a single underscore

  * It cannot start with an underscore followed by an alphanumeric

  * It cannot contain a dot ``'.'`` unless the entire string is ``"."``

  * It cannot include a space or a character that is not printable ASCII

.. index:: Spec_Suffix attribute
.. index:: Specification_Suffix attribute

**Spec_Suffix** and **Specification_Suffix**:

  For Ada, these attributes specify the suffix used in file names that contain
  specifications. For other languages, they give the extension for files
  that contain declarations (header files in C for instance). The attribute
  is indexed by the language name.
  The two attributes are equivalent, but ``Specification_Suffix`` is obsolescent.

  If the value of the attribute is the empty string, it indicates to the
  Project Manager that the only specifications/header files for the language
  are those specified with attributes ``Spec`` or
  ``Specification_Exceptions``.

  If ``Spec_Suffix ("Ada")`` is not specified, then the default is
  ``".ads"``.

  A non empty value must satisfy the following requirements:

  * It must include at least one dot

  * If ``Dot_Replacement`` is a single dot, then it cannot include
    more than one dot.

.. index:: Body_Suffix attribute
.. index:: Implementation_Suffix attribute

**Body_Suffix** and **Implementation_Suffix**:

  These attributes are equivalent and specify the extension used for file names that contain
  code (bodies in Ada). They are indexed by the language name.
  ``Implementation_Suffix`` is obsolescent and fully replaced by the first attribute.

  For each language of a project, one of these two attributes needs to be
  specified, either in the project itself or in the configuration project file.

  If the value of the attribute is the empty string, it indicates to the
  Project Manager that the only source files for the language
  are those specified with attributes ``Body`` or
  ``Implementation_Exceptions``.

  These attributes must satisfy the same requirements as ``Spec_Suffix``.
  In addition, they must be different from any of the values in
  ``Spec_Suffix``.
  If ``Body_Suffix ("Ada")`` is not specified, then the default is
  ``".adb"``.

  If ``Body_Suffix ("Ada")`` and ``Spec_Suffix ("Ada")`` end with the
  same string, then a file name that ends with the longest of these two
  suffixes will be a body if the longest suffix is ``Body_Suffix ("Ada")``,
  or a spec if the longest suffix is ``Spec_Suffix ("Ada")``.

  If the suffix does not start with a ``'.'``, a file with a name exactly equal to
  the suffix will also be part of the project (for instance if you define the
  suffix as ``Makefile.in``, a file called :file:`Makefile.in` will be part
  of the project. This capability is usually not of interest when building.
  However, it might become useful when a project is also used to
  find the list of source files in an editor, like the GNAT Programming System
  (GPS).

.. index:: Separate_Suffix attribute

**Separate_Suffix**:

  This attribute is specific to Ada. It denotes the suffix used in file names for files
  that contain subunits (separate bodies). If it is not specified, then it defaults to
  same value as ``Body_Suffix ("Ada")``.

  The value of this attribute cannot be the empty string.

  Otherwise, the same rules apply as for the ``Body_Suffix`` attribute.


.. index:: Spec attribute
.. index:: Specification attribute

**Spec** or **Specification**:

  These attributes are equivalent.
  The ``Spec`` attribute can be used to define the source file name for a
  given Ada compilation unit's spec. The index is the literal name of the Ada
  unit (case insensitive). The value is the literal base name of the file that
  contains this unit's spec (case sensitive or insensitive depending on the
  operating system). This attribute allows the definition of exceptions to the
  general naming scheme, in case some files do not follow the usual
  convention.

  When a source file contains several units, the relative position of the unit
  can be indicated. The first unit in the file is at position 1.

    .. code-block:: gpr

         for Spec ("MyPack.MyChild") use "mypack.mychild.spec";
         for Spec ("top") use "foo.a" at 1;
         for Spec ("foo") use "foo.a" at 2;


.. index:: Body attribute
.. index:: Implementation attribute

**Body** or **Implementation**:

  These attribute play the same role as ``Spec``, but for Ada bodies.


.. index:: Specification_Exceptions attribute
.. index:: Implementation_Exceptions attribute

**Specification_Exceptions** and **Implementation_Exceptions**:

  These attributes define exceptions to the naming scheme for languages
  other than Ada. They are indexed by the language name, and contain
  a list of file names respectively for headers and source code.

As an example of several of these attributes,
the following package models the Apex file naming rules:

  .. code-block:: gpr

       package Naming is
         for Casing               use "lowercase";
         for Dot_Replacement      use ".";
         for Spec_Suffix ("Ada")  use ".1.ada";
         for Body_Suffix ("Ada")  use ".2.ada";
       end Naming;


.. _Organizing_Projects_into_Subsystems:

Organizing Projects into Subsystems
===================================

A **subsystem** is a coherent part of the complete system to be built. It is
represented by a set of sources and a single object directory. A system can
consist of a single subsystem when it is simple as we have seen in the
earlier examples. Complex systems are usually composed of several interdependent
subsystems. A subsystem is dependent on another subsystem if knowledge of the
other one is required to build it, and in particular if visibility on some of
the sources of this other subsystem is required. Each subsystem is usually
represented by its own project file.

In this section, we'll enhance the previous example. Let's assume some
sources of our ``Build`` project depend on other sources.
For instance, when building a graphical interface, it is usual to depend upon
a graphical library toolkit such as GtkAda. Furthermore, we also need
sources from a logging module we had previously written.

.. _Importing_Projects:

Importing Projects
------------------

.. index:: Importing a project

GtkAda comes with its own project file (appropriately called
:file:`gtkada.gpr`), and we will assume we have already built a project
called :file:`logging.gpr` for the logging module. With the information provided
so far in :file:`build.gpr`, building the application would fail with an error
indicating that the gtkada and logging units that are relied upon by the sources
of this project cannot be found.

This is solved by defining :file:`build.gpr` to *import* the gtkada and
logging projects: this is done by adding the following |with| clauses
at the beginning of our project:

  .. code-block:: gpr

       with "gtkada.gpr";
       with "a/b/logging.gpr";
       project Build is
         ...  --  as before
       end Build;


.. index:: Externally_Built attribute

When such a project is compiled, *gprbuild* will automatically check
the imported projects and recompile their sources when needed. It will also
recompile the sources from `Build` when needed, and finally create the
executable.

In some cases, the implementation units needed to recompile a
project are not available, or come from some third party and you do not want to
recompile it yourself. In this case, set the attribute **Externally_Built** to
:samp:`"true"`, indicating to the builder that this project can be assumed to be
up-to-date, and should not be considered for recompilation. In Ada, if the
sources of this externally built project were compiled with another version of
the compiler or with incompatible options, the binder will issue an error.

.. index:: with clause

The project's |with| clause has several effects. It provides source
visibility between projects during the compilation process. It also guarantees
that the necessary object files from ``Logging`` and ``GtkAda`` are
available when linking ``Build``.

As can be seen in this example, the syntax for importing projects is similar
to the syntax for importing compilation units in Ada. However, project files
use literal strings instead of names, and the |with| clause identifies
project files rather than packages.

Each literal string after |with| is the path
(absolute or relative) to a project file. The :file:`.gpr` extension is
optional, but we recommend adding it. If no extension is specified,
and no project file with the :file:`.gpr` extension is found, then
the file is searched for exactly as written in the |with| clause,
that is with no extension.

As mentioned above, the path after a |with| has to be a literal
string, and you cannot use concatenation, or lookup the value of external
variables to change the directories from which a project is loaded.
A solution if you need something like this is to use aggregate projects
(see :ref:`Aggregate_Projects`).

.. index:: Project path

When a relative path or a base name is used, the
project files are searched relative to each of the directories in the
**project path**. This path includes all the directories found by the
following procedure, in decreasing order of priority; the first matching
file is used:

* First, the file is searched relative to the directory that contains the
  current project file.

* Then it is searched relative to all the directories specified in the
  environment variables :envvar:`GPR_PROJECT_PATH_FILE`,
  :envvar:`GPR_PROJECT_PATH` and :envvar:`ADA_PROJECT_PATH` (in that order) if they exist.
  The value of :envvar:`GPR_PROJECT_PATH_FILE`, when defined, is the path name of
  a text file that contains project directory path names, one per line.
  :envvar:`GPR_PROJECT_PATH` and :envvar:`ADA_PROJECT_PATH`, when defined, contain
  project directory path names separated by directory separators.
  :envvar:`ADA_PROJECT_PATH` is used for compatibility, it is recommended to
  use :envvar:`GPR_PROJECT_PATH_FILE` or :envvar:`GPR_PROJECT_PATH`.

* Finally, it is searched relative to the default project directories.
  The following locations are searched, in the specified order:

  * :file:`<compiler_prefix>/<target>/<runtime>/share/gpr`
  * :file:`<compiler_prefix>/<target>/<runtime>/lib/gnat`
  * :file:`<compiler_prefix>/<target>/share/gpr`
  * :file:`<compiler_prefix>/<target>/lib/gnat`
  * :file:`<compiler_prefix>/share/gpr/`
  * :file:`<compiler_prefix>/lib/gnat/`

The first two paths are only added if the explicit runtime is specified either
via :samp:`--RTS` switch or via ``Runtime`` attribute. <target> can be
communicated via :samp:`--target` switch or ``Target`` attribute, otherwise
default target will be used. <compiler_prefix> is typically discovered
automatically based on target, runtime and language information.

In our example, :file:`gtkada.gpr` is found in the predefined directory if
it was installed at the same root as GNAT.

.. index:: -aP
.. index:: -v (gprls)

Some tools also support extending the project path from the command line,
generally through the :samp:`-aP`. You can see the value of the project
path by using the ``gprls -v`` command.

Any symbolic link will be fully resolved in the directory of the
importing project file before the imported project file is examined.

.. index:: --no-indirect-imports (gprbuild)

Any source file in the imported project can be used by the sources of the
importing project, transitively.
Thus if `A` imports `B`, which imports `C`, the sources of
`A` may depend on the sources of `C`, even if `A` does not
import `C` explicitly. However, this is not recommended, because if
and when `B` ceases to import `C`, some sources in `A` will
no longer compile. *GPRbuild* has a switch :samp:`--no-indirect-imports`
that will report such indirect dependencies.

.. index:: Project import closure

.. _Project_Import_Closure:

.. rubric:: Project import closure

The :dfn:`project import closure` for a given project `proj` is the set
of projects consisting of `proj` itself, together with each
project that is directly or indirectly imported by `proj`.
The import may be from either a |with| or, as will be explained below,
a |limited_with|.

.. note::

   One very important aspect of a project import closure is that
   **a given source can only belong to one project** in this set
   (otherwise the project manager
   would not know which settings apply to it and when to recompile it).
   Thus different project files do not usually share source directories, or,
   when they do, they need to specify precisely which project owns which sources
   using the attribute *Source_Files* or equivalent. By contrast, two projects
   can each own a source with the same base file name as long as they reside in
   different directories. The latter is not true for Ada sources because of the
   correlation between source files and Ada units.

.. index:: Cyclic project dependencies
.. index:: Limited with (project import)

.. _Cyclic_Project_Dependencies:

Cyclic Project Dependencies
---------------------------

In general, cyclic import dependencies are forbidden:
if project `A` |withs| project `B` (directly or indirectly) then `B`
is not allowed to |with| `A`. However, there are cases when cyclic
dependencies at the project level are necessary, as dependencies at
the source level may exist both ways between `A`'s sources and `B`'s sources.
For these cases, another form of import between projects is supplied:
the **limited with**.
A project `A` that imports a project `B` with a simple |with| may also be
imported, directly or indirectly, by `B` through a |limited_with|.

The difference between a simple |with| and |limited_with| is that
the name of a project imported with a |limited_with| cannot be used
in the importing project. In particular, its packages cannot be renamed and
its variables cannot be referenced.

  .. code-block:: gpr

       with "b.gpr";
       with "c.gpr";
       project A is
           for Exec_Dir use B'Exec_Dir; -- OK
       end A;

       limited with "a.gpr";   --  Cyclic dependency: A -> B -> A
       project B is
          for Exec_Dir use A'Exec_Dir; -- not OK
       end B;

       with "d.gpr";
       project C is
       end C;

       limited with "a.gpr";  --  Cyclic dependency: A -> C -> D -> A
       project D is
          for Exec_Dir use A'Exec_Dir; -- not OK
       end D;


.. _Sharing_between_Projects:

Sharing between Projects
------------------------

When building an application, it is common to have similar needs in several of
the projects corresponding to the subsystems under construction. For instance,
they might all have the same compilation switches.

As seen above (see :ref:`Tools_Options_in_Project_Files`), setting compilation
switches for all sources of a subsystem is simple: it is just a matter of
adding a ``Compiler'Default_Switches`` attribute to each project file with
the same value. However, that would entail duplication of data, and both places would need
to be changed in order to recompile the whole application with different
switches. This may be a serious issue if there are many subsystems and thus
many project files to edit.

There are two main approaches to avoiding this duplication:

* Since :file:`build.gpr` imports :file:`logging.gpr`, we could change the former
  to reference the attribute in Logging, either through a package renaming,
  or by referencing the attribute. The following example shows both cases:

  .. code-block:: gpr

      project Logging is
         package Compiler is
            for Switches ("Ada")
                use ("-O2");
         end Compiler;
         package Binder is
            for Switches ("Ada")
                use ("-E");
         end Binder;
      end Logging;

      with "logging.gpr";
      project Build is
         package Compiler renames Logging.Compiler;
         package Binder is
            for Switches ("Ada") use Logging.Binder'Switches ("Ada");
         end Binder;
      end Build;

  The solution used for `Compiler` gets the same value for all
  attributes of the package, but you cannot modify anything from the
  package (adding extra switches or some exceptions). The solution
  for the `Binder` package is more flexible, but more verbose.

  If you need to refer to the value of a variable in an imported
  project, rather than an attribute, the syntax is similar but uses
  a ``"."`` rather than an apostrophe. For instance:

  .. code-block:: gpr

      with "imported";
      project Main is
         Var1 := Imported.Var;
      end Main;

* The second approach is to define the switches in a separate project.
  That project does not contain any source files (thus, as opposed to
  the first example, none of the projects plays a special role), and
  will only be used to define the attributes. Such a project is
  typically named :file:`shared.gpr`.

  .. code-block:: gpr

      abstract project Shared is
         for Source_Files use ();   --  no sources
         package Compiler is
            for Switches ("Ada")
                use ("-O2");
         end Compiler;
      end Shared;

      with "shared.gpr";
      project Logging is
         package Compiler renames Shared.Compiler;
      end Logging;

      with "shared.gpr";
      project Build is
         package Compiler renames Shared.Compiler;
      end Build;

  As with the first example, we could have chosen to set the attributes
  one by one rather than to rename a package. The reason we explicitly
  indicate that `Shared` has no sources is so that it can be created
  in any directory, and we are sure it shares no sources with `Build`
  or `Logging`, which would be invalid.

  .. index:: Project qualifier
  .. index:: abstract project qualifier

  Note the additional use of the **abstract** qualifier in :file:`shared.gpr`.
  This qualifier is optional, but helps convey the message that we do not
  intend this project to have source files (see :ref:`Qualified_Projects` for
  additional information about project qualifiers).

.. index:: Global attribute

.. _Global_Attributes:

Global Attributes
-----------------

We have already seen many examples of attributes used to specify a particular
option for one of the tools involved in the build process. Most of those
attributes are project specific. That is to say, they only affect the invocation
of tools on the sources of the project where they are defined.

.. index:: Main project

There are a few additional attributes that, when defined for a "main" project
`proj`, also apply to all other projects in the project import closure of `proj`.
A :dfn:`main project` is a project explicitly specified on the command line.

Such attributes are known as :dfn:`global attributes`;
here are several that are commonly used:

.. index:: Global_Configuration_Pragmas attribute

**Builder'Global_Configuration_Pragmas**:

  This attribute specifies a file that contains configuration pragmas
  to use when building executables. These pragmas apply to all
  executables built from this project import closure. As noted earlier,
  additional pragmas can be specified on a per-project basis by setting the
  ``Compiler'Local_Configuration_Pragmas`` attribute.

.. index:: Global_Compilation_Switches attribute

**Builder'Global_Compilation_Switches**:

  This attribute is a list of compiler switches that apply when compiling any
  source file in the project import closure. These switches are used in addition
  to the ones defined in the ``Compiler`` package, which only apply to
  the sources of the corresponding project. This attribute is indexed by
  the name of the language.

Using such global capabilities is convenient, but care is needed since
it can also lead to unexpected
behavior. An example is when several subsystems are shared among different main
projects but the different global attributes are not
compatible. Note that using aggregate projects can be a safer and more powerful
alternative to global attributes.


.. index Scenarios

.. _Scenarios_in_Projects:

Scenarios in Projects
=====================

Various project properties can be modified based on **scenarios**. These
are user-defined modes (the values of project variables and attributes)
that determine the behavior of a project, based on the
values of externally defined variables. Typical
examples are the setup of platform-specific compiler options, or the use of
a debug and a release mode (the former would activate the generation of debug
information, while the latter would request an increased level of code
optimization).

Let's enhance our example to support debug and release modes. The issue is to
let the user choose which kind of system to build: use :samp:`-g` as a
compiler switch in debug mode and :samp:`-O2` in release mode. We will also
set up the projects so that we do not share the same object directory in both
modes; otherwise switching from one to the other might trigger more
recompilations than needed or mix objects from the two modes.

One approach is to create two different project files, say
:file:`build_debug.gpr` and :file:`build_release.gpr`, that set the appropriate
attributes as explained in previous sections. This solution does not scale
well, because in the presence of multiple projects depending on each other, you
will also have to duplicate the complete set of projects and adapt the project
files accordingly.

.. index:: External variable

Instead, project files support the notion of scenarios controlled
by the values of externally defined variables.
Such values can come from several sources (in decreasing
order of priority):

.. index:: -X

**Command line**:
  When launching *gprbuild*, the user can pass
  :samp:`-X` switches to define the external variables. In
  our case, the command line might look like

  .. code-block:: sh

         gprbuild -Pbuild.gpr -Xmode=release

  which defines the external variable named :samp:`mode` and sets its value to
  :samp:`"release"`.

.. index:: Environment variable in scenarios

**Environment variables**:
  When the external value does not come from the command line, it can come from
  the value of an environment variable of the appropriate name.
  In our case, if an environment variable named :samp:`mode`
  exists, its value will be used.


.. index:: external function

**Tool mode**:
  In the special case of the ``GPR_TOOL`` variable, if its value has not been
  specified via the command line or as an environment variable, the various
  tools set this variable to a value proper to each tool. gprbuild sets this
  value to ``gprbuild``. See the documentation of other tools to find out which
  value they set this variable to.

**External function second parameter**.
  Once an external variable is defined, its value needs to be obtained by
  the project. The general form is to use
  the predefined function :samp:`external`, which returns the current value of
  the external variable. For instance, we could set up the object directory to point to
  either :file:`obj/debug` or :file:`obj/release` by changing our project to

  .. code-block:: gpr

       project Build is
           for Object_Dir use "obj/" & external ("mode", "debug");
           ... --  as before
       end Build;

  The second parameter to :samp:`external` is optional, and is the default
  value to use if :samp:`mode` is not set from the command line or the environment.
  If the second parameter is not supplied, and there is no external or
  environment variable named by the first parameter, then an error is reported.

In order to set the switches according to the different scenarios, other
constructs are needed, such as typed variables and case constructions.

.. index:: Typed variable
.. index:: Case construction

A **typed variable** is a variable that
can take only a limited number of values, similar to variable from an
enumeration type in Ada.
Such a variable can then be used in a **case construction**, resulting in
conditional sections in the project. The following example shows how
this can be done:

  .. code-block:: gpr

       project Build is
          type Mode_Type is ("debug", "release");         -- all possible values
          Mode : Mode_Type := external ("mode", "debug"); -- a typed variable

          package Compiler is
             case Mode is
                when "debug" =>
                   for Switches ("Ada")
                       use ("-g");
                when "release" =>
                   for Switches ("Ada")
                       use ("-O2");
             end case;
          end Compiler;
       end Build;

This project is larger than the ones we have seen previously,
but it has become much more flexible.
The :samp:`Mode_Type` type defines the only valid values for the
:samp:`Mode` variable. If
any other value is read from the environment, an error is reported and the
project is considered as invalid.

The ``Mode`` variable is initialized with an external value
defaulting to :samp:`"debug"`. This default could be omitted and that would
force the user to define the value. Finally, we can use a case construction to set the
switches depending on the scenario the user has chosen.

Most aspects of a project can depend on scenarios. The notable exception
is the identity of an imported project (via a |with| or |limited_with|
clause), which cannot depend on a scenario.

Scenarios work analogously across projects in a project import closure.
You can either
duplicate a variable similar to :samp:`Mode` in each of the projects (as long
as the first argument to :samp:`external` is always the same and the type is
the same), or simply set the variable in the :file:`shared.gpr` project
(see :ref:`Sharing_Between_Projects`).

.. index:: Library project

.. _Library_Projects:

Library Projects
================

So far, we have seen examples of projects that create executables. However,
it is also possible to create libraries instead. A **library** is a specific
type of subsystem where, for convenience, objects are grouped together
using system-specific means such as archives or Windows DLLs.

Library projects provide a system- and language-independent way of building
both **static** and **dynamic** libraries. They also support the concept of
**standalone libraries** (SAL) which offer two significant properties: the
elaboration (e.g. initialization) of the library is either automatic or
very simple; a change in the
implementation part of the library implies minimal post-compilation actions on
the complete system and potentially no action at all for the rest of the
system in the case of dynamic SALs.

There is a restriction on shared library projects: by default, they are only
allowed to import other shared library projects. They are not allowed to
import non-library projects or static library projects.

The GNAT Project Manager takes complete care of the library build, rebuild and
installation tasks, including recompilation of the source files for which
objects do not exist or are not up to date, assembly of the library archive, and
installation of the library (i.e., copying associated source, object and
:file:`ALI` files to the specified location).


.. _Building_Libraries:

Building Libraries
------------------

Let's enhance our example and transform the `logging` subsystem into a
library.  In order to do so, a few changes need to be made to
:file:`logging.gpr`.  Some attributes need to be defined: at least
`Library_Name` and `Library_Dir`; in addition, some other attributes
can be used to specify specific aspects of the library. For readability, it is
also recommended (although not mandatory), to use the qualifier `library`
in front of the `project` keyword.

.. index:: Library_Name attribute

**Library_Name**:

  This attribute is the name of the library to be built. There is no
  restriction on the name of a library imposed by the project manager, except
  for stand-alone libraries whose names must follow the syntax of Ada
  identifiers; however, there may be system-specific restrictions on the name.
  In general, we recommend using only alphanumeric characters (and
  possibly single underscores), to help portability.

.. index:: Library_Dir attribute

**Library_Dir**:

  This attribute  is the path (absolute or relative) of the directory where
  the library is to be installed. In the process of building a library,
  the sources are compiled and the object files are placed in the explicitly- or
  implicitly specified :file:`Object_Dir` directory. When all sources of a library
  are compiled, some of the compilation artifacts, including the library itself,
  are copied to the library_dir directory. This directory must exist and be
  writable. It must also be different from the object directory so that cleanup
  activities in the Library_Dir do not affect recompilation needs.

Here is the new version of :file:`logging.gpr` that makes it a library:

  .. code-block:: gpr

       library project Logging is          --  "library" is optional
          for Library_Name use "logging";  --  will create "liblogging.a" on Unix
          for Object_Dir   use "obj";
          for Library_Dir  use "lib";      --  different from object_dir
       end Logging;

Once the above two attributes are defined, the library project is valid and
is sufficient for building a library with default characteristics.
Other library-related attributes can be used to change the defaults:

.. index:: Library_Kind attribute

**Library_Kind**:

  The value of this attribute must be either :samp:`"static"`,
  :samp:`"static-pic"`, :samp:`"dynamic"` or :samp:`"relocatable"` (the last is
  a synonym for :samp:`"dynamic"`). It indicates which kind of library should
  be built (the default is to build a static library, that is an archive of
  object files that can potentially be linked into a static executable). A
  static-pic library is also an archive, but the code is Position Independent
  Code, usually compiled with the switch -fPIC. When the library is set to
  be dynamic, a separate image is created that will be loaded independently,
  usually at the start of the main program execution. Support for dynamic
  libraries is very platform specific, for instance on Windows it takes the
  form of a DLL while on GNU/Linux, it is a dynamic `elf` image whose suffix is
  usually :file:`.so`. Library project files, on the other hand, can be written
  in a platform independent way so that the same project file can be used to
  build a library on different operating systems.

  If you need to build both a static and a dynamic library, we recommend
  using two different object directories, since in some cases some extra code
  needs to be generated for the latter. For such cases, one can either define
  two different project files, or a single one that uses scenarios to indicate
  the various kinds of library to be built and their corresponding object_dir.

.. index:: Library_ALI_Dir attribute

**Library_ALI_Dir**:

  This attribute may be specified to indicate the directory where the ALI
  files of the library are installed. By default, they are copied into the
  :file:`Library_Dir` directory, but as for the executables where we have a
  separate `Exec_Dir` attribute, you might want to put them in a separate
  directory since there may be hundreds of such files. The same restrictions as for
  the :samp:`Library_Dir` attribute apply.

.. index:: Library_Version attribute

**Library_Version**:

  This attribute is platform dependent, and has no effect on Windows.
  On Unix, it is used only for dynamic libraries as the internal
  name of the library (the "soname"). If the library file name (built
  from the ``Library_Name``) is different from the ``Library_Version``,
  then the library file will be a symbolic link to the actual file whose name
  will be ``Library_Version``. This follows the usual installation schemes
  for dynamic libraries on many Unix systems.

  .. code-block:: gpr

      project Logging is
         Version := "1";
         for Library_Dir use "lib";
         for Library_Name use "logging";
         for Library_Kind use "dynamic";
         for Library_Version use "liblogging.so." & Version;
      end Logging;


  After the compilation, the directory :file:`lib` will contain both a
  :file:`liblogging.so.1` library and a symbolic link to it called
  :file:`liblogging.so`.

.. index:: Library_GCC attribute

**Library_GCC**:

  This attribute is the name of the tool to use instead of ``gcc`` to link shared
  libraries. A common use of this attribute is to define a wrapper script that
  accomplishes specific actions before calling ``gcc`` (which itself calls the
  linker to build the library image).

.. index:: Library_Options attribute

**Library_Options**:

  This attribute may be used to specify additional switches ("last switches")
  when linking a shared library or a static standalone library.
  In the case of a simple static library, the values for this attribute are
  restricted to paths to object files. Those paths may be absolute or relative
  to the object directory.

.. index:: Leading_Library_Options attribute

**Leading_Library_Options**:

  This attribute, which is taken into account only by *GPRbuild*, may be
  used to specify leading options ("first switches") when linking a shared
  library.

.. _Using_Library_Projects:

Using Library Projects
----------------------

When the builder detects that a project file is a library project file, it
recompiles all sources of the project that need recompilation and rebuilds the
library if any of the sources have been recompiled. It then groups all object
files into a single file, which is a shared or a static library. This library
can later on be linked with multiple executables. Note that the use
of shared libraries reduces the size of the final executable and can also reduce
the memory footprint at execution time when the library is shared among several
executables.

*GPRbuild* also allows building **multi-language libraries** when specifying
sources from multiple languages.

A non-library project `NLP` can import a library project `LP`. When the builder
is invoked on `NLP`, it always rebuilds `LP` even if all of the latter's files are
up to date. For instance, let's assume in our example that ``logging`` has
the following sources: :file:`log1.ads`, :file:`log1.adb`, :file:`log2.ads` and
:file:`log2.adb`. If :file:`log1.adb` has been modified, then the library
:file:`liblogging` will be rebuilt when compiling all the sources of
``Build`` even if :file:`proc.ads`, :file:`pack.ads` and :file:`pack.adb`
do not include a ``"with Log1"``.

To ensure that all the sources in the ``Logging`` library are
up to date, and that all the sources of ``Build`` are also up to date,
the following two commands need to be used:

  .. code-block:: sh

       gprbuild -Plogging.gpr
       gprbuild -Pbuild.gpr

All :file:`ALI` files will also be copied from the object directory to the
library directory. To build executables, *GPRbuild* will use the
library rather than the individual object files.

.. index:: Externally_Built attribute

Library projects can also be useful to specify a library that needs to be used
but, for some reason, cannot be rebuilt. Such a situation may arise when some
of the library sources are not available. Such library projects need to use the
``Externally_Built`` attribute as in the example below:

  .. code-block:: gpr

       library project Extern_Lib is
          for Languages    use ("Ada", "C");
          for Source_Dirs  use ("lib_src");
          for Library_Dir  use "lib2";
          for Library_Kind use "dynamic";
          for Library_Name use "l2";
          for Externally_Built use "true";  --  <<<<
       end Extern_Lib;

In the case of externally built libraries, the ``Object_Dir``
attribute does not need to be specified because it will never be
used.

The main effect of using such an externally built library project is mostly to
affect the linker command in order to reference the desired library. It can
also be achieved by using ``Linker'Linker_Options`` or ``Linker'Switches``
in the project corresponding to the subsystem needing this external library.
This latter method is more straightforward in simple cases but when several
subsystems depend upon the same external library, finding the proper place
for the ``Linker'Linker_Options`` might not be easy and if it is
not placed properly, the final link command is likely to present ordering issues.
In such a situation, it is better to use the externally built library project
so that all other subsystems depending on it can declare this dependency through
a project |with| clause, which in turn will trigger the builder to find
the proper order of libraries in the final link command.


.. _Stand-alone_Library_Projects:

Stand-alone Library Projects
----------------------------

.. index:: Stand-alone libraries

A **stand-alone library** is a library that contains the necessary code to
elaborate the Ada units that are included in the library. A stand-alone
library is a convenient way to add an Ada subsystem to a more global system
whose main is not in Ada since it makes the elaboration of the Ada part mostly
transparent. However, stand-alone libraries are also useful when the main is in
Ada: they provide a means for minimizing relinking and redeployment of complex
systems when localized changes are made.

The name of a stand-alone library, specified with attribute
``Library_Name``, must have the syntax of an Ada identifier.

The most prominent characteristic of a stand-alone library is that it offers a
distinction between interface units and implementation units. Only the former
are visible to units outside the library. Thus to define a stand-alone library
project, one extra attribute, either ``Interfaces`` or ``Library_Interface``,
must be specified in addition to the two attributes that make a project a
Library Project (``Library_Name`` and ``Library_Dir``). ``Interfaces`` defines
the list of sources of the library, in any language, that should be considered
as library's interface; ``Library_Interface`` is an Ada-specific alternative
that defines the list of units rather than their containing sources.

.. index:: Library_Interface attribute

**Library_Interface**:

  This attribute defines an explicit subset of the units of the project. Units
  from projects importing this library project may only "with" units whose
  sources are listed in the `Library_Interface`. Other sources are
  considered implementation units.

  .. code-block:: gpr

     for Library_Dir use "lib";
     for Library_Name use "logging";
     for Library_Interface use ("lib1", "lib2");  --  unit names

.. index:: Interfaces attribute

**Interfaces**

  This attribute defines an explicit subset of the source files of a project.
  Sources from projects importing this project, can only depend on sources from
  this subset. This attribute can be used on non library projects. It can also
  be used as a replacement for attribute ``Library_Interface``, in which
  case, units have to be replaced by source files. For multi-language library
  projects, it is the only way to make the project a Stand-Alone Library project
  whose interface is not purely Ada.


.. index:: Library_Standalone attribute

**Library_Standalone**:

  This attribute defines the kind of stand-alone library to
  build. Values are either ``standard`` (the default), ``no`` or
  ``encapsulated``. When ``standard`` is used the code to elaborate and
  finalize the library is embedded, when ``encapsulated`` is used the
  library can furthermore depend only on static libraries (including
  the GNAT runtime). This attribute can be set to ``no`` to make it clear
  that the library should not be stand-alone in which case attributes
  ``Library_Interface`` or ``Interfaces`` should not be defined.

  .. code-block:: gpr

     for Library_Dir use "lib";
     for Library_Name use "logging";
     for Library_Kind use "dynamic";
     for Library_Interface use ("lib1", "lib2");  --  unit names
     for Library_Standalone use "encapsulated";

In order to include the elaboration code in the stand-alone library, the binder
is invoked on the closure of the library units creating a package whose name
depends on the library name (:file:`b~logging.ads/b` in the example).
This binder-generated package includes **initialization** and **finalization**
procedures whose names depend on the library name (``logginginit`` and
``loggingfinal`` in the example). The object corresponding to this package is
included in the library.

.. index:: Library_Auto_Init attribute

**Library_Auto_Init**:

  A dynamic stand-alone Library is automatically initialized
  if automatic initialization of stand-alone Libraries is supported on the
  platform and if attribute ``Library_Auto_Init`` is not specified or
  is specified with the value ``"true"``.
  Whether a static stand-alone Library is automatically initialized
  is platform dependent. Specifying ``"false"`` for the ``Library_Auto_Init``
  attribute prevents automatic initialization.

  When a non-automatically initialized stand-alone library is used in an
  executable, its initialization procedure must be called before any service of
  the library is used. When the main subprogram is in Ada, it may mean that the
  initialization procedure has to be called during elaboration of another
  package.


.. index:: Library_Dir attribute

**Library_Dir**:

  For a stand-alone library, only the :file:`ALI` files of the interface units
  (those that are listed in attribute `Library_Interface`) are copied to
  the library directory. As a consequence, only the interface units may be
  imported from Ada units outside of the library. If other units are imported,
  the binding phase will fail.


.. index:: Binder'Default_Switches attribute
.. index:: Default_Switches attribute

**Binder'Default_Switches**:

  When a stand-alone library is bound, the switches that are specified in
  the attribute ``Binder'Default_Switches ("Ada")`` are
  used in the call to *gnatbind*.


.. index:: Library_Src_Dir attribute

**Library_Src_Dir**:

  This attribute defines the location (absolute or relative to the project
  directory) where the sources of the interface units are copied at
  installation time.
  These sources includes the specs of the interface units along with the
  closure of sources necessary to compile them successfully. That may include
  bodies and subunits, when pragmas `Inline` are used, or when there are
  generic units in specs. This directory cannot point to the object directory
  or one of the source directories, but it can point to the library directory,
  which is the default value for this attribute.


.. index:: Library_Symbol_Policy attribute

**Library_Symbol_Policy**:

  This attribute controls the export of symbols on some platforms (like
  Windows, GNU/Linux). It is not supported on all platforms (where it
  will just have no effect). It may have one of the
  following values:

  * ``"restricted"``: The exported symbols will be restricted to the one
    from the interface of the stand-alone library. This is either computed
    automatically or using the ``Library_Symbol_File`` if specified.

  * ``"unrestricted"``: All symbols from the stand-alone library are exported.


.. index:: Library_Symbol_File attribute

**Library_Symbol_File**

  This attribute may define the name of the symbol file to be used when
  building a stand-alone library when the symbol policy is
  ``"restricted"``, on platforms that support symbol control. This file
  must contain one symbol per line and only those symbols will be
  exported from the stand-alone library.


.. _Installing_a_Library_with_Project_Files:

Installing a Library with Project Files
---------------------------------------

When using project files, a usable version of the library is created in the
directory specified by the ``Library_Dir`` attribute of the library
project file. Thus no further action is needed in order to make use of
the libraries that are built as part of the general application build.

You may want to install a library in a context different from where the library
is built. This situation arises with third party suppliers, who may want
to distribute a library in binary form where the user is not expected to be
able to recompile the library. The simplest option in this case is to provide
a project file slightly different from the one used to build the library, by
using the ``Externally_Built`` attribute. See :ref:`Using_Library_Projects`.

.. index:: gprinstall tool

Another option is to use *gprinstall* to install the library in a
different context than the build location. The *gprinstall* tool automatically
generates a project to use this library, and also copies the minimum set of
sources needed to use the library to the install location.
See :ref:`Package_Install_Attributes`.

.. index:: Project extension
.. index:: Extending a project

.. _Project_Extension:

Project Extension
=================

During development of a large system, it is sometimes necessary to use
modified versions of some of the source files, without changing the original
sources. This can be achieved through the *project extension* facility.

Suppose that our example ``Build`` project is built every night for the whole
team, in some shared directory. A developer usually needs to work on a small
part of the system, and might not want to have a copy of all the sources and
all the object files since that could require too much disk space and too
much time to recompile everything. A better approach is to override some of
the source files in a separate directory, while still using the object files
generated at night for the non-overridden shared sources.

Another use case is a large software system with multiple implementations of
a common interface; in Ada terms, multiple versions of a package body for the
same spec, or perhaps different versions of a package spec that have the same
visible part but different private parts. For example, one package might be
safe for use in tasking programs, while another might be used only in
sequential applications.

A third example is different versions of the same system. For instance,
assume that a ``Common`` project is used by two development branches. One of
the branches has now been frozen, and no further change can be done to it or
to ``Common``. However, on the other development branch the sources in ``Common``
are still evolving. A new version of the subsystem is needed, which reuses
as much as possible from the original.

.. index:: Base project

Each of these can be implemented in GNAT using **project extension**:

  If one project *extends* another project (the *base project*) then by default
  all source files of the base project are inherited by the extending project,
  but the latter can override any of the base project's source files with a
  new version, and can also add new files or remove unnecessary ones. A project
  can extend at most one base project.

This facility is somewhat analogous to class extension (with single
inheritance) in object-oriented programming. Project extension hierarchies
are permitted (an extending project may itself serve as a base project and
be extended), and a project that extends a project can also import other projects.

An extending project implicitly inherits all the sources and objects from
its base project. It is possible to create a new version of some of
the sources in one of the additional source directories of the extending
project. Those new versions hide the original versions. As noted above,
adding new sources or removing existing ones is also possible. Here is an
example of how to extend the project `Build` from previous examples:

  .. code-block:: gpr

       project Work extends "../bld/build.gpr" is
       end Work;

The project after the ``extends`` keyword is the base project being extended.
As usual, it can be specified using an absolute path, or a path relative
to any of the directories in the project path. The ``Work`` project does not
specify source or object directories, so the default values for these
attributes will be used; that is, the current directory (where project
``Work`` is placed). We can compile that project with

  .. code-block:: sh

       gprbuild -Pwork

If no sources have been placed in the current directory, this command
has no effect, since this project does not change the
sources it inherited from ``Build`` and thus all the object files
in ``Build`` and its dependencies are still valid and are reused
automatically.

Suppose we now want to supply an alternative version of :file:`pack.adb`
but use the existing versions of :file:`pack.ads` and :file:`proc.adb`.
We can create the new file in the ``Work`` project's directory (for example by
copying the one from the ``Build`` project and making changes to it).
If new packages are needed at the same time, we simply create new files
in the source directory of the extending project.

When we recompile, *GPRbuild* will now automatically recompile
this file (thus creating :file:`pack.o` in the current directory) and
any file that depends on it (thus creating :file:`proc.o`). Finally, the
executable is also linked locally.

Note that we could have obtained the desired behavior using project import
rather than project inheritance. Some project ``proj`` would contain
the sources for :file:`pack.ads` and :file:`proc.adb`, and ``Work`` would
import ``proj`` and add :file:`pack.adb`. In this situation  ``proj``
cannot contain the original version of :file:`pack.adb` since otherwise
two versions of the same unit would be in project import closure of ``proj``,
which is not allowed. In general we do not recommended placing the spec and
body of a unit in different projects, since this affects their autonomy and
reusability.

In a project file that extends another project, it is possible to
indicate that an inherited source is **not part** of the sources of the
extending project. This is necessary, for example, when a package spec has
been overridden in such a way that a body is forbidden. In this case, it is
necessary to indicate that the inherited body is not part of the sources
of the project, otherwise there will be a compilation error.

.. index:: Excluded_Source_Files attribute
.. index:: Excluded_Source_List_File attribute

Two attributes are available for this purpose:

* **Excluded_Source_Files**, whose value is a list of file names, and

* **Excluded_Source_List_File**, whose value is the path of a text file
  containing one file name per line.

  .. code-block:: gpr

       project Work extends "../bld/build.gpr" is
          for Source_Files use ("pack.ads");
          --  New spec of Pkg does not need a completion
          for Excluded_Source_Files use ("pack.adb");
       end Work;


All tool packages that are not declared in the extending project are inherited from
the base project, with their attributes, with the exception of
``Linker'Linker_Options`` which is never inherited. In particular, an
extending project retains all the switches specified in its base project.

At the project level, if they are not declared in the extending project, some
attributes are inherited from the base project. They are:
``Languages``, ``Main`` (for a root non library project) and
``Library_Name`` (for a project extending a library project).

.. _Importing_and_Project_Extension:

Importing and Project Extension
-------------------------------

One of the fundamental restrictions for project extension is the following:

  **A project is not allowed to import, directly or indirectly, both an extending
  project P and also some project that P extends either directly or
  indirectly**

In the absence of this rule, two imports might access different versions of the
same source file, or different sets of tool switches for the same source file
(one from the base project and the other from an extending project).

As an example of this problem, consider the following set of project files:

* :file:`a.gpr` which contains the source files :file:`foo.ads` and
  :file:`foo.adb`, among others

* :file:`b.gpr` which imports :file:`a.gpr` (one of its source files
  |withs| ``foo``)

* :file:`c.gpr` which imports :file:`b.gpr`

Suppose we want to extend the projects as follows:

* :file:`a_ext.gpr` extends :file:`a.gpr` and overrides :file:`foo.adb`

* :file:`c_ext.gpr` extends :file:`c.gpr`, overriding one of its source files

Since :file:`c_ext.gpr` needs to access sources in :file:`b.gpr`, it will
import :file:`b.gpr`

Finally, :file:`main.gpr` needs to access the overridden source files in
:file:`a_ext.gpr` and :file:`c_ext.gpr` and thus will import these two
projects.

.. only:: html or latex

   This project structure is shown in :numref:`fig-badproject`.

   .. _fig-badproject:

   .. figure:: importing_and_project_extension_figure_1.png
      :figwidth: image
      :align: center

      Example of Source File Ambiguity from `imports`/`extends` Violation


.. only:: not (html or latex)

   This project structure is as follows:

   ::

                +-imports--> a_ext.gpr ---extends-----> a.gpr
                |                                         ^
                |                                         |
                |                                         |imports
                |                                         |
             main.gpr            +--------imports-----> b.gpr
                |                |                        ^
                |                |                        |
                |                |                        |imports
                |                |                        |
                +-imports--> c_ext.gpr-----extends----> c.gpr

This violates the restriction above, since :file:`main.gpr` imports the extending project
:file:`a_ext.gpr` and also (indirectly through :file:`c_ext.gpr` and :file:`b.gpr`)
the project :file:`a.gpr` that :file:`a_ext.gpr` extends.
The problem is that the import path through :file:`c_ext.gpr` and :file:`b.gpr` would build with the version
of :file:`foo.adb` from :file:`a.gpr`, whereas the import path through :file:`a_ext.gpr` would use that project's
version of :file:`foo.adb`.
The error will be detected and reported by ``gprbuild``.

A solution is to introduce an "empty" extension of :file:`b.gpr`, which is imported by
:file:`c_ext.gpr` and imports :file:`a_ext.gpr`:

  .. code-block:: gpr

       with "a_ext.gpr";
       project B_Ext extends "b.gpr" is
       end B_Ext;

.. only:: html or latex

   This project structure is shown in :numref:`fig-goodproject`.

   .. _fig-goodproject:

   .. figure:: importing_and_project_extension_figure_2.png
      :figwidth: image
      :align: center

      Using "Empty" Project Extension to Avoid `imports`/`extends` Violation

.. only:: not (html or latex)

   This project structure is as follows:

   ::

               +-imports--> a_ext.gpr ---extends-----> a.gpr
               |              ^                         ^
               |              |                         |
               |              |imports                  |imports
               |              |                         |
            main.gpr        b_ext.gpr-----extends----> b.gpr
               |              ^                         ^
               |              |                         |
               |              |imports                  |imports
               |              |                         |
               +-imports--> c_ext.gpr-----extends----> c.gpr

There is now no ambiguity over which version of :file:`foo.adb` to use;
it will be the one from :file:`a_ext.gpr`.

.. index:: extends all

When extending a large system spanning multiple projects, it is often
inconvenient to extend every project in the project import closure that
is impacted by a small change introduced in a low layer. In such cases,
it is possible to create an **implicit extension** of an entire hierarchy
using the **extends all** relationship.

When a project ``P`` is extended using `extends all` inheritance, all projects
that are imported by ``P``, both directly and indirectly, are considered virtually
extended. That is, the project manager creates implicit projects
that extend every project in the project import closure; all these implicit
projects do not control sources on their own and use the object directory of
the `extends all` project.

It is possible to explicitly extend one or more projects in the import closure
in order to adapt the sources. These extending projects must be imported by
the ``extends all`` project, which will replace the corresponding virtual
projects with the explicit ones.

When building such a project closure extension, the project manager will
ensure recompilation of both the modified sources and the sources in implicit
extending projects that depend on them.

To illustrate the ``extends all`` feature, here's a slight variation on the
earlier examples.  We have a ``Main`` project that imports project ``C``,
which imports ``B``, which imports ``A``. The source files in ``Main`` refer
to compilation units whose sources are in ``C`` and ``A``.  (Recall that
``imports`` is transitive, so ``A`` is implicitly accessible in ``Main``.)

.. only:: html or latex

   This project structure is shown in :numref:`fig-simpleproject`.

   .. _fig-simpleproject:

   .. figure:: importing_and_project_extension_figure_3.png
      :figwidth: image
      :align: center

      Simple Project Structure before Extension

.. only:: not (html or latex)

   Thus:

     ::

          a.gpr
           ^
           |
           |imports
           |
          b.gpr
           ^
           |
           |imports
           |
          c.gpr
           ^
           |
           |imports
           |
         main.gpr

Suppose that we want to extend :file:`a.gpr`, overriding one of
its source files, and create a new version of :file:`main.gpr`
that can access the overridden file in the extending project
:file:`a_ext.gpr` and otherwise use the sources in :file:`b.gpr`
and :file:`c.gpr`.

Instead of explicitly defining  empty projects to extend :file:`b.gpr` and
:file:`c.gpr`, we can create a new project :file:`main_ext.gpr` that does an
``extends all`` of :file:`main.gpr` and imports :file:`a_ext.gpr`. The
``extends_all`` will implicitly create the empty projects :file:`b_ext.gpr`
and :file:`c_ext.gpr` as well as the relevant import relationships:

* :file:`c_ext.gpr` will import
  :file:`b_ext.gpr`, which will import :file:`a_ext.gpr`

* :file:`main_ext.gpr` will implicitly import :file:`c_ext.gpr` since
  :file:`main.gpr` imports :file:`c.gpr`.

.. only:: html or latex

   The resulting project structure is shown in :numref:`fig-extends_all`,
   where the italicized labels, dashed arrows, and dashed boxes indicate what was
   added implicitly as an effect of the ``extends_all``.

   .. _fig-extends_all:

   .. figure:: importing_and_project_extension_figure_4.png
      :figwidth: image
      :align: center

      Project Structure with ``extends_all``

.. only:: not (html or latex)

  Thus:

     ::

           a.gpr <--- extends --- a_ext.gpr <-------+
            ^                        ^              |
            |                        |              |
         imports                 [imports]       imports
            |                        |              |
           b.gpr <-- [extends] -- [b_ext.gpr]   main_ext.gpr --+
            ^                        ^              |          |
            |                        |              |          |
         imports                  [imports]    [imports]       |
            |                        |              |          |
           c.gpr <--- extends --- [c_ext.gpr] <-----+          |
            ^                                                  |
            |                                                  |
         imports                                               |
            |                                                  |
          main.gpr <-------- extends_all-----------------------+

     where the bracketed elements indicate what was created
     automatically as an effect of the ``extends_all``.

When project :file:`main_ext.gpr` is built, the entire modified project space is
considered for recompilation, including the sources from :file:`b.gpr` and
:file:`c.gpr` that are affected by the changes to :file:`a.gpr`.


.. index:: Child project
.. index:: Parent project

.. _Child_Projects:

Child Projects
==============

In order to more clearly express the relationship between
a project ``Q`` and some other project ``P`` that ``Q``
either imports or extends, you can use the notation ``P.Q``
to declare ``Q`` as a **child** of ``P``.
The project ``P`` is then referred to as the **parent** of ``Q``.
This is useful, for example, when the purpose of the child
is to serve as a testing subsystem for the parent.

The visibility of the child on the sources and other properties
of the parent is determined by whether the child imports or
extends the parent. No additional visibility is obtained by
declaring the project as a child; the `parent.child` notation
serves solely as a naming convention  to convey to the reader
the closeness of the relationship between the projects.

For example:

  .. code-block:: gpr

     -- math_proj.gpr
     project Math_Proj is
        ...
     end Math_Proj;

     ---------------

     with "math_proj.gpr";
     project Math_Proj.Tests is      -- Legal; child imports parent
        ...
     end Math_Proj.Tests;

     ---------------

     project Math_Proj.High_Performance
        extends "math_proj.gpr" is   -- Legal; child extends parent
        ...
     end Math_Proj.High_Performance;

     ---------------

     project GUI_Proj.Tests is       -- Illegal
        ...
     end GUI_Proj.Tests;

Child projects may in turn be the parents of other projects, so
in general a project hierarchy can be created. A project
may be the parent of many child projects, but a child project
can only have one parent.

Note that child projects have slightly different semantics from
their Ada language analog (child units). An Ada child unit implicitly
|withs| its parent, whereas a child project must have an explicit
|with| clause (or else extend its parent). The need to explicitly
|with| or extend the parent project helps avoid the error of
unintentionally creating a child of some project that happens
to be on the project path.


.. index:: Aggregate project

.. _Aggregate_Projects:

Aggregate Projects
==================

Aggregate projects are an extension of the project paradigm, and are
designed to handle a few specific situations that cannot be solved directly
using standard projects. This section will present several such use
cases.


.. _Building_all_main_programs_from_a_single_project_closure:

Building all main programs from a single project closure
--------------------------------------------------------

A large application is typically organized into modules and submodules,
which are conveniently represented as a project graph (the project import
closure): a "root" project `A` |withs| the projects for modules `B` and `C`,
which in turn |with| projects for submodules.

Very often, modules will build their own executables (for testing
purposes for instance) or libraries (for easier reuse in various
contexts).

However, if you build your project through *GPRbuild*, using a syntax similar to

  ::

      gprbuild -PA.gpr

this will only rebuild the main programs of project A, not those of the
imported projects B and C. Therefore you have to spawn several
*GPRbuild* commands, one per project, to build all executables.
This is somewhat inconvenient, but more importantly is inefficient
because *GPRbuild* needs to do duplicate work to ensure that sources are
up-to-date, and cannot easily compile things in parallel when using
the :samp:`-j` switch.

Also, libraries are always rebuilt when building a project.

To solve this problem you can define an *aggregate project* ``Agg`` that
groups ``A``, ``B`` and ``C``:

  .. index:: Project_Files attribute

  .. code-block:: gpr

       aggregate project Agg is
          for Project_Files use ("a.gpr", "b.gpr", "c.gpr");
       end Agg;

Then, when you build with

  ::

      gprbuild -PAgg.gpr

this will build all main programs from ``A``, ``B`` and ``C``.

If `B` or `C` do not define any main program (through their `Main`
attribute), all their sources are built. When you do not group them
in an aggregate project, only those sources that are needed by `A`
will be built.

If you add a main to a project ``P`` not already explicitly referenced in the
aggregate project, you will need to add :file:`p.gpr` in the list of project
files for the aggregate project, or the main will not be built when
building the aggregate project.

.. _Building_a_set_of_projects_with_a_single_command:

Building a set of projects with a single command
------------------------------------------------

Another application of aggregate projects is when you have multiple
applications and libraries that are built independently
(but can be built in parallel). For instance, you might have a project graph
rooted at ``A``, and another one (which might share some subprojects) rooted
at ``B``.

Using only *GPRbuild*, you could do

  .. code-block:: sh

      gprbuild -PA.gpr
      gprbuild -PB.gpr

to build both. But again, *GPRbuild* has to do some duplicate work for
those files that are shared between the two, and cannot truly build
things in parallel efficiently.

If the two projects are really independent, share no sources other
than through a common subproject, and have no source files with a
common basename, you could create a project ``C`` that imports ``A`` and
``B``. But these restrictions are often too strong, and one has to build
them independently. An aggregate project does not have these
limitations and can aggregate two project graphs that have common
sources:

  .. code-block:: gpr

       aggregate project Agg is
          for Project_Files use ("a.gpr", "b.gpr");
       end Agg;

This scenario is particularly useful in environments like VxWorks 653
where the applications running in the multiple partitions can be built
in parallel through a single *GPRbuild* command. This also works well
with Annex E of the Ada Language Reference Manual.


.. _Defining_a_build_environment:

Defining a build environment
----------------------------

The environment variables at the time you launch *GPRbuild*
will influence the view these tools have of the project
(for example :envvar:`PATH` to find the compiler, :envvar:`ADA_PROJECT_PATH` or
:envvar:`GPR_PROJECT_PATH` to find the projects, and environment variables
that are referenced in project files
through the ``external`` built-in function). Several command line switches
can be used to override those (:samp:`-X` or :samp:`-aP`), but on some
systems and with some projects, this might make the command line too long,
and on all systems often make it hard to read.

An aggregate project can be used to set the environment for all
projects built through that aggregate. One of the benefits is that
you can put the aggregate project under configuration management, and
make sure all your users have a consistent environment when
building. For example:

  .. code-block:: gpr

       aggregate project Agg is
          for Project_Files use ("A.gpr", "B.gpr");
          for Project_Path use ("../dir1", "../dir1/dir2");
          for External ("BUILD") use "PRODUCTION";

          package Builder is
             for Global_Compilation_Switches ("Ada") use ("-g");
          end Builder;
       end Agg;

Another use of aggregate projects is to simulate the
referencing of external variables in |with| clauses,
For technical reasons the following project file is not
allowed:

  .. code-block:: gpr

       with external("SETUP") & "path/prj.gpr";   --  ILLEGAL
       project MyProject is
          ...
       end MyProject;

However, you can use aggregate projects to obtain an equivalent effect:

  .. code-block:: gpr

       aggregate project Agg is
           for Project_Path use (external("SETUP") & "path");
           for Project_Files use ("myproject.gpr");
       end Agg;

  .. code-block:: gpr

       with "prj.gpr";  --  searched on Agg'Project_Path
       project MyProject is
          ...
       end MyProject;


.. _Improving_builder_performance:

Improving builder performance
-----------------------------

The loading of aggregate projects is optimized in *GPRbuild*,
so that all files are searched for only once on the disk
(thus reducing the number of system calls and yielding faster
compilation times, especially on systems with sources on remote
servers). As part of the loading, *GPRbuild*
computes how and where a source file should be compiled, and even if it is
located several times in the aggregated projects it will be compiled only
once.

.. index:: -j

Since there is no ambiguity as to which switches should be used, individual
compilations, binds and links can be performed in parallel (through the usual
:samp:`-j` switch) and this can be done while maximizing the use of CPUs
(compared to launching multiple *GPRbuild* commands in parallel). The -j
option can control parallelization of compilation, binding, and linking
separately with -jc, -jb, and -jl variants accordingly.


.. _Syntax_of_aggregate_projects:

Syntax of aggregate projects
----------------------------

An aggregate project follows the general syntax of project files. The
recommended extension is still :file:`.gpr`. However, a special
``aggregate`` qualifier must appear before the keyword
``project``.

An aggregate project cannot |with| any other project (standard or
aggregate), except an abstract project (which can be used to share attribute
values). Also, aggregate projects cannot be extended or imported though a
|with| clause by any other project. Building other aggregate projects from
an aggregate project is done through the ``Project_Files`` attribute (see below).

An aggregate project does not have any source files directly (only
through other standard projects). Therefore a number of the standard
attributes and packages are forbidden in an aggregate project. Here is a
(non exhaustive) list:

* ``Languages``
* ``Source_Files``, ``Source_List_File`` and other attributes dealing with
  list of sources.
* ``Source_Dirs`` and ``Exec_Dir``
* ``Library_Dir``, ``Library_Name`` and other library-related attributes
* ``Main``
* ``Roots``
* ``Externally_Built``
* ``Inherit_Source_Path``
* ``Excluded_Source_Dirs``
* ``Locally_Removed_Files``
* ``Excluded_Source_Files``
* ``Excluded_Source_List_File``
* ``Interfaces``

The ``Object_Dir`` attribute is allowed and used by some analysis tools such
as `gnatcheck` to store intermediate files and aggregated results. The
attribute value is just ignored by the compilation toolchain, for which every
artifact of interest is best associated with the leaf non aggregate projects
and stored in the corresponding ``Object_Dir``.

The package ``Naming`` and packages that control the compilation process
(``Compiler``, ``Binder``, ``Linker`` and ``Install``) are forbidden.

The following three attributes can be used only in an aggregate project:

.. index:: Project_Files attribute

**Project_Files**:

  This attribute is compulsory.
  It specifies a list of constituent :file:`.gpr` files
  that are grouped in the aggregate. The list may be empty. The project
  files can be any projects except configuration or abstract projects;
  they can be other aggregate projects. When
  grouping standard projects, you can have both the root of a project import
  closure (and you do not need to specify all its imported projects), and
  any project within the closure.

  The basic idea is to specify all those projects that have
  main programs you want to build and link, or libraries you want to
  build. You can specify projects that do not use the ``Main``
  attribute or the ``Library_*`` attributes, and the result will be to
  build all their source files (not just the ones needed by other
  projects).

  The file can include paths (absolute or relative). Paths are relative to
  the location of the aggregate project file itself (if you use a base name,
  the :file:`.gpr` file is expected in the same directory as the aggregate
  project file). The environment variables :envvar:`ADA_PROJECT_PATH`,
  :envvar:`GPR_PROJECT_PATH` and :envvar:`GPR_PROJECT_PATH_FILE` are not used to
  find the project files. The extension :file:`.gpr` is mandatory, since
  this attribute contains file names, not project names.

  Paths can also include the ``"*"`` and ``"**"`` globbing patterns. The
  latter indicates that any subdirectory (recursively) will be
  searched for matching files. The ``"**"`` pattern can only occur at the
  last position in the directory part (i.e. ``"a/**/*.gpr"`` is supported, but
  not ``"**/a/*.gpr"``). Starting the pattern with ``"**"`` is equivalent
  to starting with ``"./**"``.

  At present the pattern ``"*"`` is only allowed in the filename part, not
  in the directory part. This is mostly for efficiency reasons to limit the
  number of system calls that are needed.

  Here are a few examples:

    .. code-block:: gpr

       for Project_Files use ("a.gpr", "subdir/b.gpr");
       --  two specific projects relative to the directory of agg.gpr

       for Project_Files use ("**/*.gpr");
       --  all projects recursively, except in the current directory

       for Project_Files use ("**/*.gpr", "*.gpr");
       --  all projects recursively


.. index:: Project_Path attribute

**Project_Path**:

  This attribute can be used to specify a list of directories in
  which to search for project files in |with| clauses.

  When you specify a project in ``Project_Files`` (say :file:`x/y/a.gpr`), and
  :file:`a.gpr` imports a project :file:`b.gpr`, only :file:`b.gpr` is
  searched in the project path. The file :file:`a.gpr` must be exactly at
  :samp:`{dir of the aggregate}/x/y/a.gpr`.

  This attribute, however, does not affect the search for the aggregated
  project files specified with ``Project_Files``.

  Each aggregate project has its own ``Project_Path`` (thus if
  :file:`agg1.gpr` includes :file:`agg2.gpr`, they can potentially both have a
  different `Project_Path`).

  This project path is defined as the concatenation, in this order, of:

  * the current directory;

  * followed by the command line :samp:`-aP` switches;

  * then the directories from the :envvar:`GPR_PROJECT_PATH` and
    :envvar:`ADA_PROJECT_PATH` environment variables;

  * then the directories from the ``Project_Path`` attribute;

  * and finally the predefined directories.

  In the example above, the project path for :file:`agg2.gpr` is not influenced by
  the attribute `agg1'Project_Path`, nor is `agg1` influenced by
  `agg2'Project_Path`.


  .. only:: html or latex

     This can potentially lead to errors. Consider the example in
     :numref:`fig-aggproject_error`.

     .. _fig-aggproject_error:

     .. figure:: project-manager-figure.png
        :figwidth: image
        :align: center

        Example of ``Project_Path`` Error

  .. only:: not (html or latex)

     This can potentially lead to errors. Consider the following example:

     ::

         +---------------+                  +----------------+
         | agg1.gpr      |                  | agg2.gpr       |
         |  'project_path|----includes----->|  'project_path |
         |               |                  |                |
         +---------------+                  +----------------+
               |                                   |
            includes                           includes
               |                                   |
               v                                   v
           +-------+                          +---------+
           | p.gpr |<-------- imports --------|  q.gpr  |
           +-------+---------+                +---------+
               |             |
            imports       imports
               |             |
               v             v
           +-------+      +---------+
           | r.gpr |      | r'.gpr  |
           +-------+      +---------+

  When looking for :file:`p.gpr`, both aggregates find the same physical
  file on the disk. However, it might happen that with their different
  project paths, both aggregate projects would in fact find a different
  :file:`r.gpr`.
  Since we have a common project :file:`p.gpr` |withing| two different
  :file:`r.gpr`, this will be reported as an error by the builder.

  Directories are relative to the location of the aggregate project file.

  Example:

    .. code-block:: gpr

        for Project_Path use ("/usr/local/gpr", "gpr/");

.. index:: External attribute

**External**:

  This attribute can be used to set the value of environment
  variables as retrieved through the ``external`` function
  in projects. It does not affect the environment variables
  themselves (so for instance you cannot use it to change the value
  of your :envvar:`PATH` as seen from the spawned compiler).

  This attribute affects the external values as seen in the rest of
  the aggregate project, and in the aggregated projects.

  The exact value of an external variable comes from one of three
  sources (each level overrides the previous levels):

  * An External attribute in aggregate project, for instance
    `for External ("BUILD_MODE") use "DEBUG"`;

  * Environment variables.
    These override the value given by the attribute, so that
    users can override the value set in the (presumably shared
    with others team members) aggregate project.

  * The :samp:`-X` command line switch to *gprbuild*.
    This always takes precedence.

  This attribute is only taken into account in the main aggregate
  project (i.e. the one specified on the command line to *GPRbuild*),
  and ignored in other aggregate projects. It is invalid
  in standard projects.
  The goal is to have a consistent value in all
  projects that are built through the aggregate, which would not
  be the case in a "diamond" situation: ``A`` groups the aggregate
  projects ``B`` and ``C``, which both (either directly or indirectly)
  build the project ``P``. If ``B`` and ``C`` could set different values for
  the environment variables, we would have two different views of
  ``P``, which in particular might impact the list of source files in ``P``.


.. index:: Package Builder

.. _package_Builder_in_aggregate_projects:

package Builder in aggregate projects
-------------------------------------

When used in an aggregate project, only the following attributes of this
package are valid:

.. index:: Switches attribute

**Switches**:

  This attribute gives the list of switches to use for *GPRbuild*.
  Because no mains can be specified for aggregate projects, the only possible
  index for attribute ``Switches`` is ``others``. All other indexes will
  be ignored.

  Example:

  .. code-block:: gpr

     for Switches (others) use ("-v", "-k", "-j8");

  These switches are only read from the main aggregate project (the
  one passed on the command line), and ignored in all other aggregate
  projects or projects.

  It can only contain builder switches, not compiler switches.

.. index:: Global_Compilation_Switches attribute

**Global_Compilation_Switches**

  This attribute gives the list of compiler switches for the various
  languages. For instance,

  .. code-block:: gpr

    for Global_Compilation_Switches ("Ada") use ("O1", "-g");
    for Global_Compilation_Switches ("C")   use ("-O2");

  This attribute is only taken into account in the aggregate project
  specified on the command line, not in other aggregate projects.

  In the projects grouped by that aggregate, the attribute
  ``Builder'Global_Compilation_Switches`` is also ignored. However, the
  attribute ``Compiler'Default_Switches`` will be taken into account (but
  that of the aggregate has higher priority). The attribute
  ``Compiler'Switches`` is also taken into account and can be used to
  override the switches for a specific file. As a result, it always
  has priority.

  The rules are meant to avoid ambiguities when compiling. For
  instance, aggregate project ``Agg`` groups the projects ``A`` and ``B``, which
  both depend on ``C``. Here is an example for all of these projects:

  .. code-block:: gpr

     aggregate project Agg is
         for Project_Files use ("a.gpr", "b.gpr");
         package Builder is
            for Global_Compilation_Switches ("Ada") use ("-O2");
         end Builder;
     end Agg;

  .. code-block:: gpr

     with "c.gpr";
     project A is
         package Builder is
            for Global_Compilation_Switches ("Ada") use ("-O1");
            --  ignored
         end Builder;

         package Compiler is
            for Default_Switches ("Ada")
                use ("-O1", "-g");
            for Switches ("a_file1.adb")
                use ("-O0");
         end Compiler;
     end A;

  .. code-block:: gpr

     with "c.gpr";
     project B is
         package Compiler is
            for Default_Switches ("Ada") use ("-O0");
         end Compiler;
     end B;

  .. code-block:: gpr

     project C is
         package Compiler is
            for Default_Switches ("Ada")
                use ("-O3",
                     "-gnatn");
            for Switches ("c_file1.adb")
                use ("-O0", "-g");
         end Compiler;
     end C;


  The following switches are used:

  * all files from project ``A`` except :file:`a_file1.adb` are compiled
    with :samp:`-O2 -g`, since the aggregate project has priority.

  * the file :file:`a_file1.adb` is compiled with
    :option"`-O0`, since ``Compiler'Switches`` has priority

  * all files from project ``B`` are compiled with
    :samp:`-O2`, since the aggregate project has priority

  * all files from ``C`` are compiled with :samp:`-O2 -gnatn`, except for
    :file:`c_file1.adb` which is compiled with :samp:`-O0 -g`

  Even though ``C`` is seen through two paths (through ``A`` and through
  ``B``), the switches used by the compiler are unambiguous.


.. index:: Global_Configuration_Pragmas attribute

**Global_Configuration_Pragmas**

  This attribute can be used to specify a file containing
  configuration pragmas, to be passed to the Ada compiler.  Since we
  ignore the package ``Builder`` in other aggregate projects and projects,
  only those pragmas defined in the main aggregate project will be
  taken into account.

  Projects can locally add to those by using the
  ``Compiler'Local_Configuration_Pragmas`` attribute if they need.


.. index:: Global_Config_File attribute

**Global_Config_File**

  This attribute, indexed with a language name, can be used to specify a config
  when compiling sources of the language. For Ada, these files are configuration
  pragmas files.

For projects that are built through the aggregate mechanism, the package ``Builder``
is ignored, except for the ``Executable`` attribute which specifies the
name of the executables resulting from the link of the main programs, and
for the ``Executable_Suffix``.


.. index:: Aggregate library project

.. _Aggregate_Library_Projects:

Aggregate Library Projects
==========================

Aggregate library projects make it possible to build a single library
using object files built using other standard or library
projects. This gives the flexibility to describe an application as
having multiple modules (for example a GUI, database access, and other)
using different project files (so possibly built with different compiler
options) and yet create a single library (static or relocatable) out of the
corresponding object files.

.. _Building_aggregate_library_projects:

Building aggregate library projects
-----------------------------------

For example, we can define an aggregate project ``Agg`` that groups ``A``, ``B``
and ``C``:

  .. code-block:: gpr

       aggregate library project Agg is
          for Project_Files use ("a.gpr", "b.gpr", "c.gpr");
          for Library_Name  use "agg";
          for Library_Dir   use "lagg";
       end Agg;

Then, when you build with:

  .. code-block:: sh

       gprbuild agg.gpr

this will build all units from projects ``A``, ``B`` and ``C`` and will create a
static library named :file:`libagg.a` in the :file:`lagg`
directory. An aggregate library project has the same set of
restrictions as a standard library project.

Note that a shared aggregate library project cannot aggregate a
static library project. In platforms where a compiler option is
required to create relocatable object files, a ``Builder package`` in the
aggregate library project may be used:

  .. code-block:: gpr

       aggregate library project Agg is
          for Project_Files use ("a.gpr", "b.gpr", "c.gpr");
          for Library_Name use ("agg");
          for Library_Dir use ("lagg");
          for Library_Kind use "relocatable";

          package Builder is
             for Global_Compilation_Switches ("Ada") use ("-fPIC");
          end Builder;
       end Agg;

With the above aggregate library Builder package, the :samp:`-fPIC`
option will be passed to the compiler when building any source code
from projects :file:`a.gpr`, :file:`b.gpr` and :file:`c.gpr`.


.. _Syntax_of_aggregate_library_projects:

Syntax of aggregate library projects
------------------------------------

An aggregate library project follows the general syntax of project
files. The recommended extension is still :file:`.gpr`. However, a special
``aggregate library`` qualifier must appear before the keyword
``project``.

An aggregate library project cannot |with| any other project
(standard or aggregate), except an abstract project which can be used
to share attribute values.

An aggregate library project does not have any source files directly (only
through other standard projects). Therefore a number of the standard
attributes and packages are forbidden in an aggregate library
project. Here is a (non-exhaustive) list:

* ``Languages``
* ``Source_Files``, ``Source_List_File`` and other attributes dealing with
  a list of sources.
* ``Source_Dirs`` and ``Exec_Dir``
* ``Main``
* ``Roots``
* ``Externally_Built``
* ``Inherit_Source_Path``
* ``Excluded_Source_Dirs``
* ``Locally_Removed_Files``
* ``Excluded_Source_Files``
* ``Excluded_Source_List_File``

The only package that is allowed (and optional) is ``Builder``.

The ``Project_Files`` attribute is used to
describe the aggregated projects whose object files have to be
included into the aggregate library. The environment variables
:envvar:`ADA_PROJECT_PATH`, :envvar:`GPR_PROJECT_PATH` and
:envvar:`GPR_PROJECT_PATH_FILE` are not used to find the project files.

As for regular (not library) aggregate projects, the ``Object_Dir`` attribute
is allowed and used by some analysis tools in the same fashion.

.. _Project_File_Reference:

Project File Reference
======================

This section describes the syntactic structure of project files, explains
the various constructs that can be used, and summarizes the available
attributes.

The syntax is presented in a notation similar to what is used in the Ada
Language Reference Manual. Curly braces '{' and '}' indicate 0 or more
occurrences of the enclosed construct, and square brackets '[' and ']' indicate
0 or 1 occurrence of the enclosed construct. Reserved words are enclosed
between apostrophes.

.. toctree::
   :maxdepth: 1

   project_file_reference
   attributes
   environment_variables


.. _Project_File_Glossary:

Glossary
========

.. glossary::

   Abstract project
     A project with no source files, typically used to define common attributes
     that are shared by other project files. See :ref:`Sharing_between_Projects`.

   Aggregate project
     A project that in effect combines several projects in order to efficiently support
     concurrent builds or builds of all main programs from the constituent projects,
     or the convenient definition of a common environment for the constituent projects.
     See :ref:`Aggregate_Projects`.

   Attribute
     A named property of a project or one of its packages.  See :ref:`Attributes`.

   Base project
     A project that is extended by some other project. See :ref:`Project_Extension`.

   Child project
     A project that is defined by a name ``Parent_proj.Child_proj``
     where ``Child_proj`` either imports or extends ``Parent_Proj``.
     This feature is typically used to show a close relationship between
     the two projects, for example where the child project serves as a testbed
     for the parent. See :ref:`Child_Projects`.

   Configuration project
     A project that describes compilers and other tools, for use by *GPRbuild*.
     See :ref:`Configuration Project<Configuration_Project>`.

   Extending a project
     The reuse and possible adaption by one project of the source files from
     another project (the base project). Somewhat analogous to (single) class inheritance in
     object-oriented programming. See :ref:`Project_Extension`.

   External variable
     A variable that is defined on the command line (by the :samp:`-X` switch), as the
     value of an environment variable, or, by default, as the second parameter to the
     ``external`` function. See :ref:`Scenarios_in_Projects`.

   Global attribute
     An attribute that applies to all projects in the project import closure
     of a main project. See :ref:`Global_Attributes`.

   Importing a project
     The usage of a |with| or |limited_with| clause on a project file in order
     to reuse properties of some other project file. See :ref:`Importing_Projects`.

   Independent project
     A project defined by a single project file and thus not dependent on any other
     projects. See :ref:`Independent Project<Independent_Project>`.

   Library project
     A project that is used to define a library rather than an executable program.
     See :ref:`Library_Projects`.

   Main project
     A project that is specified on the command line. See :ref:`Global_Attributes`.

   Package
     A grouping of attribute definitions related to a particular GNAT tool.
     See :ref:`Packages`.

   Parent project
     A project that has one or more child projects.  See :ref:`Child_Projects`.

   Project
     A set of named properties and their values, associated with the GNAT tools that
     are used during the development of software in Ada and other languages.
     Properties include directories for source files, object files, and executables;
     the switch settings for the various tools; and the naming scheme for source
     files.

   Project extension
     See glossary item `Extending a project`

   Project file
     A textual representation of a project, which uses an Ada-like notation.
     The syntax is presented in :ref:`Project_File_Reference`.

   Project import closure
     The *project import closure* for a given project `proj` is the set
     of projects consisting of `proj` itself, together with each
     project that is directly or indirectly imported by `proj`.
     The import may be from either a |with| or a |limited_with|.
     See :ref:`Project Import Closure<Project_Import_Closure>`.

   Scenario
     The values of a project's variables and attributes, as determined by
     the settings of external variables referenced by a project. A scenario
     typically defines a particular mode of usage for the project.
     See :ref:`Scenarios_in_Projects`.

   Scenario variable
     An external variable, typically assigned to a typed variable and queried in
     a `case construction`. See :ref:`Scenario variable<Scenario_Variable>`.

   Standard project
     A non-library project with source files. See :ref:`Standard project<Standard_Project>`

   Typed variable
     A project variable that can take any of a specified set of values, analogous
     to a variable of an Ada enumeration type but where the values are string literals.
     See :ref:`Scenarios_in_Projects`.
