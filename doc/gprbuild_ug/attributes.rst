.. index:: attribute

.. _Attributes:

Attributes
----------

A project (and its packages) may have **attributes** that define
the project's properties.  Some attributes have values that are strings;
others have values that are string lists.

::

     attribute_declaration ::=
        simple_attribute_declaration | indexed_attribute_declaration

     simple_attribute_declaration ::= 'for' attribute_designator 'use' expression ;

     indexed_attribute_declaration ::=
       'for' *<indexed_attribute_>*simple_name ( string_literal) 'use' expression ;

     attribute_designator ::=
       <simple_attribute_>simple_name
       | <indexed_attribute_>simple_name ( string_literal )

There are two categories of attributes: **simple attributes**
and **indexed attributes**.
Each simple attribute has a default value: the empty string (for string
attributes) and the empty list (for string list attributes).
An attribute declaration defines a new value for an attribute, and overrides
the previous value. The syntax of a simple attribute declaration is similar to
that of an attribute definition clause in Ada.

Some attributes are indexed. These attributes are mappings whose
domain is a set of strings. They are declared one association
at a time, by specifying a point in the domain and the corresponding image
of the attribute.
Like untyped variables and simple attributes, indexed attributes
may be declared several times. Each declaration supplies a new value for the
attribute, and replaces the previous setting.

Here are some examples of attribute declarations:

  .. code-block:: gpr

       --  simple attributes
       for Object_Dir use "objects";
       for Source_Dirs use ("units", "test/drivers");

       --  indexed attributes
       for Body ("main") use "Main.ada";
       for Switches ("main.ada")
           use ("-v", "-gnatv");
       for Switches ("main.ada") use Builder'Switches ("main.ada") & "-g";

       --  indexed attributes copy (from package Builder in project Default)
       --  The package name must always be specified, even if it is the current
       --  package.
       for Default_Switches use Default.Builder'Default_Switches;

When an attribute is defined in the configuration project but not in the user
project, it is inherited in the user project.

When a single string attribute is defined in both the configuration project
and the user project, its value in the user project is as declared; the value
in the configuration project does not matter.

For string list attributes, there are two cases. Some of these attributes are
**configuration concatenable**. For these attributes, when they are declared
in both the configuration project and the user project, the final value is
the concatenation of the value in the configuration project with the value
in the user project. The configuration concatenable attributes are indicated
in the list below.

Attributes references may appear anywhere in expressions, and are used
to retrieve the value previously assigned to the attribute. If an attribute
has not been set in a given package or project, its value defaults to the
empty string or the empty list, with some exceptions.

::

    attribute_reference ::=
      attribute_prefix ' <simple_attribute>_simple_name [ (string_literal) ]

    attribute_prefix ::= 'project'
      | <project_>simple_name
      | package_identifier
      | <project_>simple_name . package_identifier

Here are some examples:

  ::

      project'Object_Dir
      Naming'Dot_Replacement
      Imported_Project'Source_Dirs
      Imported_Project.Naming'Casing
      Builder'Default_Switches ("Ada")

The exceptions to the empty defaults are:

* ``Object_Dir``: default is ``"."``
* ``Exec_Dir``: default is ``'Object_Dir``, that is, the value of attribute
  ``Object_Dir`` in the same project, declared or defaulted
* ``Source_Dirs``: default is ``(".")``

The prefix of an attribute may be:

* ``project`` for an attribute of the current project
* The name of an existing package of the current project
* The name of an imported project
* The name of a parent project that is extended by the current project
* An expanded name whose prefix is imported/base/parent project name,
  and whose selector is a package name

In the following sections, all predefined attributes are succinctly described,
first the project level attributes (that is, those attributes that are not in a
package), then the attributes in the different packages.

It is possible for different tools to dynamically create new packages with
attributes, or new attributes in predefined packages. These attributes are
not documented here.

The attributes under Configuration headings are usually found only in
configuration project files.

The characteristics of each attribute are indicated as follows:

* **Type of value**

  The value of an attribute may be a single string, indicated by the word
  "single value", a string list, indicated by the word "list value" or a
  string set (list where the elements are unique), indicated by the word "set
  value". In the case of a set, it is also indicated if the elements are
  considered case sensitive or not.

* **Read-only**

  When the attribute is read-only -- that is when a declaration for
  the attribute is forbidden -- this is indicated by the "read-only".

* **Optional index**

  If an optional index is allowed in the value of the attribute (both single
  and list), this is indicated by the words "others index allowed". Such
  definition is used when no other index match.

* **Indexed attribute**

  An indexed attribute is indicated by the word "indexed by ..." followed by
  the kind of index:

  * *language*: the index is a language, case insensitive
  * *file name*: the index is a simple file name (so without any directory
    or subdirectory indication).
  * *source glob*: the index is the simple file name of a source file, or
    a glob pattern that matches simple file name of several source files. The
    case sensitivity depends on the host.
  * *source glob or language*: the index is either a source glob or a
    language (see above the two kind of index).
  * *unit*: the index is an Ada unit, case insensitive
  * *string*: the index is a generic case sensitive string
  * *external reference*: the index is an external reference

* **Configuration concatenable**

  For a string list attribute, the final value if the attribute is declared
  in both the configuration project and the user project is the concatenation
  of the two value, configuration then user.

* **Inheritance**

  By default, top-level attribute values are inherited from extended projects
  if any. If not, it is indicated:

  * *not inherited from extended* when the value is not inherited
  * *concatenated from extended* when the final value is the concatenation
    of the list inherited from the extended project and the project's own
    definition.


.. _Project_Level_Attributes:

Project Level Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

* **Configuration - Archives**

  .. index:: Attributes - Project Level Attributes; Archive_Builder

  * **Archive_Builder**: list value, not inherited from extended project

    Value is the name of the application to be used to create a static library
    (archive), followed by the options to be used.

  .. index:: Attributes - Project Level Attributes; Archive_Builder_Append_Option

  * **Archive_Builder_Append_Option**: list value, not inherited from extended project

    Value is the list of options to be used when invoking the archive builder to
    add project files into an archive.

  .. index:: Attributes - Project Level Attributes; Archive_Indexer

  * **Archive_Indexer**: list value, not inherited from extended project

    Value is the name of the archive indexer, followed by the required options.

  .. index:: Attributes - Project Level Attributes; Archive_Suffix

  * **Archive_Suffix**: single value, not inherited from extended project

    Value is the extension of archives. When not declared, the extension is '.a'.

  .. index:: Attributes - Project Level Attributes; Library_Partial_Linker

  * **Library_Partial_Linker**: list value, not inherited from extended project

    Value is the name of the partial linker executable, followed by the required
    options.

* **Directories**

  .. index:: Attributes - Project Level Attributes; Create_Missing_Dirs

  * **Create_Missing_Dirs**: single value

    Indicates if the missing object, library and executable directories should be
    created automatically by the project-aware tool. Taken into account only in the
    main project. Only authorized case-insensitive values are 'true' and 'false'.

  .. index:: Attributes - Project Level Attributes; Exec_Dir

  * **Exec_Dir**: single value, not inherited from extended project

    Indicates the exec directory for the project, that is the directory where the
    executables are.

  .. index:: Attributes - Project Level Attributes; Ignore_Source_Sub_Dirs

  * **Ignore_Source_Sub_Dirs**: list value, not inherited from extended project

    Value is a list of simple names or patterns for subdirectories that are removed
    from the list of source directories, including their subdirectories.

  .. index:: Attributes - Project Level Attributes; Inherit_Source_Path

  * **Inherit_Source_Path**: list value, indexed by a language

    Index is a language name. Value is a list of language names. Indicates that in
    the source search path of the index language the source directories of the
    languages in the list should be included.

  .. index:: Attributes - Project Level Attributes; Object_Dir

  * **Object_Dir**: single value, not inherited from extended project

    Indicates the object directory for the project.

  .. index:: Attributes - Project Level Attributes; Source_Dirs

  * **Source_Dirs**: list value, not inherited from extended project

    The list of source directories of the project.

* **Configuration - General**

  .. index:: Attributes - Project Level Attributes; Default_Language

  * **Default_Language**: single value, not inherited from extended project

    Value is the case-insensitive name of the language of a project when attribute
    Languages is not specified.

  .. index:: Attributes - Project Level Attributes; Object_Generated

  * **Object_Generated**: single value, indexed by a language

    Index is a language name. Indicates if invoking the compiler for a language
    produces an object file. Only authorized case-insensitive values are 'false'
    and 'true' (the default).

  .. index:: Attributes - Project Level Attributes; Objects_Linked

  * **Objects_Linked**: single value, indexed by a language

    Index is a language name. Indicates if the object files created by the compiler
    for a language need to be linked in the executable. Only authorized
    case-insensitive values are 'false' and 'true' (the default).

  .. index:: Attributes - Project Level Attributes; Required_Toolchain_Version

  * **Required_Toolchain_Version**: single value, indexed by a language

    Index is a language name. Specify the value expected for the Toolchain_Version
    attribute for this language, typically provided by an auto-generated
    configuration project. If Required_Toolchain_Version and Toolchain_Version do
    not match, the project processing aborts with an error.

  .. index:: Attributes - Project Level Attributes; Run_Path_Option

  * **Run_Path_Option**: list value, not inherited from extended project

    Value is the list of switches to be used when specifying the run path option in
    an executable.

  .. index:: Attributes - Project Level Attributes; Run_Path_Origin

  * **Run_Path_Origin**: single value, not inherited from extended project

    Value is the string that may replace the path name of the executable directory
    in the run path options.

  .. index:: Attributes - Project Level Attributes; Runtime

  * **Runtime**: single value, indexed by a language

    Index is a language name. Indicates the runtime directory that is to be used
    when using the compiler of the language. Taken into account only in the main
    project, or its extended projects if any. Note that when the runtime is
    specified for a language on the command line (usually with a switch ``--RTS``),
    the value of attribute reference 'Runtime for this language is the one
    specified on the command line.

  .. index:: Attributes - Project Level Attributes; Runtime_Dir

  * **Runtime_Dir**: single value, indexed by a language

    Index is a language name. Value is the path name of the runtime directory for
    the language.

  .. index:: Attributes - Project Level Attributes; Runtime_Library_Dir

  * **Runtime_Library_Dir**: single value, indexed by a language, not inherited from extended project

    Index is a language name. Value is the path name of the directory where the
    runtime libraries are located. This attribute is obsolete.

  .. index:: Attributes - Project Level Attributes; Runtime_Source_Dir

  * **Runtime_Source_Dir**: single value, indexed by a language

    Index is a language name. Value is the path name of the directory where the
    sources of runtime libraries are located. This attribute is obsolete.

  .. index:: Attributes - Project Level Attributes; Runtime_Source_Dirs

  * **Runtime_Source_Dirs**: single value, indexed by a language

    Index is a language name. Value is the path names of the directories where the
    sources of runtime libraries are located. This attribute is not normally
    declared.

  .. index:: Attributes - Project Level Attributes; Separate_Run_Path_Options

  * **Separate_Run_Path_Options**: single value, not inherited from extended project

    Indicates if there may be several run path options specified when linking an
    executable. Only authorized case-insensitive values are 'true' or 'false' (the
    default).

  .. index:: Attributes - Project Level Attributes; Target

  * **Target**: single value

    Value is the name of the target platform. Taken into account only in the main
    project. Note that when the target is specified on the command line (usually
    with a switch ``--target=``), the value of attribute reference 'Target is the
    one specified on the command line.

  .. index:: Attributes - Project Level Attributes; Toolchain_Version

  * **Toolchain_Version**: single value, indexed by a language

    Index is a language name. Specify the version of a toolchain for a language.

  .. index:: Attributes - Project Level Attributes; Toolchain_Name

  * **Toolchain_Name**: single value, indexed by a language

    Index is a language name. Indicates the toolchain name that is to be used when
    using the compiler of the language. Taken into account only in the main
    project, or its extended projects if any.

  .. index:: Attributes - Project Level Attributes; Toolchain_Description

  * **Toolchain_Description**: single value, indexed by a language

    Obsolescent. No longer used.

* **Source Files**

  .. index:: Attributes - Project Level Attributes; Excluded_Source_Files

  * **Excluded_Source_Files**: list value, not inherited from extended project

    Value is a list of simple file names that are not sources of the project.
    Allows to remove sources that are inherited or found in the source directories
    and that match the naming scheme.

  .. index:: Attributes - Project Level Attributes; Excluded_Source_List_File

  * **Excluded_Source_List_File**: single value, not inherited from extended project

    Value is a text file name that contains a list of file simple names that are
    not sources of the project.

  .. index:: Attributes - Project Level Attributes; Interfaces

  * **Interfaces**: set value, case-sensitive

    Value is a list of file names that constitutes the interfaces of the project.

  .. index:: Attributes - Project Level Attributes; Locally_Removed_Files

  * **Locally_Removed_Files**: list value, not inherited from extended project

    Obsolescent. Equivalent to Excluded_Source_Files.

  .. index:: Attributes - Project Level Attributes; Source_Files

  * **Source_Files**: list value, not inherited from extended project

    Value is a list of source file simple names.

  .. index:: Attributes - Project Level Attributes; Source_List_File

  * **Source_List_File**: single value, not inherited from extended project

    Value is a text file name that contains a list of source file simple names, one
    on each line.

* **Aggregate Projects**

  .. index:: Attributes - Project Level Attributes; External

  * **External**: single value, indexed by an external reference

    Index is the name of an external reference. Value is the value of the external
    reference to be used when parsing the aggregated projects.

  .. index:: Attributes - Project Level Attributes; Project_Files

  * **Project_Files**: list value, not inherited from extended project

    Value is the list of aggregated projects.

  .. index:: Attributes - Project Level Attributes; Project_Path

  * **Project_Path**: list value, not inherited from extended project

    Value is a list of directories that are added to the project search path when
    looking for the aggregated projects.

* **General**

  .. index:: Attributes - Project Level Attributes; Externally_Built

  * **Externally_Built**: single value, not inherited from extended project

    Indicates if the project is externally built. Only case-insensitive values
    allowed are 'true' and 'false', the default.

  .. index:: Attributes - Project Level Attributes; Languages

  * **Languages**: set value, case-insensitive, concatenated with extended value

    The list of languages of the sources of the project.

  .. index:: Attributes - Project Level Attributes; Main

  * **Main**: list value

    The list of main sources for the executables.

  .. index:: Attributes - Project Level Attributes; Name

  * **Name**: single value, read-only, not inherited from extended project

    The name of the project.

  .. index:: Attributes - Project Level Attributes; Project_Dir

  * **Project_Dir**: single value, read-only, not inherited from extended project

    The path name of the project directory.

  .. index:: Attributes - Project Level Attributes; Roots

  * **Roots**: list value, indexed by a source glob

    The index is the file name of an executable source. Indicates the list of units
    from the main project that need to be bound and linked with their closures with
    the executable. The index is either a file name, a language name or '*'. The
    roots for an executable source are those in Roots with an index that is the
    executable source file name, if declared. Otherwise, they are those in Roots
    with an index that is the language name of the executable source, if present.
    Otherwise, they are those in Roots ('*'), if declared. If none of these three
    possibilities are declared, then there are no roots for the executable source.

  .. index:: Attributes - Project Level Attributes; Warning_Message

  * **Warning_Message**: single value

    Causes gprbuild to emit a user-defined warning message.

* **Libraries**

  .. index:: Attributes - Project Level Attributes; Leading_Library_Options

  * **Leading_Library_Options**: list value, configuration concatenable, not inherited from extended project

    Value is a list of options that are to be used at the beginning of the command
    line when linking a shared library.

  .. index:: Attributes - Project Level Attributes; Library_Auto_Init

  * **Library_Auto_Init**: single value, not inherited from extended project

    Indicates if a Stand-Alone Library is auto-initialized. Only authorized
    case-insensitive values are 'true' and 'false'.

  .. index:: Attributes - Project Level Attributes; Library_Dir

  * **Library_Dir**: single value

    Value is the name of the library directory. This attribute needs to be declared
    for each library project.

  .. index:: Attributes - Project Level Attributes; Library_Encapsulated_Options

  * **Library_Encapsulated_Options**: list value, configuration concatenable, not inherited from extended project

    Value is a list of options that need to be used when linking an encapsulated
    Stand-Alone Library.

  .. index:: Attributes - Project Level Attributes; Library_Encapsulated_Supported

  * **Library_Encapsulated_Supported**: single value, not inherited from extended project

    Indicates if encapsulated Stand-Alone Libraries are supported. Only authorized
    case-insensitive values are 'true' and 'false' (the default).

  .. index:: Attributes - Project Level Attributes; Library_Interface

  * **Library_Interface**: set value, case-insensitive

    Value is the list of unit names that constitutes the interfaces of a
    Stand-Alone Library project.

  .. index:: Attributes - Project Level Attributes; Library_Kind

  * **Library_Kind**: single value, not inherited from extended project

    Specifies the kind of library: static library (archive) or shared library.
    Case-insensitive values must be one of 'static' for archives (the default),
    'static-pic' for archives of Position Independent Code, or 'dynamic' or
    'relocatable' for shared libraries.

  .. index:: Attributes - Project Level Attributes; Library_Name

  * **Library_Name**: single value

    Value is the name of the library. This attribute needs to be declared or
    inherited for each library project.

  .. index:: Attributes - Project Level Attributes; Library_Options

  * **Library_Options**: list value, configuration concatenable, not inherited from extended project

    Value is a list of options that are to be used when linking a shared library.

  .. index:: Attributes - Project Level Attributes; Library_Reference_Symbol_File

  * **Library_Reference_Symbol_File**: single value, not inherited from extended project

    Value is the name of the reference symbol file.

  .. index:: Attributes - Project Level Attributes; Library_Rpath_Options

  * **Library_Rpath_Options**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is a list of options for an invocation of the
    compiler of the language. This invocation is done for a shared library project
    with sources of the language. The output of the invocation is the path name of
    a shared library file. The directory name is to be put in the run path option
    switch when linking the shared library for the project.

  .. index:: Attributes - Project Level Attributes; Library_Src_Dir

  * **Library_Src_Dir**: single value, not inherited from extended project

    Value is the name of the directory where copies of the sources of the
    interfaces of a Stand-Alone Library are to be copied.

  .. index:: Attributes - Project Level Attributes; Library_Standalone

  * **Library_Standalone**: single value, not inherited from extended project

    Specifies if a Stand-Alone Library (SAL) is encapsulated or not. Only
    authorized case-insensitive values are 'standard' for non encapsulated SALs,
    'encapsulated' for encapsulated SALs or 'no' for non SAL library project.

  .. index:: Attributes - Project Level Attributes; Library_Symbol_File

  * **Library_Symbol_File**: single value, not inherited from extended project

    Value is the name of the library symbol file.

  .. index:: Attributes - Project Level Attributes; Library_Symbol_Policy

  * **Library_Symbol_Policy**: single value, not inherited from extended project

    Indicates the symbol policy kind. Only authorized case-insensitive values are
    'restricted', 'unrestricted'.

  .. index:: Attributes - Project Level Attributes; Library_Version

  * **Library_Version**: single value, not inherited from extended project

    Value is the name of the library file.

* **Configuration - Shared Libraries**

  .. index:: Attributes - Project Level Attributes; Library_Auto_Init_Supported

  * **Library_Auto_Init_Supported**: single value, not inherited from extended project

    Indicates if auto-initialization of Stand-Alone Libraries is supported. Only
    authorized case-insensitive values are 'true' and 'false' (the default).

  .. index:: Attributes - Project Level Attributes; Library_Install_Name_Option

  * **Library_Install_Name_Option**: single value, not inherited from extended project

    Value is the name of the option that needs to be used, concatenated with the
    path name of the library file, when linking a shared library.

  .. index:: Attributes - Project Level Attributes; Library_Major_Minor_Id_Supported

  * **Library_Major_Minor_Id_Supported**: single value, not inherited from extended project

    Indicates if major and minor ids for shared library names are supported on the
    platform. Only authorized case-insensitive values are 'true' and 'false' (the
    default).

  .. index:: Attributes - Project Level Attributes; Library_Version_Switches

  * **Library_Version_Switches**: list value, configuration concatenable, not inherited from extended project

    Value is the list of switches to specify a internal name for a shared library.

  .. index:: Attributes - Project Level Attributes; Shared_Library_Minimum_Switches

  * **Shared_Library_Minimum_Switches**: list value, not inherited from extended project

    Value is the list of required switches when linking a shared library.

  .. index:: Attributes - Project Level Attributes; Shared_Library_Prefix

  * **Shared_Library_Prefix**: single value, not inherited from extended project

    Value is the prefix in the name of shared library files. When not declared, the
    prefix is 'lib'.

  .. index:: Attributes - Project Level Attributes; Shared_Library_Suffix

  * **Shared_Library_Suffix**: single value, not inherited from extended project

    Value is the extension of the name of shared library files. When not declared,
    the extension is '.so'.

  .. index:: Attributes - Project Level Attributes; Symbolic_Link_Supported

  * **Symbolic_Link_Supported**: single value, not inherited from extended project

    Indicates if symbolic links are supported on the platform. Only authorized
    case-insensitive values are 'true' and 'false' (the default).

* **Configuration - Libraries**

  .. index:: Attributes - Project Level Attributes; Library_Builder

  * **Library_Builder**: single value, not inherited from extended project

    Value is the path name of the application that is to be used to build
    libraries. Usually the path name of 'gprlib'.

  .. index:: Attributes - Project Level Attributes; Library_Support

  * **Library_Support**: single value, not inherited from extended project

    Indicates the level of support of libraries. Only authorized case-insensitive
    values are 'static_only', 'full' or 'none' (the default).

.. _Package_Binder_Attributes:

Package Binder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  .. index:: Attributes - Package Binder Attributes; Default_Switches

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a language name. Value is the list of switches to be used when binding
    code of the language, if there is no applicable attribute Switches.

  .. index:: Attributes - Package Binder Attributes; Switches

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is either a language name or a source file name. Value is the list of
    switches to be used when binding code. Index is either the source file name of
    the executable to be bound or the language name of the code to be bound.

* **Configuration - Binding**

  .. index:: Attributes - Package Binder Attributes; Driver

  * **Driver**: single value, indexed by a language

    Index is a language name. Value is the name of the application to be used when
    binding code of the language.

  .. index:: Attributes - Package Binder Attributes; Objects_Path

  * **Objects_Path**: single value, indexed by a language

    Index is a language name. Value is the name of the environment variable that
    contains the path for the object directories.

  .. index:: Attributes - Package Binder Attributes; Prefix

  * **Prefix**: single value, indexed by a language

    Index is a language name. Value is a prefix to be used for the binder exchange
    file name for the language. Used to have different binder exchange file names
    when binding different languages.

  .. index:: Attributes - Package Binder Attributes; Required_Switches

  * **Required_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of the required switches to be used
    when binding code of the language.

.. _Package_Builder_Attributes:

Package Builder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Builder Attributes; Default_Switches

* **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

  Index is a language name. Value is the list of builder switches to be used when
  building an executable of the language, if there is no applicable attribute
  Switches.

.. index:: Attributes - Package Builder Attributes; Switches

* **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

  Index is either a language name or a source file name. Value is the list of
  builder switches to be used when building an executable. Index is either the
  source file name of the executable to be built or its language name.

.. index:: Attributes - Package Builder Attributes; Executable

* **Executable**: single value, indexed by a file name

  Index is an executable source file name. Value is the simple file name of the
  executable to be built.

.. index:: Attributes - Package Builder Attributes; Executable_Suffix

* **Executable_Suffix**: single value

  Value is the extension of the file name of executables. The actual default
  value for the extension depends on the host: ``.exe`` on windows, else an empty
  string.

.. index:: Attributes - Package Builder Attributes; Global_Compilation_Switches

* **Global_Compilation_Switches**: list value, indexed by a language, "others" index allowed, configuration concatenable

  Index is a language name. Value is the list of compilation switches to be used
  when building an executable. Index is either the source file name of the
  executable to be built or its language name.

.. index:: Attributes - Package Builder Attributes; Global_Config_File

* **Global_Config_File**: single value, indexed by a language

  Index is a language name. Value is the file name of a configuration file that
  is specified to the compiler when compiling any source of the language in the
  project tree.

.. index:: Attributes - Package Builder Attributes; Global_Configuration_Pragmas

* **Global_Configuration_Pragmas**: single value

  Value is the file name of a configuration pragmas file that is specified to the
  Ada compiler when compiling any Ada source in the project tree.

.. _Package_Clean_Attributes:

Package Clean Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Clean Attributes; Switches

* **Switches**: list value, configuration concatenable

  Taken into account only in the main project. Value is a list of switches to be
  used by the cleaning application.

.. index:: Attributes - Package Clean Attributes; Artifacts_In_Exec_Dir

* **Artifacts_In_Exec_Dir**: list value

  Value is list of file names expressed as regular expressions that are to be
  deleted by gprclean in the exec directory of the main project.

.. index:: Attributes - Package Clean Attributes; Artifacts_In_Object_Dir

* **Artifacts_In_Object_Dir**: list value

  Value is a list of file names expressed as regular expressions that are to be
  deleted by gprclean in the object directory of the project.

.. index:: Attributes - Package Clean Attributes; Object_Artifact_Extensions

* **Object_Artifact_Extensions**: list value, indexed by a language

  Index is a language names. Value is the list of extensions for file names
  derived from source file names that need to be cleaned in the object directory
  of the project.

.. index:: Attributes - Package Clean Attributes; Source_Artifact_Extensions

* **Source_Artifact_Extensions**: list value, indexed by a language

  Index is a language names. Value is the list of extensions for file names
  derived from object file names that need to be cleaned in the object directory
  of the project.

.. _Package_Compiler_Attributes:

Package Compiler Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  .. index:: Attributes - Package Compiler Attributes; Default_Switches

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a language name. Value is a list of switches to be used when invoking
    the compiler for the language for a source of the project, if there is no
    applicable attribute Switches.

  .. index:: Attributes - Package Compiler Attributes; Switches

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used when invoking the compiler for the source or for its language.

  .. index:: Attributes - Package Compiler Attributes; Local_Config_File

  * **Local_Config_File**: single value, indexed by a language

    Index is a language name. Value is the file name of a configuration file that
    is specified to the compiler when compiling any source of the language in the
    project.

  .. index:: Attributes - Package Compiler Attributes; Local_Configuration_Pragmas

  * **Local_Configuration_Pragmas**: single value

    Value is the file name of a configuration pragmas file that is specified to the
    Ada compiler when compiling any Ada source in the project.

* **Configuration - Compiling**

  .. index:: Attributes - Package Compiler Attributes; Driver

  * **Driver**: single value, indexed by a language

    Index is a language name. Value is the name of the executable for the compiler
    of the language.

  .. index:: Attributes - Package Compiler Attributes; Required_Switches

  * **Required_Switches**: list value, indexed by a language, configuration concatenable

    Equivalent to attribute Leading_Required_Switches.

  .. index:: Attributes - Package Compiler Attributes; Dependency_Kind

  * **Dependency_Kind**: single value, indexed by a language

    Index is a language name. Indicates how the dependencies are handled for the
    language. Only authorized case-insensitive values are 'makefile', 'ali_file',
    'ali_closure' or 'none' (the default).

  .. index:: Attributes - Package Compiler Attributes; Language_Kind

  * **Language_Kind**: single value, indexed by a language

    Index is a language name. Indicates the kind of the language, either file based
    or unit based. Only authorized case-insensitive values are 'unit_based' and
    'file_based' (the default).

  .. index:: Attributes - Package Compiler Attributes; Leading_Required_Switches

  * **Leading_Required_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of the minimum switches to be used
    at the beginning of the command line when invoking the compiler for the
    language.

  .. index:: Attributes - Package Compiler Attributes; Multi_Unit_Object_Separator

  * **Multi_Unit_Object_Separator**: single value, indexed by a language

    Index is a language name. Value is the string to be used in the object file
    name before the index of the unit, when compiling a unit in a multi unit source
    of the language.

  .. index:: Attributes - Package Compiler Attributes; Multi_Unit_Switches

  * **Multi_Unit_Switches**: list value, indexed by a language

    Index is a language name. Value is the list of switches to be used to compile a
    unit in a multi unit source of the language. The index of the unit in the
    source is concatenated with the last switches in the list.

  .. index:: Attributes - Package Compiler Attributes; Object_File_Suffix

  * **Object_File_Suffix**: single value, indexed by a language

    Index is a language name. Value is the extension of the object files created by
    the compiler of the language. When not specified, the extension is the default
    one for the platform.

  .. index:: Attributes - Package Compiler Attributes; Object_File_Switches

  * **Object_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to be used by the
    compiler of the language to specify the path name of the object file. When not
    specified, the switch used is '-o'.

  .. index:: Attributes - Package Compiler Attributes; Source_File_Switches

  * **Source_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is a list of switches to be used just before
    the path name of the source to compile when invoking the compiler for a source
    of the language.

  .. index:: Attributes - Package Compiler Attributes; Trailing_Required_Switches

  * **Trailing_Required_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of the minimum switches to be used
    at the end of the command line when invoking the compiler for the language.

* **Configuration - Config Files**

  .. index:: Attributes - Package Compiler Attributes; Config_Body_File_Name

  * **Config_Body_File_Name**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to a body of the language in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_Body_File_Name_Index

  * **Config_Body_File_Name_Index**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to the body a unit in a multi unit source of the
    language in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_Body_File_Name_Pattern

  * **Config_Body_File_Name_Pattern**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration for all bodies of the languages in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_File_Switches

  * **Config_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_File_Unique

  * **Config_File_Unique**: single value, indexed by a language

    Index is a language name. Indicates if there should be only one configuration
    file specified to the compiler of the language. Only authorized
    case-insensitive values are 'true' and 'false' (the default).

  .. index:: Attributes - Package Compiler Attributes; Config_Spec_File_Name

  * **Config_Spec_File_Name**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to a spec of the language in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_Spec_File_Name_Index

  * **Config_Spec_File_Name_Index**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to the spec a unit in a multi unit source of the
    language in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_Spec_File_Name_Pattern

  * **Config_Spec_File_Name_Pattern**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration for all specs of the languages in a configuration file.

* **Configuration - Dependencies**

  .. index:: Attributes - Package Compiler Attributes; Dependency_Driver

  * **Dependency_Driver**: list value, indexed by a language

    Index is a language name. Value is the name of the executable to be used to
    create the dependency file for a source of the language, followed by the
    required switches.

  .. index:: Attributes - Package Compiler Attributes; Dependency_Switches

  * **Dependency_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to be used to specify
    to the compiler the dependency file when the dependency kind of the language is
    file based, and when Dependency_Driver is not specified for the language.

* **Configuration - Search Paths**

  .. index:: Attributes - Package Compiler Attributes; Include_Path

  * **Include_Path**: single value, indexed by a language

    Index is a language name. Value is the name of an environment variable that
    contains the path of all the directories that the compiler of the language may
    search for sources.

  .. index:: Attributes - Package Compiler Attributes; Include_Path_File

  * **Include_Path_File**: single value, indexed by a language

    Index is a language name. Value is the name of an environment variable the
    value of which is the path name of a text file that contains the directories
    that the compiler of the language may search for sources.

  .. index:: Attributes - Package Compiler Attributes; Include_Switches

  * **Include_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language to indicate a directory to look for sources.

  .. index:: Attributes - Package Compiler Attributes; Object_Path_Switches

  * **Object_Path_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language the name of a text file that contains the list of
    object directories. When this attribute is not declared, the text file is not
    created.

* **Configuration - Mapping Files**

  .. index:: Attributes - Package Compiler Attributes; Mapping_Body_Suffix

  * **Mapping_Body_Suffix**: single value, indexed by a language

    Index is a language name. Value is the suffix to be used in a mapping file to
    indicate that the source is a body.

  .. index:: Attributes - Package Compiler Attributes; Mapping_File_Switches

  * **Mapping_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to be used to specify a
    mapping file when invoking the compiler for a source of the language.

  .. index:: Attributes - Package Compiler Attributes; Mapping_Spec_Suffix

  * **Mapping_Spec_Suffix**: single value, indexed by a language

    Index is a language name. Value is the suffix to be used in a mapping file to
    indicate that the source is a spec.

* **Configuration - Response Files**

  .. index:: Attributes - Package Compiler Attributes; Max_Command_Line_Length

  * **Max_Command_Line_Length**: single value

    Value is the maximum number of character in the command line when invoking a
    compiler that supports response files.

  .. index:: Attributes - Package Compiler Attributes; Response_File_Format

  * **Response_File_Format**: single value, indexed by a language

    Indicates the kind of response file to create when the length of the compiling
    command line is too large. The index is the name of the language for the
    compiler. Only authorized case-insensitive values are 'none', 'gnu',
    'object_list', 'gcc_gnu', 'gcc_option_list' and 'gcc_object_list'.

  .. index:: Attributes - Package Compiler Attributes; Response_File_Switches

  * **Response_File_Switches**: list value, indexed by a language, configuration concatenable

    Value is the list of switches to specify a response file for a compiler. The
    index is the name of the language for the compiler.

.. _Package_Gnatls_Attributes:

Package Gnatls Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Gnatls Attributes; Switches

* **Switches**: list value

  Taken into account only in the main project. Value is a list of switches to be
  used when invoking gnatls.

.. _Package_Install_Attributes:

Package Install Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Install Attributes; Prefix

* **Prefix**: single value

  Value is the install destination directory. If the value is a relative path, it
  is taken as relative to the global prefix directory. That is, either the value
  passed to --prefix option or the default installation prefix.

.. index:: Attributes - Package Install Attributes; Active

* **Active**: single value

  Indicates that the project is to be installed or not. Case-insensitive value
  'false' means that the project is not to be installed, all other values mean
  that the project is to be installed.

.. index:: Attributes - Package Install Attributes; Artifacts

* **Artifacts**: list value, indexed by a file name

  An indexed attribute to declare a set of files not part of the sources to be
  installed. The array index is the directory where the file is to be installed.
  If a relative directory then Prefix (see below) is prepended. Note also that if
  the same file name occurs multiple time in the attribute list, the last one
  will be the one installed. If an artifact is not found a warning is displayed.

.. index:: Attributes - Package Install Attributes; Exec_Subdir

* **Exec_Subdir**: single value

  Value is the executables directory or subdirectory of Prefix.

.. index:: Attributes - Package Install Attributes; Install_Name

* **Install_Name**: single value

  Specify the name to use for recording the installation. The default is the
  project name without the extension.

.. index:: Attributes - Package Install Attributes; Install_Project

* **Install_Project**: single value

  Indicates that a project is to be generated and installed. The value is either
  'true' to 'false'. Default is 'true'.

.. index:: Attributes - Package Install Attributes; Lib_Subdir

* **Lib_Subdir**: single value

  Value is library directory or subdirectory of Prefix.

.. index:: Attributes - Package Install Attributes; Mode

* **Mode**: single value

  Value is the installation mode, it is either 'dev' (default) or 'usage'.

.. index:: Attributes - Package Install Attributes; Project_Subdir

* **Project_Subdir**: single value

  Value is the project directory or subdirectory of Prefix.

.. index:: Attributes - Package Install Attributes; Required_Artifacts

* **Required_Artifacts**: list value, indexed by a file name

  As above, but artifacts must be present or an error is reported.

.. index:: Attributes - Package Install Attributes; Side_Debug

* **Side_Debug**: single value

  Indicates that the project's executable and shared libraries are to be stripped
  of the debug symbols. Those debug symbols are written into a side file named
  after the original file with the '.debug' extension added. Case-insensitive
  value 'false' (default) disables this feature. Set it to 'true' to activate.

.. index:: Attributes - Package Install Attributes; Sources_Subdir

* **Sources_Subdir**: single value

  Value is the sources directory or subdirectory of Prefix.

.. _Package_Linker_Attributes:

Package Linker Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  .. index:: Attributes - Package Linker Attributes; Default_Switches

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a language name. Value is a list of switches for the linker when
    linking an executable for a main source of the language, when there is no
    applicable Switches.

  .. index:: Attributes - Package Linker Attributes; Required_Switches

  * **Required_Switches**: list value, configuration concatenable

    Value is a list of switches that are required when invoking the linker to link
    an executable.

  .. index:: Attributes - Package Linker Attributes; Switches

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used when invoking the linker to build an executable for the source or
    for its language.

  .. index:: Attributes - Package Linker Attributes; Leading_Switches

  * **Leading_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used at the beginning of the command line when invoking the linker to
    build an executable for the source or for its language.

  .. index:: Attributes - Package Linker Attributes; Linker_Options

  * **Linker_Options**: list value, configuration concatenable

    This attribute specifies a list of additional switches to be given to the
    linker when linking an executable. It is ignored when defined in the main
    project and taken into account in all other projects that are imported directly
    or indirectly. These switches complement the Linker'Switches defined in the
    main project. This is useful when a particular subsystem depends on an external
    library: adding this dependency as a Linker_Options in the project of the
    subsystem is more convenient than adding it to all the Linker'Switches of the
    main projects that depend upon this subsystem.

  .. index:: Attributes - Package Linker Attributes; Trailing_Switches

  * **Trailing_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used at the end of the command line when invoking the linker to build an
    executable for the source or for its language. These switches may override the
    Required_Switches.

* **Configuration - Linking**

  .. index:: Attributes - Package Linker Attributes; Driver

  * **Driver**: single value

    Value is the name of the linker executable.

* **Configuration - Response Files**

  .. index:: Attributes - Package Linker Attributes; Max_Command_Line_Length

  * **Max_Command_Line_Length**: single value

    Value is the maximum number of character in the command line when invoking the
    linker to link an executable.

  .. index:: Attributes - Package Linker Attributes; Response_File_Format

  * **Response_File_Format**: single value

    Indicates the kind of response file to create when the length of the linking
    command line is too large. Only authorized case-insensitive values are 'none',
    'gnu', 'object_list', 'gcc_gnu', 'gcc_option_list' and 'gcc_object_list'.

  .. index:: Attributes - Package Linker Attributes; Response_File_Switches

  * **Response_File_Switches**: list value, configuration concatenable

    Value is the list of switches to specify a response file to the linker.

.. _Package_Naming_Attributes:

Package Naming Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Naming Attributes; Body

* **Body**: single value, indexed by a unit

  Index is a unit name. Value is the file name of the body of the unit.

.. index:: Attributes - Package Naming Attributes; Body_Suffix

* **Body_Suffix**: single value, indexed by a language

  Index is a language name. Value is the extension of file names for bodies of
  the language.

.. index:: Attributes - Package Naming Attributes; Casing

* **Casing**: single value

  Indicates the casing of sources of the Ada language. Only authorized
  case-insensitive values are 'lowercase', 'uppercase' and 'mixedcase'.

.. index:: Attributes - Package Naming Attributes; Dot_Replacement

* **Dot_Replacement**: single value

  Value is the string that replace the dot of unit names in the source file names
  of the Ada language.

.. index:: Attributes - Package Naming Attributes; Implementation

* **Implementation**: single value, indexed by a unit

  Equivalent to attribute Body.

.. index:: Attributes - Package Naming Attributes; Implementation_Exceptions

* **Implementation_Exceptions**: list value, indexed by a language

  Index is a language name. Value is a list of bodies for the language that do
  not necessarily follow the naming scheme for the language and that may or may
  not be found in the source directories of the project.

.. index:: Attributes - Package Naming Attributes; Implementation_Suffix

* **Implementation_Suffix**: single value, indexed by a language

  Equivalent to attribute Body_Suffix.

.. index:: Attributes - Package Naming Attributes; Separate_Suffix

* **Separate_Suffix**: single value

  Value is the extension of file names for subunits of Ada.

.. index:: Attributes - Package Naming Attributes; Spec

* **Spec**: single value, indexed by a unit

  Index is a unit name. Value is the file name of the spec of the unit.

.. index:: Attributes - Package Naming Attributes; Spec_Suffix

* **Spec_Suffix**: single value, indexed by a language

  Index is a language name. Value is the extension of file names for specs of the
  language.

.. index:: Attributes - Package Naming Attributes; Specification

* **Specification**: single value, indexed by a unit

  Equivalent to attribute Spec.

.. index:: Attributes - Package Naming Attributes; Specification_Exceptions

* **Specification_Exceptions**: list value, indexed by a language

  Index is a language name. Value is a list of specs for the language that do not
  necessarily follow the naming scheme for the language and that may or may not
  be found in the source directories of the project.

.. index:: Attributes - Package Naming Attributes; Specification_Suffix

* **Specification_Suffix**: single value, indexed by a language

  Equivalent to attribute Spec_Suffix.

.. _Package_Remote_Attributes:

Package Remote Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Remote Attributes; Excluded_Patterns

* **Excluded_Patterns**: list value

  Set of patterns to ignore when synchronizing sources from the build master to
  the slaves. A set of predefined patterns are supported (e.g. \*.o, \*.ali,
  \*.exe, etc.), this attribute makes it possible to add some more patterns.

.. index:: Attributes - Package Remote Attributes; Included_Patterns

* **Included_Patterns**: list value

  If this attribute is defined it sets the patterns to synchronized from the
  master to the slaves. It is incompatible with Excluded_Patterns, that is it is
  an error to define both.

.. index:: Attributes - Package Remote Attributes; Included_Artifact_Patterns

* **Included_Artifact_Patterns**: list value

  If this attribute is defined it sets the patterns of compilation artifacts to
  synchronized from the slaves to the build master. This attribute replace the
  default hard-coded patterns.

.. index:: Attributes - Package Remote Attributes; Root_Dir

* **Root_Dir**: single value

  Value is the root directory used by the slave machines.

