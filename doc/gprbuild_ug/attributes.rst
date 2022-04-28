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

* **configuration concatenable**

  For a string list attribute, the final value if the attribute is declared
  in both the configuration project and the user project is the concatenation
  of the two value, configuration then user.

* **inheritance**

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

  * **Archive_Builder**: list value, not inherited from extended project

    Value is the name of the application to be used to create a static library
    (archive), followed by the options to be used.

  * **Archive_Builder_Append_Option**: list value, not inherited from extended project

    Value is the list of options to be used when invoking the archive builder to
    add project files into an archive.

  * **Archive_Indexer**: list value, not inherited from extended project

    Value is the name of the archive indexer, followed by the required options.

  * **Archive_Suffix**: single value, not inherited from extended project

    Value is the extension of archives. When not declared, the extension is '.a'.

  * **Library_Partial_Linker**: list value, not inherited from extended project

    Value is the name of the partial linker executable, followed by the required
    options.

* **Directories**

  * **Create_Missing_Dirs**: single value

    Indicates if the missing object, library and executable directories should be
    created automatically by the project-aware tool. Taken into account only in the
    main project. Only authorized case-insensitive values are 'true' and 'false'.

  * **Exec_Dir**: single value, not inherited from extended project

    Indicates the exec directory for the project, that is the directory where the
    executables are.

  * **Ignore_Source_Sub_Dirs**: list value, not inherited from extended project

    Value is a list of simple names or patterns for subdirectories that are removed
    from the list of source directories, including their subdirectories.

  * **Inherit_Source_Path**: list value, indexed by a language

    Index is a language name. Value is a list of language names. Indicates that in
    the source search path of the index language the source directories of the
    languages in the list should be included.

  * **Object_Dir**: single value, not inherited from extended project

    Indicates the object directory for the project.

  * **Source_Dirs**: list value, not inherited from extended project

    The list of source directories of the project.

* **Configuration - General**

  * **Default_Language**: single value, not inherited from extended project

    Value is the case-insensitive name of the language of a project when attribute
    Languages is not specified.

  * **Object_Generated**: single value, indexed by a language

    Index is a language name. Indicates if invoking the compiler for a language
    produces an object file. Only authorized case-insensitive values are 'false'
    and 'true' (the default).

  * **Objects_Linked**: single value, indexed by a language

    Index is a language name. Indicates if the object files created by the compiler
    for a language need to be linked in the executable. Only authorized
    case-insensitive values are 'false' and 'true' (the default).

  * **Required_Toolchain_Version**: single value, indexed by a language

    Index is a language name. Specify the value expected for the Toolchain_Version
    attribute for this language, typically provided by an auto-generated
    configuration project. If Required_Toolchain_Version and Toolchain_Version do
    not match, the project processing aborts with an error.

  * **Run_Path_Option**: list value, not inherited from extended project

    Value is the list of switches to be used when specifying the run path option in
    an executable.

  * **Run_Path_Origin**: single value, not inherited from extended project

    Value is the string that may replace the path name of the executable directory
    in the run path options.

  * **Runtime**: single value, indexed by a language

    Index is a language name. Indicates the runtime directory that is to be used
    when using the compiler of the language. Taken into account only in the main
    project, or its extended projects if any. Note that when the runtime is
    specified for a language on the command line (usually with a switch --RTS), the
    value of attribute reference 'Runtime for this language is the one specified on
    the command line.

  * **Runtime_Dir**: single value, indexed by a language

    Index is a language name. Value is the path name of the runtime directory for
    the language.

  * **Runtime_Library_Dir**: single value, indexed by a language, not inherited from extended project

    Index is a language name. Value is the path name of the directory where the
    runtime libraries are located. This attribute is obsolete.

  * **Runtime_Source_Dir**: single value, indexed by a language

    Index is a language name. Value is the path name of the directory where the
    sources of runtime libraries are located. This attribute is obsolete.

  * **Runtime_Source_Dirs**: single value, indexed by a language

    Index is a language name. Value is the path names of the directories where the
    sources of runtime libraries are located. This attribute is not normally
    declared.

  * **Separate_Run_Path_Options**: single value, not inherited from extended project

    Indicates if there may be several run path options specified when linking an
    executable. Only authorized case-insensitive values are 'true' or 'false' (the
    default).

  * **Target**: single value

    Value is the name of the target platform. Taken into account only in the main
    project. ote that when the target is specified on the command line (usually
    with a switch --target=), the value of attribute reference 'Target is the one
    specified on the command line.

  * **Toolchain_Version**: single value, indexed by a language

    Index is a language name. Specify the version of a toolchain for a language.

  * **Toolchain_Name**: single value, indexed by a language

    Index is a language name. Indicates the toolchain name that is to be used when
    using the compiler of the language. Taken into account only in the main
    project, or its extended projects if any.

  * **Toolchain_Description**: single value, indexed by a language

    Obsolescent. No longer used.

* **Source Files**

  * **Excluded_Source_Files**: list value, not inherited from extended project

    Value is a list of simple file names that are not sources of the project.
    Allows to remove sources that are inherited or found in the source directories
    and that match the naming scheme.

  * **Excluded_Source_List_File**: single value, not inherited from extended project

    Value is a text file name that contains a list of file simple names that are
    not sources of the project.

  * **Interfaces**: list value

    Value is a list of file names that constitutes the interfaces of the project.

  * **Locally_Removed_Files**: list value, not inherited from extended project

    Obsolescent. Equivalent to Excluded_Source_Files.

  * **Source_Files**: list value, not inherited from extended project

    Value is a list of source file simple names.

  * **Source_List_File**: single value, not inherited from extended project

    Value is a text file name that contains a list of source file simple names, one
    on each line.

* **Aggregate Projects**

  * **External**: single value, indexed by an external reference

    Index is the name of an external reference. Value is the value of the external
    reference to be used when parsing the aggregated projects.

  * **Project_Files**: list value, not inherited from extended project

    Value is the list of aggregated projects.

  * **Project_Path**: list value, not inherited from extended project

    Value is a list of directories that are added to the project search path when
    looking for the aggregated projects.

* **General**

  * **Externally_Built**: single value, not inherited from extended project

    Indicates if the project is externally built. Only case-insensitive values
    allowed are 'true' and 'false', the default.

  * **Languages**: set value, case-insensitive, concatenated with extended value

    The list of languages of the sources of the project.

  * **Main**: list value

    The list of main sources for the executables.

  * **Name**: single value, read-only, not inherited from extended project

    The name of the project.

  * **Project_Dir**: single value, read-only, not inherited from extended project

    The path name of the project directory.

  * **Roots**: list value, indexed by a source glob

    The index is the file name of an executable source. Indicates the list of units
    from the main project that need to be bound and linked with their closures with
    the executable. The index is either a file name, a language name or '*'. The
    roots for an executable source are those in Roots with an index that is the
    executable source file name, if declared. Otherwise, they are those in Roots
    with an index that is the language name of the executable source, if present.
    Otherwise, they are those in Roots ('*'), if declared. If none of these three
    possibilities are declared, then there are no roots for the executable source.

  * **Warning_Message**: single value

    Causes gprbuild to emit a user-defined warning message.

* **Libraries**

  * **Leading_Library_Options**: list value, configuration concatenable, not inherited from extended project

    Value is a list of options that are to be used at the beginning of the command
    line when linking a shared library.

  * **Library_Auto_Init**: single value, not inherited from extended project

    Indicates if a Stand-Alone Library is auto-initialized. Only authorized
    case-insensitive values are 'true' and 'false'.

  * **Library_Dir**: single value

    Value is the name of the library directory. This attribute needs to be declared
    for each library project.

  * **Library_Encapsulated_Options**: list value, configuration concatenable, not inherited from extended project

    Value is a list of options that need to be used when linking an encapsulated
    Stand-Alone Library.

  * **Library_Encapsulated_Supported**: single value, not inherited from extended project

    Indicates if encapsulated Stand-Alone Libraries are supported. Only authorized
    case-insensitive values are 'true' and 'false' (the default).

  * **Library_Interface**: set value, case-sensitive

    Value is the list of unit names that constitutes the interfaces of a
    Stand-Alone Library project.

  * **Library_Kind**: single value, not inherited from extended project

    Specifies the kind of library: static library (archive) or shared library.
    Case-insensitive values must be one of 'static' for archives (the default),
    'static-pic' for archives of Position Independent Code, or 'dynamic' or
    'relocatable' for shared libraries.

  * **Library_Name**: single value

    Value is the name of the library. This attribute needs to be declared or
    inherited for each library project.

  * **Library_Options**: list value, configuration concatenable, not inherited from extended project

    Value is a list of options that are to be used when linking a shared library.

  * **Library_Reference_Symbol_File**: single value, not inherited from extended project

    Value is the name of the reference symbol file.

  * **Library_Rpath_Options**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is a list of options for an invocation of the
    compiler of the language. This invocation is done for a shared library project
    with sources of the language. The output of the invocation is the path name of
    a shared library file. The directory name is to be put in the run path option
    switch when linking the shared library for the project.

  * **Library_Src_Dir**: single value, not inherited from extended project

    Value is the name of the directory where copies of the sources of the
    interfaces of a Stand-Alone Library are to be copied.

  * **Library_Standalone**: single value, not inherited from extended project

    Specifies if a Stand-Alone Library (SAL) is encapsulated or not. Only
    authorized case-insensitive values are 'standard' for non encapsulated SALs,
    'encapsulated' for encapsulated SALs or 'no' for non SAL library project.

  * **Library_Symbol_File**: single value, not inherited from extended project

    Value is the name of the library symbol file.

  * **Library_Symbol_Policy**: single value, not inherited from extended project

    Indicates the symbol policy kind. Only authorized case-insensitive values are
    'restricted', 'unrestricted'.

  * **Library_Version**: single value, not inherited from extended project

    Value is the name of the library file.

* **Configuration - Shared Libraries**

  * **Library_Auto_Init_Supported**: single value, not inherited from extended project

    Indicates if auto-initialization of Stand-Alone Libraries is supported. Only
    authorized case-insensitive values are 'true' and 'false' (the default).

  * **Library_Install_Name_Option**: single value, not inherited from extended project

    Value is the name of the option that needs to be used, concatenated with the
    path name of the library file, when linking a shared library.

  * **Library_Major_Minor_Id_Supported**: single value, not inherited from extended project

    Indicates if major and minor ids for shared library names are supported on the
    platform. Only authorized case-insensitive values are 'true' and 'false' (the
    default).

  * **Library_Version_Switches**: list value, configuration concatenable, not inherited from extended project

    Value is the list of switches to specify a internal name for a shared library.

  * **Shared_Library_Minimum_Switches**: list value, not inherited from extended project

    Value is the list of required switches when linking a shared library.

  * **Shared_Library_Prefix**: single value, not inherited from extended project

    Value is the prefix in the name of shared library files. When not declared, the
    prefix is 'lib'.

  * **Shared_Library_Suffix**: single value, not inherited from extended project

    Value is the extension of the name of shared library files. When not declared,
    the extension is '.so'.

  * **Symbolic_Link_Supported**: single value, not inherited from extended project

    Indicates if symbolic links are supported on the platform. Only authorized
    case-insensitive values are 'true' and 'false' (the default).

* **Configuration - Libraries**

  * **Library_Builder**: single value, not inherited from extended project

    Value is the path name of the application that is to be used to build
    libraries. Usually the path name of 'gprlib'.

  * **Library_Support**: single value, not inherited from extended project

    Indicates the level of support of libraries. Only authorized case-insensitive
    values are 'static_only', 'full' or 'none' (the default).

.. _Package_Binder_Attributes:

Package Binder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a language name. Value is the list of switches to be used when binding
    code of the language, if there is no applicable attribute Switches.

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is either a language name or a source file name. Value is the list of
    switches to be used when binding code. Index is either the source file name of
    the executable to be bound or the language name of the code to be bound.

* **Configuration - Binding**

  * **Driver**: single value, indexed by a language

    Index is a language name. Value is the name of the application to be used when
    binding code of the language.

  * **Objects_Path**: single value, indexed by a language

    Index is a language name. Value is the name of the environment variable that
    contains the path for the object directories.

  * **Prefix**: single value, indexed by a language

    Index is a language name. Value is a prefix to be used for the binder exchange
    file name for the language. Used to have different binder exchange file names
    when binding different languages.

  * **Required_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of the required switches to be used
    when binding code of the language.

.. _Package_Builder_Attributes:

Package Builder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

  Index is a language name. Value is the list of builder switches to be used when
  building an executable of the language, if there is no applicable attribute
  Switches.

* **Executable**: single value, indexed by a file name

  Index is an executable source file name. Value is the simple file name of the
  executable to be built.

* **Executable_Suffix**: single value

  Value is the extension of the file names of executable. When not specified, the
  extension is the default extension of executables on the platform.

* **Global_Compilation_Switches**: list value, indexed by a language, "others" index allowed, configuration concatenable

  Index is a language name. Value is the list of compilation switches to be used
  when building an executable. Index is either the source file name of the
  executable to be built or its language name.

* **Global_Config_File**: single value, indexed by a language

  Index is a language name. Value is the file name of a configuration file that
  is specified to the compiler when compiling any source of the language in the
  project tree.

* **Global_Configuration_Pragmas**: single value

  Value is the file name of a configuration pragmas file that is specified to the
  Ada compiler when compiling any Ada source in the project tree.

* **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

  Index is either a language name or a source file name. Value is the list of
  builder switches to be used when building an executable. Index is either the
  source file name of the executable to be built or its language name.

.. _Package_Clean_Attributes:

Package Clean Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

* **Artifacts_In_Exec_Dir**: list value

  Value is list of file names expressed as regular expressions that are to be
  deleted by gprclean in the exec directory of the main project.

* **Artifacts_In_Object_Dir**: list value

  Value is a list of file names expressed as regular expressions that are to be
  deleted by gprclean in the object directory of the project.

* **Object_Artifact_Extensions**: list value, indexed by a language

  Index is a language names. Value is the list of extensions for file names
  derived from source file names that need to be cleaned in the object directory
  of the project.

* **Source_Artifact_Extensions**: list value, indexed by a language

  Index is a language names. Value is the list of extensions for file names
  derived from object file names that need to be cleaned in the object directory
  of the project.

* **Switches**: list value, configuration concatenable

  Taken into account only in the main project. Value is a list of switches to be
  used by the cleaning application.

.. _Package_Compiler_Attributes:

Package Compiler Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Configuration - Config Files**

  * **Config_Body_File_Name**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to a body of the language in a configuration file.

  * **Config_Body_File_Name_Index**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to the body a unit in a multi unit source of the
    language in a configuration file.

  * **Config_Body_File_Name_Pattern**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration for all bodies of the languages in a configuration file.

  * **Config_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language a configuration file.

  * **Config_File_Unique**: single value, indexed by a language

    Index is a language name. Indicates if there should be only one configuration
    file specified to the compiler of the language. Only authorized
    case-insensitive values are 'true' and 'false' (the default).

  * **Config_Spec_File_Name**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to a spec of the language in a configuration file.

  * **Config_Spec_File_Name_Index**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to the spec a unit in a multi unit source of the
    language in a configuration file.

  * **Config_Spec_File_Name_Pattern**: single value, indexed by a language

    Index is a language name. Value is the template to be used to indicate a
    configuration for all specs of the languages in a configuration file.

* **General**

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a language name. Value is a list of switches to be used when invoking
    the compiler for the language for a source of the project, if there is no
    applicable attribute Switches.

  * **Local_Config_File**: single value, indexed by a language

    Index is a language name. Value is the file name of a configuration file that
    is specified to the compiler when compiling any source of the language in the
    project.

  * **Local_Configuration_Pragmas**: single value

    Value is the file name of a configuration pragmas file that is specified to the
    Ada compiler when compiling any Ada source in the project.

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used when invoking the compiler for the source or for its language.

* **Configuration - Dependencies**

  * **Dependency_Driver**: list value, indexed by a language

    Index is a language name. Value is the name of the executable to be used to
    create the dependency file for a source of the language, followed by the
    required switches.

  * **Dependency_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to be used to specify
    to the compiler the dependency file when the dependency kind of the language is
    file based, and when Dependency_Driver is not specified for the language.

* **Configuration - Compiling**

  * **Dependency_Kind**: single value, indexed by a language

    Index is a language name. Indicates how the dependencies are handled for the
    language. Only authorized case-insensitive values are 'makefile', 'ali_file',
    'ali_closure' or 'none' (the default).

  * **Driver**: single value, indexed by a language

    Index is a language name. Value is the name of the executable for the compiler
    of the language.

  * **Language_Kind**: single value, indexed by a language

    Index is a language name. Indicates the kind of the language, either file based
    or unit based. Only authorized case-insensitive values are 'unit_based' and
    'file_based' (the default).

  * **Leading_Required_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of the minimum switches to be used
    at the beginning of the command line when invoking the compiler for the
    language.

  * **Multi_Unit_Object_Separator**: single value, indexed by a language

    Index is a language name. Value is the string to be used in the object file
    name before the index of the unit, when compiling a unit in a multi unit source
    of the language.

  * **Multi_Unit_Switches**: list value, indexed by a language

    Index is a language name. Value is the list of switches to be used to compile a
    unit in a multi unit source of the language. The index of the unit in the
    source is concatenated with the last switches in the list.

  * **Object_File_Suffix**: single value, indexed by a language

    Index is a language name. Value is the extension of the object files created by
    the compiler of the language. When not specified, the extension is the default
    one for the platform.

  * **Object_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to be used by the
    compiler of the language to specify the path name of the object file. When not
    specified, the switch used is '-o'.

  * **Required_Switches**: list value, indexed by a language, configuration concatenable

    Equivalent to attribute Leading_Required_Switches.

  * **Source_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is a list of switches to be used just before
    the path name of the source to compile when invoking the compiler for a source
    of the language.

  * **Trailing_Required_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of the minimum switches to be used
    at the end of the command line when invoking the compiler for the language.

* **Configuration - Search Paths**

  * **Include_Path**: single value, indexed by a language

    Index is a language name. Value is the name of an environment variable that
    contains the path of all the directories that the compiler of the language may
    search for sources.

  * **Include_Path_File**: single value, indexed by a language

    Index is a language name. Value is the name of an environment variable the
    value of which is the path name of a text file that contains the directories
    that the compiler of the language may search for sources.

  * **Include_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language to indicate a directory to look for sources.

  * **Object_Path_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language the name of a text file that contains the list of
    object directories. When this attribute is not declared, the text file is not
    created.

* **Configuration - Mapping Files**

  * **Mapping_Body_Suffix**: single value, indexed by a language

    Index is a language name. Value is the suffix to be used in a mapping file to
    indicate that the source is a body.

  * **Mapping_File_Switches**: list value, indexed by a language, configuration concatenable

    Index is a language name. Value is the list of switches to be used to specify a
    mapping file when invoking the compiler for a source of the language.

  * **Mapping_Spec_Suffix**: single value, indexed by a language

    Index is a language name. Value is the suffix to be used in a mapping file to
    indicate that the source is a spec.

* **Configuration - Response Files**

  * **Max_Command_Line_Length**: single value

    Value is the maximum number of character in the command line when invoking a
    compiler that supports response files.

  * **Response_File_Format**: single value, indexed by a language

    Indicates the kind of response file to create when the length of the compiling
    command line is too large. The index is the name of the language for the
    compiler. Only authorized case-insensitive values are 'none', 'gnu',
    'object_list', 'gcc_gnu', 'gcc_option_list' and 'gcc_object_list'.

  * **Response_File_Switches**: list value, indexed by a language, configuration concatenable

    Value is the list of switches to specify a response file for a compiler. The
    index is the name of the language for the compiler.

.. _Package_Gnatls_Attributes:

Package Gnatls Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Switches**: list value

  Taken into account only in the main project. Value is a list of switches to be
  used when invoking gnatls.

.. _Package_Install_Attributes:

Package Install Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Active**: single value

  Indicates that the project is to be installed or not. Case-insensitive value
  'false' means that the project is not to be installed, all other values mean
  that the project is to be installed.

* **Artifacts**: list value, indexed by a file name

  An indexed attribute to declare a set of files not part of the sources to be
  installed. The array index is the directory where the file is to be installed.
  If a relative directory then Prefix (see below) is prepended. Note also that if
  the same file name occurs multiple time in the attribute list, the last one
  will be the one installed. If an artifact is not found a warning is displayed.

* **Exec_Subdir**: single value

  Value is the executables directory or subdirectory of Prefix.

* **Install_Name**: single value

  Specify the name to use for recording the installation. The default is the
  project name without the extension.

* **Install_Project**: single value

  Indicates that a project is to be generated and installed. The value is either
  'true' to 'false'. Default is 'true'.

* **Lib_Subdir**: single value

  Value is library directory or subdirectory of Prefix.

* **Mode**: single value

  Value is the installation mode, it is either dev (default) or usage.

* **Prefix**: single value

  Value is the install destination directory. If the value is a relative path, it
  is taken as relative to the global prefix directory. That is, either the value
  passed to --prefix option or the default installation prefix.

* **Project_Subdir**: single value

  Value is the project directory or subdirectory of Prefix.

* **Required_Artifacts**: list value, indexed by a file name

  As above, but artifacts must be present or an error is reported.

* **Side_Debug**: single value

  Indicates that the project's executable and shared libraries are to be stripped
  of the debug symbols. Those debug symbols are written into a side file named
  after the original file with the '.debug' extension added. Case-insensitive
  value 'false' (default) disables this feature. Set it to 'true' to activate.

* **Sources_Subdir**: single value

  Value is the sources directory or subdirectory of Prefix.

.. _Package_Linker_Attributes:

Package Linker Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a language name. Value is a list of switches for the linker when
    linking an executable for a main source of the language, when there is no
    applicable Switches.

  * **Leading_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used at the beginning of the command line when invoking the linker to
    build an executable for the source or for its language.

  * **Linker_Options**: list value, configuration concatenable

    This attribute specifies a list of additional switches to be given to the
    linker when linking an executable. It is ignored when defined in the main
    project and taken into account in all other projects that are imported directly
    or indirectly. These switches complement the Linker'Switches defined in the
    main project. This is useful when a particular subsystem depends on an external
    library: adding this dependency as a Linker_Options in the project of the
    subsystem is more convenient than adding it to all the Linker'Switches of the
    main projects that depend upon this subsystem.

  * **Required_Switches**: list value, configuration concatenable

    Value is a list of switches that are required when invoking the linker to link
    an executable.

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used when invoking the linker to build an executable for the source or
    for its language.

  * **Trailing_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Index is a source file name or a language name. Value is the list of switches
    to be used at the end of the command line when invoking the linker to build an
    executable for the source or for its language. These switches may override the
    Required_Switches.

* **Configuration - Linking**

  * **Driver**: single value

    Value is the name of the linker executable.

* **Configuration - Response Files**

  * **Max_Command_Line_Length**: single value

    Value is the maximum number of character in the command line when invoking the
    linker to link an executable.

  * **Response_File_Format**: single value

    Indicates the kind of response file to create when the length of the linking
    command line is too large. Only authorized case-insensitive values are 'none',
    'gnu', 'object_list', 'gcc_gnu', 'gcc_option_list' and 'gcc_object_list'.

  * **Response_File_Switches**: list value, configuration concatenable

    Value is the list of switches to specify a response file to the linker.

.. _Package_Naming_Attributes:

Package Naming Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Body**: single value, indexed by a unit

  Index is a unit name. Value is the file name of the body of the unit.

* **Body_Suffix**: single value, indexed by a language

  Index is a language name. Value is the extension of file names for bodies of
  the language.

* **Casing**: single value

  Indicates the casing of sources of the Ada language. Only authorized
  case-insensitive values are 'lowercase', 'uppercase' and 'mixedcase'.

* **Dot_Replacement**: single value

  Value is the string that replace the dot of unit names in the source file names
  of the Ada language.

* **Implementation**: single value, indexed by a unit

  Equivalent to attribute Body.

* **Implementation_Exceptions**: list value, indexed by a language

  Index is a language name. Value is a list of bodies for the language that do
  not necessarily follow the naming scheme for the language and that may or may
  not be found in the source directories of the project.

* **Implementation_Suffix**: single value, indexed by a language

  Equivalent to attribute Body_Suffix.

* **Separate_Suffix**: single value

  Value is the extension of file names for subunits of Ada.

* **Spec**: single value, indexed by a unit

  Index is a unit name. Value is the file name of the spec of the unit.

* **Spec_Suffix**: single value, indexed by a language

  Index is a language name. Value is the extension of file names for specs of the
  language.

* **Specification**: single value, indexed by a unit

  Equivalent to attribute Spec.

* **Specification_Exceptions**: list value, indexed by a language

  Index is a language name. Value is a list of specs for the language that do not
  necessarily follow the naming scheme for the language and that may or may not
  be found in the source directories of the project.

* **Specification_Suffix**: single value, indexed by a language

  Equivalent to attribute Spec_Suffix.

.. _Package_Remote_Attributes:

Package Remote Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Excluded_Patterns**: list value

  Set of patterns to ignore when synchronizing sources from the build master to
  the slaves. A set of predefined patterns are supported (e.g. \*.o, \*.ali,
  \*.exe, etc.), this attribute makes it possible to add some more patterns.

* **Included_Patterns**: list value

  If this attribute is defined it sets the patterns to synchronized from the
  master to the slaves. It is exclusive with Excluded_Patterns, that is it is an
  error to define both.

* **Included_Artifact_Patterns**: list value

  If this attribute is defined it sets the patterns of compilation artifacts to
  synchronized from the slaves to the build master. This attribute replace the
  default hard-coded patterns.

* **Root_Dir**: single value

  Value is the root directory used by the slave machines.

