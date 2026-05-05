..
  THIS FILE IS AUTO-GENERATED. DO NOT EDIT.
  If you want to edit its content, either edit :
    - Introduction                        : ./__python__/metadata/attributes_introduction.rst
    - Attribute information & description : directly from libGPR2 source code
  then run `make docgen` in gpr2 root directory to generate the wanted version of this document

.. index:: Attribute

.. _Attributes:

Attributes
----------

Attributes communicate build properties to GPR tools. They are declared with
the ``for … use`` syntax described in :ref:`Project_File_Language`; this
chapter lists all predefined attributes and their semantics.

**Default values**

Every attribute has a default value that applies when no declaration is
present. The default is indicated in each attribute's description below.

**Interaction with the configuration project**

When an attribute is declared in the configuration project but not in the
user project, the user project inherits the configuration value.

When a single-value attribute is declared in both, the user project's
declaration takes precedence.

For list attributes marked **configuration concatenable**, the final value
is the concatenation of the configuration project's value followed by the
user project's value.

**Reading attribute values**

Attribute values may be referenced in expressions anywhere in a project file.
If an attribute has not been set, its value defaults as described in each
attribute's entry. For the reference syntax see :ref:`Project_File_Language`.

**How to read the attribute descriptions**

Each attribute entry below indicates:

* **Type** — ``single`` (string) or ``list`` (string list).
* **Read-only** — the attribute is set by the build system; user declarations
  are forbidden.
* **Indexed by** — the kind of index accepted; see :ref:`Project_File_Language`
  for the full description of each index kind. Possible values:

  * *language* — case-insensitive language identifier
  * *file name* — simple file name without directory components
  * *source glob* — simple file name or glob pattern; case sensitivity is
    host-dependent
  * *source glob or language* — either a source glob or a language identifier;
    resolution priority: exact file name, glob, language, ``others``
  * *unit* — Ada unit name, case-insensitive
  * *string* — arbitrary string key
  * *external reference* — name of an external variable

* **Others index allowed** — the ``others`` index is accepted as a catch-all
  for this attribute.
* **Configuration concatenable** — for list attributes, the final value is the
  configuration value concatenated with the user value.
* **Inheritance** — by default, attributes are inherited from extended projects.
  Deviations are noted as *not inherited from extended* or *concatenated from
  extended*.

In addition to the attributes listed here, individual tools may define their
own attributes in standard or tool-specific packages; refer to each tool's
documentation.


.. _Project_Level_Attributes:

Project Level Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

* **Configuration - Archives**

  .. index:: Attributes - Project Level Attributes; Archive_Builder

  * **Archive_Builder**: list value, not inherited from extended project

    .. warning::

    	Empty value is not supported yet in non GNATcoll.Project-based tools

    Name of the application used to create a static library (archive), followed by
    its required options. If the value is empty, the object files listed in the
    archive recipe are copied to the library directory instead.

  .. index:: Attributes - Project Level Attributes; Archive_Builder_Append_Option

  * **Archive_Builder_Append_Option**: list value, not inherited from extended project

    Options passed to the archive builder when appending files to an existing
    archive.

  .. index:: Attributes - Project Level Attributes; Archive_Indexer

  * **Archive_Indexer**: list value, not inherited from extended project

    Name of the archive indexer executable, followed by its required options.

  .. index:: Attributes - Project Level Attributes; Archive_Prefix

  * **Archive_Prefix**: single value, not inherited from extended project

    Prefix for archive file names. Defaults to ``"lib"``.

  .. index:: Attributes - Project Level Attributes; Archive_Suffix

  * **Archive_Suffix**: single value, not inherited from extended project

    Extension for archive file names. Defaults to ``".a"``.

  .. index:: Attributes - Project Level Attributes; Library_Partial_Linker

  * **Library_Partial_Linker**: list value, not inherited from extended project

    Name of the partial linker executable, followed by its required options. An
    empty list disables partial linking.

* **Directories**

  .. index:: Attributes - Project Level Attributes; Create_Missing_Dirs

  * **Create_Missing_Dirs**: single value

    Applies to the main project only. When set to ``"true"``, automatically creates
    missing object, library, and executable directories. Accepted values
    (case-insensitive): ``"true"`` or ``"false"``.

  .. index:: Attributes - Project Level Attributes; Exec_Dir

  * **Exec_Dir**: single value, not inherited from extended project

    Directory where executables are placed.

  .. index:: Attributes - Project Level Attributes; Gpr_Registry_Dirs

  * **Gpr_Registry_Dirs**: list value, not inherited from extended project

    List of directories containing JSON files with external package and attribute
    definitions. All JSON files found in the listed directories are loaded at
    project tree initialization.

  .. index:: Attributes - Project Level Attributes; Ignore_Source_Sub_Dirs

  * **Ignore_Source_Sub_Dirs**: list value, not inherited from extended project

    List of simple names or patterns for subdirectories to exclude from the source
    directory list, including their own subdirectories.

  .. index:: Attributes - Project Level Attributes; Inherit_Source_Path

  * **Inherit_Source_Path**: list value, indexed by a language

    Indexed by language name. Lists additional languages whose source directories
    are included in the source search path of the indexed language.

  .. index:: Attributes - Project Level Attributes; Object_Dir

  * **Object_Dir**: single value, not inherited from extended project

    Directory where the compiler places object files.

  .. index:: Attributes - Project Level Attributes; Source_Dirs

  * **Source_Dirs**: list value, not inherited from extended project

    List of source directories for the project.

* **Configuration - General**

  .. index:: Attributes - Project Level Attributes; Config_Prj_File

  * **Config_Prj_File**: single value

    The main configuration project file.

  .. index:: Attributes - Project Level Attributes; Default_Language

  * **Default_Language**: single value, not inherited from extended project

    Default language for the project when ``Languages`` is not declared. The value
    is a case-insensitive language name.

  .. index:: Attributes - Project Level Attributes; Disable_Linking

  * **Disable_Linking**: single value, not inherited from extended project

    Indicates whether linking is disabled on the platform. Accepted values
    (case-insensitive): ``"true"`` or ``"false"`` (default).

  .. index:: Attributes - Project Level Attributes; Object_Generated

  * **Object_Generated**: single value, indexed by a language

    Indexed by language name. Indicates whether compiling a source of the language
    produces an object file. Accepted values (case-insensitive): ``"true"``
    (default) or ``"false"``.

  .. index:: Attributes - Project Level Attributes; Objects_Linked

  * **Objects_Linked**: single value, indexed by a language

    Indexed by language name. Indicates whether object files produced for the
    language are linked into executables. Accepted values (case-insensitive):
    ``"true"`` (default) or ``"false"``.

  .. index:: Attributes - Project Level Attributes; Required_Toolchain_Version

  * **Required_Toolchain_Version**: single value, indexed by a language

    Indexed by language name. Expected value for ``Toolchain_Version`` for the
    language, typically set in an auto-generated configuration project. Project
    processing aborts if ``Required_Toolchain_Version`` and ``Toolchain_Version``
    do not match.

  .. index:: Attributes - Project Level Attributes; Run_Path_Option

  * **Run_Path_Option**: list value, not inherited from extended project

    Switches used to specify the run-path option when linking an executable.

  .. index:: Attributes - Project Level Attributes; Run_Path_Origin

  * **Run_Path_Origin**: single value, not inherited from extended project

    String that may substitute the executable directory path in run-path options.

  .. index:: Attributes - Project Level Attributes; Runtime

  * **Runtime**: single value, indexed by a language

    Indexed by language name. Applies to the main project and any extended
    projects. Specifies the runtime directory for the language's compiler. When
    ``--RTS`` is specified on the command line, ``'Runtime`` for that language
    reflects the command-line value instead.

  .. index:: Attributes - Project Level Attributes; Runtime_Dir

  * **Runtime_Dir**: single value, indexed by a language

    Indexed by language name. Path of the runtime directory for the language.

  .. index:: Attributes - Project Level Attributes; Runtime_Library_Dir

  * **Runtime_Library_Dir**: single value, indexed by a language, not inherited from extended project

    Indexed by language name. Path of the directory containing runtime libraries.
    Obsolete.

  .. index:: Attributes - Project Level Attributes; Runtime_Source_Dir

  * **Runtime_Source_Dir**: single value, indexed by a language

    Indexed by language name. Path of the directory containing runtime library
    sources. Obsolete.

  .. index:: Attributes - Project Level Attributes; Runtime_Source_Dirs

  * **Runtime_Source_Dirs**: list value, indexed by a language

    Indexed by language name. Paths of the directories containing runtime library
    sources. Not normally declared directly.

  .. index:: Attributes - Project Level Attributes; Separate_Run_Path_Options

  * **Separate_Run_Path_Options**: single value, not inherited from extended project

    Indicates whether multiple separate run-path options may be passed to the
    linker. Accepted values (case-insensitive): ``"true"`` or ``"false"``
    (default).

  .. index:: Attributes - Project Level Attributes; Target

  * **Target**: single value

    Applies to the main project only. Name of the target platform. When
    ``--target=`` is specified on the command line, ``'Target`` reflects that value
    instead.

  .. index:: Attributes - Project Level Attributes; Toolchain_Version

  * **Toolchain_Version**: single value, indexed by a language

    Indexed by language name. Records the version of the toolchain used for the
    language.

  .. index:: Attributes - Project Level Attributes; Toolchain_Name

  * **Toolchain_Name**: single value, indexed by a language

    Indexed by language name. Applies to the main project and any extended
    projects. Identifies the toolchain used for the language.

  .. index:: Attributes - Project Level Attributes; Toolchain_Description

  * **Toolchain_Description**: single value, indexed by a language

    Obsolescent. No longer used.

* **Source Files**

  .. index:: Attributes - Project Level Attributes; Excluded_Source_Files

  * **Excluded_Source_Files**: list value, not inherited from extended project

    List of simple file names excluded from the project's sources. Use this to
    remove sources that are inherited or found in source directories but should not
    be part of this project.

  .. index:: Attributes - Project Level Attributes; Excluded_Source_List_File

  * **Excluded_Source_List_File**: single value, not inherited from extended project

    Name of a text file listing file simple names to exclude from the project's
    sources.

  .. index:: Attributes - Project Level Attributes; Interfaces

  * **Interfaces**: set value, case-sensitive

    List of file names that form the interface of the project.

  .. index:: Attributes - Project Level Attributes; Locally_Removed_Files

  * **Locally_Removed_Files**: list value, not inherited from extended project

    Obsolescent. Equivalent to ``Excluded_Source_Files``.

  .. index:: Attributes - Project Level Attributes; Source_Files

  * **Source_Files**: list value, not inherited from extended project

    List of source file simple names for the project.

  .. index:: Attributes - Project Level Attributes; Source_List_File

  * **Source_List_File**: single value, not inherited from extended project

    Name of a text file listing source file simple names, one per line.

* **Aggregate Projects**

  .. index:: Attributes - Project Level Attributes; External

  * **External**: single value, indexed by an external reference

    Indexed by external reference name. Sets the value of the external reference to
    use when parsing the aggregated projects.

  .. index:: Attributes - Project Level Attributes; Project_Files

  * **Project_Files**: list value, not inherited from extended project

    List of project files aggregated by this project.

  .. index:: Attributes - Project Level Attributes; Project_Path

  * **Project_Path**: list value, not inherited from extended project

    Additional directories added to the project search path when locating
    aggregated projects.

* **General**

  .. index:: Attributes - Project Level Attributes; Externally_Built

  * **Externally_Built**: single value, not inherited from extended project

    Marks the project as externally built. Accepted values (case-insensitive):
    ``"true"`` or ``"false"`` (default).

  .. index:: Attributes - Project Level Attributes; Languages

  * **Languages**: set value, case-insensitive

    List of languages used in the project's sources.

  .. index:: Attributes - Project Level Attributes; Main

  * **Main**: list value

    List of main sources for the executables.

  .. index:: Attributes - Project Level Attributes; Name

  * **Name**: single value, read-only, not inherited from extended project

    The name of the project.

  .. index:: Attributes - Project Level Attributes; Project_Dir

  * **Project_Dir**: single value, read-only, not inherited from extended project

    Path of the project directory.

  .. index:: Attributes - Project Level Attributes; Roots

  * **Roots**: list value, indexed by a source glob or language

    Indexed by source file name, language name, or ``"*"``. Lists units from the
    main project that must be bound and linked, together with their closures, into
    the indexed executable. Resolution order: exact source file name, then language
    name, then ``"*"``.

  .. index:: Attributes - Project Level Attributes; Warning_Message

  * **Warning_Message**: single value

    Emits a user-defined warning message during project processing.

* **Libraries**

  .. index:: Attributes - Project Level Attributes; Leading_Library_Options

  * **Leading_Library_Options**: list value, configuration concatenable, not inherited from extended project

    Options placed at the beginning of the linker command line when building a
    shared library.

  .. index:: Attributes - Project Level Attributes; Library_Auto_Init

  * **Library_Auto_Init**: single value, not inherited from extended project

    Controls whether a Stand-Alone Library is automatically initialized at load
    time. Accepted values (case-insensitive): ``"true"`` or ``"false"``. Defaults
    to the value of ``Library_Auto_Init_Supported``. Cannot be set to ``"true"``
    when ``Library_Auto_Init_Supported`` is ``"false"``.

  .. index:: Attributes - Project Level Attributes; Library_Dir

  * **Library_Dir**: single value

    Directory where the library is placed. Must be declared in every library
    project.

  .. index:: Attributes - Project Level Attributes; Library_Encapsulated_Options

  * **Library_Encapsulated_Options**: list value, configuration concatenable, not inherited from extended project

    Additional link options required when building an encapsulated Stand-Alone
    Library.

  .. index:: Attributes - Project Level Attributes; Library_Encapsulated_Supported

  * **Library_Encapsulated_Supported**: single value, not inherited from extended project

    Indicates whether encapsulated Stand-Alone Libraries are supported on the
    platform. Accepted values (case-insensitive): ``"true"`` or ``"false"``
    (default).

  .. index:: Attributes - Project Level Attributes; Library_Interface

  * **Library_Interface**: set value, case-insensitive

    List of unit names that form the interface of a Stand-Alone Library.

  .. index:: Attributes - Project Level Attributes; Library_Kind

  * **Library_Kind**: single value, not inherited from extended project

    Accepted values (case-insensitive): ``"static"`` for archives (default),
    ``"static-pic"`` for archives of position-independent code, or ``"dynamic"`` /
    ``"relocatable"`` for shared libraries.

  .. index:: Attributes - Project Level Attributes; Library_Name

  * **Library_Name**: single value

    Name of the library. Must be declared or inherited in every library project.

  .. index:: Attributes - Project Level Attributes; Library_Options

  * **Library_Options**: list value, configuration concatenable, not inherited from extended project

    Additional switches ("last switches") passed when linking a shared library or a
    static standalone library. For a simple static library or when partial linking
    is disabled, values are restricted to paths to object files (absolute or
    relative to the object directory).

  .. index:: Attributes - Project Level Attributes; Library_Reference_Symbol_File

  * **Library_Reference_Symbol_File**: single value, not inherited from extended project

    Name of the reference symbol file.

  .. index:: Attributes - Project Level Attributes; Library_Rpath_Options

  * **Library_Rpath_Options**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Compiler options used to determine the run-path entry
    for a shared library built from sources of the language. The compiler is
    invoked with these options; its output is a shared library path whose directory
    is added to the run-path option when linking.

  .. index:: Attributes - Project Level Attributes; Library_Src_Dir

  * **Library_Src_Dir**: single value, not inherited from extended project

    Directory where copies of the interface sources of a Stand-Alone Library are
    placed.

  .. index:: Attributes - Project Level Attributes; Library_Standalone

  * **Library_Standalone**: single value, not inherited from extended project

    Accepted values (case-insensitive): ``"standard"`` for non-encapsulated
    Stand-Alone Libraries, ``"encapsulated"`` for encapsulated Stand-Alone
    Libraries, or ``"no"`` for a regular (non-SAL) library project.

  .. index:: Attributes - Project Level Attributes; Library_Symbol_File

  * **Library_Symbol_File**: single value, not inherited from extended project

    Name of the library symbol file.

  .. index:: Attributes - Project Level Attributes; Library_Symbol_Policy

  * **Library_Symbol_Policy**: single value, not inherited from extended project

    Accepted values (case-insensitive): ``"restricted"`` or ``"unrestricted"``.

  .. index:: Attributes - Project Level Attributes; Library_Version

  * **Library_Version**: single value, not inherited from extended project

    Internal version string of the library file (for shared library versioning).

    For more details see the :ref:`attribute semantics <Library_Version>`.

* **Configuration - Shared Libraries**

  .. index:: Attributes - Project Level Attributes; Library_Auto_Init_Supported

  * **Library_Auto_Init_Supported**: single value, not inherited from extended project

    Indicates whether automatic initialization of Stand-Alone Libraries is
    supported on the platform. Accepted values (case-insensitive): ``"true"`` or
    ``"false"`` (default).

  .. index:: Attributes - Project Level Attributes; Library_Install_Name_Option

  * **Library_Install_Name_Option**: single value, not inherited from extended project

    Option prefix that, concatenated with the library file path, sets the install
    name of a shared library at link time.

  .. index:: Attributes - Project Level Attributes; Library_Major_Minor_Id_Supported

  * **Library_Major_Minor_Id_Supported**: single value, not inherited from extended project

    Indicates whether major/minor version identifiers in shared library names are
    supported on the platform. Accepted values (case-insensitive): ``"true"`` or
    ``"false"`` (default).

  .. index:: Attributes - Project Level Attributes; Library_Version_Switches

  * **Library_Version_Switches**: list value, configuration concatenable, not inherited from extended project

    Switches used to set the internal (soname) name of a shared library.

  .. index:: Attributes - Project Level Attributes; Shared_Library_Minimum_Switches

  * **Shared_Library_Minimum_Switches**: list value, not inherited from extended project

    Minimum required switches when linking a shared library.

  .. index:: Attributes - Project Level Attributes; Shared_Library_Prefix

  * **Shared_Library_Prefix**: single value, not inherited from extended project

    Prefix for shared library file names. Defaults to ``"lib"``.

  .. index:: Attributes - Project Level Attributes; Shared_Library_Suffix

  * **Shared_Library_Suffix**: single value, not inherited from extended project

    Extension for shared library file names. Defaults to ``".so"``.

  .. index:: Attributes - Project Level Attributes; Symbolic_Link_Supported

  * **Symbolic_Link_Supported**: single value, not inherited from extended project

    Indicates whether symbolic links are supported on the platform. Accepted values
    (case-insensitive): ``"true"`` or ``"false"`` (default).

* **Configuration - Libraries**

  .. index:: Attributes - Project Level Attributes; Library_Builder

  * **Library_Builder**: single value, not inherited from extended project

    Path of the application used to build libraries (typically ``gprlib``).

  .. index:: Attributes - Project Level Attributes; Library_Support

  * **Library_Support**: single value, not inherited from extended project

    Accepted values (case-insensitive): ``"none"`` (default), ``"static_only"``, or
    ``"full"``.

  .. index:: Attributes - Project Level Attributes; Linker_Lib_Dir_Option

  * **Linker_Lib_Dir_Option**: single value, not inherited from extended project

    Option used to add a library directory to the linker search path.

.. _Package_Binder_Attributes:

Package Binder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  .. index:: Attributes - Package Binder Attributes; Default_Switches

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by language name. Switches passed to the binder for the language when
    no ``Switches`` entry matches.

  .. index:: Attributes - Package Binder Attributes; Switches

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by source file name or language name. Switches passed to the binder for
    the matching executable or language.

* **Configuration - Binding**

  .. index:: Attributes - Package Binder Attributes; Driver

  * **Driver**: single value, indexed by a language

    Indexed by language name. Name of the binder executable for the language.

  .. index:: Attributes - Package Binder Attributes; Objects_Path

  * **Objects_Path**: single value, indexed by a language

    Indexed by language name. Name of the environment variable that holds the
    object directory search path for the binder.

  .. index:: Attributes - Package Binder Attributes; Prefix

  * **Prefix**: single value, indexed by a language

    Indexed by language name. Prefix applied to binder exchange file names for the
    language, allowing distinct exchange files when binding multiple languages.

  .. index:: Attributes - Package Binder Attributes; Required_Switches

  * **Required_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Mandatory switches always passed to the binder for
    the language.

.. _Package_Builder_Attributes:

Package Builder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Builder Attributes; Default_Switches

* **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

  Indexed by language name. Builder switches used when building an executable for
  the language and no ``Switches`` entry applies.

.. index:: Attributes - Package Builder Attributes; Switches

* **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

  Indexed by source file name or language name. Builder switches applied when
  building the matching executable.

.. index:: Attributes - Package Builder Attributes; Executable

* **Executable**: single value, indexed by a file name

  Indexed by executable source file name. Simple file name of the resulting
  executable.

.. index:: Attributes - Package Builder Attributes; Executable_Suffix

* **Executable_Suffix**: single value

  Extension appended to executable file names. Defaults to ``.exe`` on Windows,
  empty on other platforms.

.. index:: Attributes - Package Builder Attributes; Global_Compilation_Switches

* **Global_Compilation_Switches**: list value, indexed by a language, "others" index allowed, configuration concatenable

  Indexed by language name. Compilation switches applied globally when building
  an executable.

.. index:: Attributes - Package Builder Attributes; Global_Config_File

* **Global_Config_File**: single value, indexed by a language

  Indexed by language name. File name of a configuration file passed to the
  compiler for every source of the language in the project tree.

.. index:: Attributes - Package Builder Attributes; Global_Configuration_Pragmas

* **Global_Configuration_Pragmas**: single value

  File name of a configuration pragmas file passed to the Ada compiler for every
  Ada source in the project tree.

.. _Package_Clean_Attributes:

Package Clean Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Clean Attributes; Switches

* **Switches**: list value, configuration concatenable

  Applies to the main project only. Switches passed to the cleaning application.

.. index:: Attributes - Package Clean Attributes; Artifacts_In_Exec_Dir

* **Artifacts_In_Exec_Dir**: list value

  List of file name patterns (regular expressions) deleted by ``gprclean`` in the
  main project's executable directory.

.. index:: Attributes - Package Clean Attributes; Artifacts_In_Object_Dir

* **Artifacts_In_Object_Dir**: list value

  List of file name patterns (regular expressions) deleted by ``gprclean`` in the
  project's object directory.

.. index:: Attributes - Package Clean Attributes; Object_Artifact_Extensions

* **Object_Artifact_Extensions**: list value, indexed by a language

  Indexed by language name. Extensions of files derived from source file names
  that ``gprclean`` removes from the object directory.

.. index:: Attributes - Package Clean Attributes; Source_Artifact_Extensions

* **Source_Artifact_Extensions**: list value, indexed by a language

  Indexed by language name. Extensions of files derived from object file names
  that ``gprclean`` removes from the object directory.

.. _Package_Compiler_Attributes:

Package Compiler Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  .. index:: Attributes - Package Compiler Attributes; Default_Switches

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by language name. Switches passed to the compiler for the language when
    no ``Switches`` entry applies.

  .. index:: Attributes - Package Compiler Attributes; Switches

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by source file name or language name. Switches passed to the compiler
    for the matching source or language.

  .. index:: Attributes - Package Compiler Attributes; Local_Config_File

  * **Local_Config_File**: single value, indexed by a language

    Indexed by language name. File name of a configuration file passed to the
    compiler for every source of the language in this project.

  .. index:: Attributes - Package Compiler Attributes; Local_Configuration_Pragmas

  * **Local_Configuration_Pragmas**: single value

    File name of a configuration pragmas file passed to the Ada compiler for every
    Ada source in this project.

* **Configuration - Compiling**

  .. index:: Attributes - Package Compiler Attributes; Driver

  * **Driver**: single value, indexed by a language

    Indexed by language name. Name of the compiler executable for the language.

  .. index:: Attributes - Package Compiler Attributes; Required_Switches

  * **Required_Switches**: list value, indexed by a language, configuration concatenable

    Equivalent to ``Leading_Required_Switches``.

  .. index:: Attributes - Package Compiler Attributes; Dependency_Kind

  * **Dependency_Kind**: single value, indexed by a language

    Indexed by language name. Accepted values (case-insensitive): ``"none"``
    (default), ``"makefile"``, ``"ali_file"``, or ``"ali_closure"``.

  .. index:: Attributes - Package Compiler Attributes; Language_Kind

  * **Language_Kind**: single value, indexed by a language

    Indexed by language name. Accepted values (case-insensitive): ``"file_based"``
    (default) or ``"unit_based"``.

  .. index:: Attributes - Package Compiler Attributes; Leading_Required_Switches

  * **Leading_Required_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Mandatory switches placed at the beginning of the
    compiler command line for the language.

  .. index:: Attributes - Package Compiler Attributes; Multi_Unit_Object_Separator

  * **Multi_Unit_Object_Separator**: single value, indexed by a language

    Indexed by language name. String inserted in the object file name before the
    unit index when compiling a unit from a multi-unit source.

  .. index:: Attributes - Package Compiler Attributes; Multi_Unit_Switches

  * **Multi_Unit_Switches**: list value, indexed by a language

    Indexed by language name. Switches used to identify the unit to compile in a
    multi-unit source. The unit's index within the source is appended to the last
    switch in the list.

  .. index:: Attributes - Package Compiler Attributes; Object_File_Suffix

  * **Object_File_Suffix**: single value, indexed by a language

    Indexed by language name. Extension of object files produced by the compiler.
    Defaults to the platform's standard object file extension.

  .. index:: Attributes - Package Compiler Attributes; Object_File_Switches

  * **Object_File_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to pass the output object file path to
    the compiler. Defaults to ``"-o"``.

  .. index:: Attributes - Package Compiler Attributes; Source_File_Switches

  * **Source_File_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches placed immediately before the source file
    path when invoking the compiler.

  .. index:: Attributes - Package Compiler Attributes; Trailing_Required_Switches

  * **Trailing_Required_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Mandatory switches placed at the end of the compiler
    command line for the language.

* **Configuration - Config Files**

  .. index:: Attributes - Package Compiler Attributes; Config_Body_File_Name

  * **Config_Body_File_Name**: single value, indexed by a language

    Indexed by language name. Template for identifying a body-specific
    configuration entry in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_Body_File_Name_Index

  * **Config_Body_File_Name_Index**: single value, indexed by a language

    Indexed by language name. Template for identifying a body-specific
    configuration entry for a unit within a multi-unit source in a configuration
    file.

  .. index:: Attributes - Package Compiler Attributes; Config_Body_File_Name_Pattern

  * **Config_Body_File_Name_Pattern**: single value, indexed by a language

    Indexed by language name. Template matching all body configuration entries for
    the language in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_File_Switches

  * **Config_File_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to pass a configuration file to the
    compiler.

  .. index:: Attributes - Package Compiler Attributes; Config_File_Unique

  * **Config_File_Unique**: single value, indexed by a language

    Indexed by language name. When ``"true"``, only one configuration file is
    passed to the compiler. Accepted values (case-insensitive): ``"true"`` or
    ``"false"`` (default).

  .. index:: Attributes - Package Compiler Attributes; Config_Spec_File_Name

  * **Config_Spec_File_Name**: single value, indexed by a language

    Indexed by language name. Template for identifying a spec-specific
    configuration entry in a configuration file.

  .. index:: Attributes - Package Compiler Attributes; Config_Spec_File_Name_Index

  * **Config_Spec_File_Name_Index**: single value, indexed by a language

    Indexed by language name. Template for identifying a spec-specific
    configuration entry for a unit within a multi-unit source in a configuration
    file.

  .. index:: Attributes - Package Compiler Attributes; Config_Spec_File_Name_Pattern

  * **Config_Spec_File_Name_Pattern**: single value, indexed by a language

    Indexed by language name. Template matching all spec configuration entries for
    the language in a configuration file.

* **Configuration - Dependencies**

  .. index:: Attributes - Package Compiler Attributes; Dependency_Driver

  * **Dependency_Driver**: list value, indexed by a language

    Indexed by language name. Executable, followed by required switches, used to
    generate dependency files for sources of the language.

  .. index:: Attributes - Package Compiler Attributes; Dependency_Switches

  * **Dependency_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to specify the dependency file to the
    compiler, when the dependency kind is file-based and ``Dependency_Driver`` is
    not set.

* **Configuration - Search Paths**

  .. index:: Attributes - Package Compiler Attributes; Include_Path

  * **Include_Path**: single value, indexed by a language

    Indexed by language name. Name of the environment variable that holds all
    source search directories for the compiler.

  .. index:: Attributes - Package Compiler Attributes; Include_Path_File

  * **Include_Path_File**: single value, indexed by a language

    Indexed by language name. Name of the environment variable whose value is a
    text file listing the source search directories for the compiler.

  .. index:: Attributes - Package Compiler Attributes; Include_Switches

  * **Include_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to pass a source search directory to
    the compiler.

  .. index:: Attributes - Package Compiler Attributes; Object_Path_Switches

  * **Object_Path_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to pass a text file listing object
    directories to the compiler. When not declared, no such file is created.

* **Configuration - Mapping Files**

  .. index:: Attributes - Package Compiler Attributes; Mapping_Body_Suffix

  * **Mapping_Body_Suffix**: single value, indexed by a language

    Indexed by language name. Suffix used in mapping files to mark a source as a
    body.

  .. index:: Attributes - Package Compiler Attributes; Mapping_File_Switches

  * **Mapping_File_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to pass a mapping file to the compiler.

  .. index:: Attributes - Package Compiler Attributes; Mapping_Spec_Suffix

  * **Mapping_Spec_Suffix**: single value, indexed by a language

    Indexed by language name. Suffix used in mapping files to mark a source as a
    spec.

* **Configuration - Response Files**

  .. index:: Attributes - Package Compiler Attributes; Max_Command_Line_Length

  * **Max_Command_Line_Length**: single value

    Maximum number of characters in the compiler command line before a response
    file is used.

  .. index:: Attributes - Package Compiler Attributes; Response_File_Format

  * **Response_File_Format**: single value, indexed by a language

    Indexed by language name. Format of the response file generated when the
    compiler command line exceeds ``Max_Command_Line_Length``. Accepted values
    (case-insensitive): ``"none"``, ``"gnu"``, ``"object_list"``, ``"gcc_gnu"``,
    ``"gcc_option_list"``, or ``"gcc_object_list"``.

  .. index:: Attributes - Package Compiler Attributes; Response_File_Switches

  * **Response_File_Switches**: list value, indexed by a language, configuration concatenable

    Indexed by language name. Switches used to pass a response file to the
    compiler.

.. _Package_Gnatls_Attributes:

Package Gnatls Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Gnatls Attributes; Switches

* **Switches**: list value

  Applies to the main project only. Switches passed to ``gnatls``.

.. _Package_Install_Attributes:

Package Install Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Install Attributes; Prefix

* **Prefix**: single value

  Installation destination directory. A relative path is resolved against the
  global prefix (``--prefix`` option value, or the default installation prefix).

.. index:: Attributes - Package Install Attributes; Active

* **Active**: single value

  Controls whether the project is installed. Setting this to ``"false"``
  (case-insensitive) skips installation; all other values enable it.

.. index:: Attributes - Package Install Attributes; Artifacts

* **Artifacts**: list value, indexed by a file name

  Indexed by installation directory. Lists non-source files to install. Relative
  directory indexes are resolved against ``Prefix``. If the same file name
  appears more than once, the last entry wins. A missing artifact produces a
  warning.

.. index:: Attributes - Package Install Attributes; Exec_Subdir

* **Exec_Subdir**: single value

  Subdirectory under ``Prefix`` where executables are installed. Default:
  ``bin/``.

.. index:: Attributes - Package Install Attributes; Install_Name

* **Install_Name**: single value

  Name used to record the installation. Defaults to the project name without
  extension.

.. index:: Attributes - Package Install Attributes; Install_Project

* **Install_Project**: single value

  Controls whether a relocatable project file is generated and installed.
  Accepted values: ``"true"`` (default) or ``"false"``.

.. index:: Attributes - Package Install Attributes; Lib_Subdir

* **Lib_Subdir**: single value

  Subdirectory under ``Prefix`` where library files are installed. Default:
  ``lib/``.

.. index:: Attributes - Package Install Attributes; Mode

* **Mode**: single value

  Installation mode. Accepted values (case-insensitive): ``"dev"`` (default) —
  full developer installation including sources, ALI files, and libraries;
  ``"usage"`` — end-user installation including only shared libraries and
  executables.

.. index:: Attributes - Package Install Attributes; Project_Subdir

* **Project_Subdir**: single value

  Subdirectory under ``Prefix`` where the generated GPR project file and
  installation manifests are placed. Default: ``share/gpr/``.

.. index:: Attributes - Package Install Attributes; Required_Artifacts

* **Required_Artifacts**: list value, indexed by a file name

  Same as ``Artifacts``, but a missing file raises an error instead of a warning.

.. index:: Attributes - Package Install Attributes; Side_Debug

* **Side_Debug**: single value

  When set to ``"true"``, strips debug symbols from executables and shared
  libraries and writes them to a side file with a ``.debug`` extension. Accepted
  values (case-insensitive): ``"false"`` (default) or ``"true"``.

.. index:: Attributes - Package Install Attributes; Sources_Subdir

* **Sources_Subdir**: single value

  Subdirectory under ``Prefix`` where source files are installed. Default:
  ``include/``.

.. _Package_Linker_Attributes:

Package Linker Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  .. index:: Attributes - Package Linker Attributes; Default_Switches

  * **Default_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    .. warning::

    	Index "others" is not supported yet in gprbuild or GNATcoll.Projects-based tools

    Indexed by language name. Linker switches used when no ``Switches`` entry
    applies for an executable of the language.

  .. index:: Attributes - Package Linker Attributes; Required_Switches

  * **Required_Switches**: list value, configuration concatenable

    Mandatory switches always passed to the linker.

  .. index:: Attributes - Package Linker Attributes; Switches

  * **Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by source file name or language name. Switches passed to the linker for
    the matching executable.

  .. index:: Attributes - Package Linker Attributes; Group_End_Switch

  * **Group_End_Switch**: single value

    Switch that ends a link group.

  .. index:: Attributes - Package Linker Attributes; Group_Start_Switch

  * **Group_Start_Switch**: single value

    Switch that begins a link group (a set of libraries linked with recursive
    symbol resolution).

  .. index:: Attributes - Package Linker Attributes; Leading_Switches

  * **Leading_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by source file name or language name. Switches placed at the beginning
    of the linker command line for the matching executable.

  .. index:: Attributes - Package Linker Attributes; Linker_Options

  * **Linker_Options**: list value, configuration concatenable

    Additional linker switches for imported subsystems. Ignored when set in the
    main project; applied when set in any directly or indirectly imported project.
    Complements ``Linker'Switches`` in the main project. Use this to declare
    per-subsystem external library dependencies instead of repeating them in every
    top-level project.

  .. index:: Attributes - Package Linker Attributes; Trailing_Switches

  * **Trailing_Switches**: list value, indexed by a source glob or language, "others" index allowed, configuration concatenable

    Indexed by source file name or language name. Switches placed at the end of the
    linker command line for the matching executable. These may override
    ``Required_Switches``.

  .. index:: Attributes - Package Linker Attributes; Unconditional_Linking

  * **Unconditional_Linking**: single value, indexed by a language

    Indexed by language name. When set, the link phase always explicitly includes
    all object files produced for the language.

* **Configuration - Linking**

  .. index:: Attributes - Package Linker Attributes; Driver

  * **Driver**: single value

    Name of the linker executable.

* **Configuration - Response Files**

  .. index:: Attributes - Package Linker Attributes; Max_Command_Line_Length

  * **Max_Command_Line_Length**: single value

    Maximum number of characters in the linker command line before a response file
    is used.

  .. index:: Attributes - Package Linker Attributes; Response_File_Format

  * **Response_File_Format**: single value

    Format of the response file generated when the linker command line exceeds
    ``Max_Command_Line_Length``. Accepted values (case-insensitive): ``"none"``,
    ``"gnu"``, ``"object_list"``, ``"gcc_gnu"``, ``"gcc_option_list"``, or
    ``"gcc_object_list"``.

  .. index:: Attributes - Package Linker Attributes; Response_File_Switches

  * **Response_File_Switches**: list value, configuration concatenable

    Switches used to pass a response file to the linker.

.. _Package_Naming_Attributes:

Package Naming Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Attributes - Package Naming Attributes; Body

* **Body**: single value, indexed by a unit

  Indexed by unit name. File name of the unit's body.

.. index:: Attributes - Package Naming Attributes; Body_Suffix

* **Body_Suffix**: single value, indexed by a language

  .. warning::

  	Also has case-insensitive values in gprbuild and GNATcoll.Project-based tools

  Indexed by language name. File name extension for body files of the language.

.. index:: Attributes - Package Naming Attributes; Casing

* **Casing**: single value

  Accepted values (case-insensitive): ``"lowercase"``, ``"uppercase"``, or
  ``"mixedcase"``. Specifies the expected casing of Ada source file names.

.. index:: Attributes - Package Naming Attributes; Dot_Replacement

* **Dot_Replacement**: single value

  String that replaces the dot separator in Ada unit names when forming source
  file names.

.. index:: Attributes - Package Naming Attributes; Implementation

* **Implementation**: single value, indexed by a unit

  Equivalent to ``Body``.

.. index:: Attributes - Package Naming Attributes; Implementation_Exceptions

* **Implementation_Exceptions**: list value, indexed by a language

  Indexed by language name. List of body files for the language that do not
  follow the standard naming scheme and may reside outside the declared source
  directories.

.. index:: Attributes - Package Naming Attributes; Implementation_Suffix

* **Implementation_Suffix**: single value, indexed by a language

  .. warning::

  	Also has case-insensitive values in gprbuild and GNATcoll.Project-based tools

  Equivalent to ``Body_Suffix``.

.. index:: Attributes - Package Naming Attributes; Separate_Suffix

* **Separate_Suffix**: single value

  File name extension for Ada subunit files.

.. index:: Attributes - Package Naming Attributes; Spec

* **Spec**: single value, indexed by a unit

  Indexed by unit name. File name of the unit's spec.

.. index:: Attributes - Package Naming Attributes; Spec_Suffix

* **Spec_Suffix**: single value, indexed by a language

  .. warning::

  	Also has case-insensitive values in gprbuild and GNATcoll.Project-based tools

  Indexed by language name. File name extension for spec files of the language.

.. index:: Attributes - Package Naming Attributes; Specification

* **Specification**: single value, indexed by a unit

  Equivalent to ``Spec``.

.. index:: Attributes - Package Naming Attributes; Specification_Exceptions

* **Specification_Exceptions**: list value, indexed by a language

  Indexed by language name. List of spec files for the language that do not
  follow the standard naming scheme and may reside outside the declared source
  directories.

.. index:: Attributes - Package Naming Attributes; Specification_Suffix

* **Specification_Suffix**: single value, indexed by a language

  .. warning::

  	Also has case-insensitive values in gprbuild and GNATcoll.Project-based tools

  Equivalent to ``Spec_Suffix``.

