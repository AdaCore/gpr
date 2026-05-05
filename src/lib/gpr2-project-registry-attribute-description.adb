--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Project.Registry.Attribute.Description is

   package PRA renames GPR2.Project.Registry.Attribute;

   -------------------------------
   -- Get_Attribute_Description --
   -------------------------------

   function Get_Attribute_Description (Key : Q_Attribute_Id) return String is
      use Pack_Attribute_Description;
   begin
      if Contains (Attribute_Description, Key) then
         return Element (Attribute_Description, Key);
      else
         return "";
      end if;
   end Get_Attribute_Description;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Q_Attribute_Id) return Hash_Type is
     (Strings.Hash (Image (Key)));

   -------------------------------
   -- Set_Attribute_Description --
   -------------------------------

   procedure Set_Attribute_Description
     (Key : Q_Attribute_Id; Description : String)
   is
      use Pack_Attribute_Description;

      C : constant Cursor := Find (Attribute_Description, Key);
   begin
      if C = No_Element then
         Insert (Attribute_Description, Key, Description);
      else
         Replace_Element (Attribute_Description, C, Description);
      end if;
   end Set_Attribute_Description;

begin
   --  Name
   Set_Attribute_Description
     (Key         => PRA.Name,
      Description => "The name of the project.");

   --  Project_Dir
   Set_Attribute_Description
     (Key         => PRA.Project_Dir,
      Description => "Path of the project directory.");

   --  Main
   Set_Attribute_Description
     (Key         => PRA.Main,
      Description => "List of main sources for the executables.");

   --  Languages
   Set_Attribute_Description
     (Key         => PRA.Languages,
      Description => "List of languages used in the project's sources.");

   --  Config_Prj_File
   Set_Attribute_Description
     (Key         => PRA.Config_Prj_File,
      Description => "The main configuration project file.");

   --  Roots
   Set_Attribute_Description
     (Key         => PRA.Roots,
      Description =>
        "Indexed by source file name, language name, or ``""*""``. Lists "
      & "units from the main project that must be bound and linked, together "
      & "with their closures, into the indexed executable. Resolution order: "
      & "exact source file name, then language name, then ``""*""``.");

   --  Externally_Built
   Set_Attribute_Description
     (Key         => PRA.Externally_Built,
      Description =>
        "Marks the project as externally built. Accepted values "
      & "(case-insensitive): ``""true""`` or ``""false""`` (default).");

   --  Warning_Message
   Set_Attribute_Description
     (Key         => PRA.Warning_Message,
      Description => "Emits a user-defined warning message during project "
      & "processing.");

   --  Object_Dir
   Set_Attribute_Description
     (Key         => PRA.Object_Dir,
      Description => "Directory where the compiler places object files.");

   --  Exec_Dir
   Set_Attribute_Description
     (Key         => PRA.Exec_Dir,
      Description => "Directory where executables are placed.");

   --  Create_Missing_Dirs
   Set_Attribute_Description
     (Key         => PRA.Create_Missing_Dirs,
      Description =>
        "Applies to the main project only. When set to ``""true""``, "
      & "automatically creates missing object, library, and executable "
      & "directories. Accepted values (case-insensitive): ``""true""`` or "
      & "``""false""``.");

   --  Source_Dirs
   Set_Attribute_Description
     (Key         => PRA.Source_Dirs,
      Description => "List of source directories for the project.");

   --  Inherit_Source_Path
   Set_Attribute_Description
     (Key         => PRA.Inherit_Source_Path,
      Description =>
        "Indexed by language name. Lists additional languages whose source "
      & "directories are included in the source search path of the indexed "
      & "language.");

   --  Excluded_Source_Dirs
   Set_Attribute_Description
     (Key         => PRA.Excluded_Source_Dirs,
      Description =>
        "Directories listed in ``Source_Dirs`` that are excluded from the "
      & "project's source directories.");

   --  GPR_Registry_Dirs
   Set_Attribute_Description
     (Key         => PRA.GPR_Registry_Dirs,
      Description =>
        "List of directories containing JSON files with external package "
      & "and attribute definitions. All JSON files found in the listed "
      & "directories are loaded at project tree initialization.");

   --  Ignore_Source_Sub_Dirs
   Set_Attribute_Description
     (Key         => PRA.Ignore_Source_Sub_Dirs,
      Description =>
        "List of simple names or patterns for subdirectories to exclude from "
      & "the source directory list, including their own subdirectories.");

   --  Source_Files
   Set_Attribute_Description
     (Key         => PRA.Source_Files,
      Description => "List of source file simple names for the project.");

   --  Locally_Removed_Files
   Set_Attribute_Description
     (Key         => PRA.Locally_Removed_Files,
      Description => "Obsolescent. Equivalent to ``Excluded_Source_Files``.");

   --  Excluded_Source_Files
   Set_Attribute_Description
     (Key         => PRA.Excluded_Source_Files,
      Description =>
        "List of simple file names excluded from the project's sources. Use "
      & "this to remove sources that are inherited or found in source "
      & "directories but should not be part of this project.");

   --  Source_List_File
   Set_Attribute_Description
     (Key         => PRA.Source_List_File,
      Description =>
        "Name of a text file listing source file simple names, one per line.");

   --  Excluded_Source_List_File
   Set_Attribute_Description
     (Key         => PRA.Excluded_Source_List_File,
      Description =>
        "Name of a text file listing file simple names to exclude from the "
      & "project's sources.");

   --  Interfaces
   Set_Attribute_Description
     (Key         => PRA.Interfaces,
      Description => "List of file names that form the interface of the "
      & "project.");

   --  Project_Files
   Set_Attribute_Description
     (Key         => PRA.Project_Files,
      Description => "List of project files aggregated by this project.");

   --  Project_Path
   Set_Attribute_Description
     (Key         => PRA.Project_Path,
      Description =>
        "Additional directories added to the project search path when "
      & "locating aggregated projects.");

   --  External
   Set_Attribute_Description
     (Key         => PRA.External,
      Description =>
        "Indexed by external reference name. Sets the value of the external "
      & "reference to use when parsing the aggregated projects.");

   --  Library_Dir
   Set_Attribute_Description
     (Key         => PRA.Library_Dir,
      Description =>
        "Directory where the library is placed. Must be declared in every "
      & "library project.");

   --  Library_Name
   Set_Attribute_Description
     (Key         => PRA.Library_Name,
      Description =>
        "Name of the library. Must be declared or inherited in every library "
      & "project.");

   --  Library_Kind
   Set_Attribute_Description
     (Key         => PRA.Library_Kind,
      Description =>
        "Accepted values (case-insensitive): ``""static""`` for archives "
      & "(default), ``""static-pic""`` for archives of position-independent "
      & "code, or ``""dynamic""`` / ``""relocatable""`` for shared "
      & "libraries.");

   --  Library_Version
   Set_Attribute_Description
     (Key         => PRA.Library_Version,
      Description =>
        "Internal version string of the library file (for shared library "
      & "versioning).");

   --  Library_Interface
   Set_Attribute_Description
     (Key         => PRA.Library_Interface,
      Description =>
        "List of unit names that form the interface of a Stand-Alone "
      & "Library.");

   --  Library_Standalone
   Set_Attribute_Description
     (Key         => PRA.Library_Standalone,
      Description =>
        "Accepted values (case-insensitive): ``""standard""`` for "
      & "non-encapsulated Stand-Alone Libraries, ``""encapsulated""`` for "
      & "encapsulated Stand-Alone Libraries, or ``""no""`` for a regular "
      & "(non-SAL) library project.");

   --  Library_Encapsulated_Options
   Set_Attribute_Description
     (Key         => PRA.Library_Encapsulated_Options,
      Description =>
        "Additional link options required when building an encapsulated "
      & "Stand-Alone Library.");

   --  Library_Encapsulated_Supported
   Set_Attribute_Description
     (Key         => PRA.Library_Encapsulated_Supported,
      Description =>
        "Indicates whether encapsulated Stand-Alone Libraries are supported "
      & "on the platform. Accepted values (case-insensitive): ``""true""`` "
      & "or ``""false""`` (default).");

   --  Library_Auto_Init
   Set_Attribute_Description
     (Key         => PRA.Library_Auto_Init,
      Description =>
        "Controls whether a Stand-Alone Library is automatically initialized "
      & "at load time. Accepted values (case-insensitive): ``""true""`` or "
      & "``""false""``. Defaults to the value of "
      & "``Library_Auto_Init_Supported``. Cannot be set to ``""true""`` when "
      & "``Library_Auto_Init_Supported`` is ``""false""``.");

   --  Leading_Library_Options
   Set_Attribute_Description
     (Key         => PRA.Leading_Library_Options,
      Description =>
        "Options placed at the beginning of the linker command line when "
      & "building a shared library.");

   --  Library_Options
   Set_Attribute_Description
     (Key         => PRA.Library_Options,
      Description =>
        "Additional switches (""last switches"") passed when linking a shared "
      & "library or a static standalone library. For a simple static library "
      & "or when partial linking is disabled, values are restricted to paths "
      & "to object files (absolute or relative to the object directory).");

   --  Library_Rpath_Options
   Set_Attribute_Description
     (Key         => PRA.Library_Rpath_Options,
      Description =>
        "Indexed by language name. Compiler options used to determine the "
      & "run-path entry for a shared library built from sources of the "
      & "language. The compiler is invoked with these options; its output is "
      & "a shared library path whose directory is added to the run-path "
      & "option when linking.");

   --  Library_Src_Dir
   Set_Attribute_Description
     (Key         => PRA.Library_Src_Dir,
      Description =>
        "Directory where copies of the interface sources of a Stand-Alone "
      & "Library are placed.");

   --  Library_ALI_Dir
   Set_Attribute_Description
     (Key         => PRA.Library_Ali_Dir,
      Description =>
        "Directory where the ALI files for the interface units of a "
      & "Stand-Alone Library are placed. Defaults to the library directory.");

   --  Library_Symbol_File
   Set_Attribute_Description
     (Key         => PRA.Library_Symbol_File,
      Description => "Name of the library symbol file.");

   --  Library_Symbol_Policy
   Set_Attribute_Description
     (Key         => PRA.Library_Symbol_Policy,
      Description =>
        "Accepted values (case-insensitive): ``""restricted""`` or "
      & "``""unrestricted""``.");

   --  Library_Reference_Symbol_File
   Set_Attribute_Description
     (Key         => PRA.Library_Reference_Symbol_File,
      Description => "Name of the reference symbol file.");

   --  Linker_Lib_Dir_Option
   Set_Attribute_Description
     (Key         => PRA.Linker_Lib_Dir_Option,
      Description =>
        "Option used to add a library directory to the linker search path.");

   --  Default_Language
   Set_Attribute_Description
     (Key         => PRA.Default_Language,
      Description =>
        "Default language for the project when ``Languages`` is not declared."
      & " The value is a case-insensitive language name.");

   --  Run_Path_Option
   Set_Attribute_Description
     (Key         => PRA.Run_Path_Option,
      Description =>
        "Switches used to specify the run-path option when linking an "
      & "executable.");

   --  Run_Path_Origin
   Set_Attribute_Description
     (Key         => PRA.Run_Path_Origin,
      Description =>
        "String that may substitute the executable directory path in "
      & "run-path options.");

   --  Separate_Run_Path_Options
   Set_Attribute_Description
     (Key         => PRA.Separate_Run_Path_Options,
      Description =>
        "Indicates whether multiple separate run-path options may be passed "
      & "to the linker. Accepted values (case-insensitive): ``""true""`` or "
      & "``""false""`` (default).");

   --  Toolchain_Version
   Set_Attribute_Description
     (Key         => PRA.Toolchain_Version,
      Description =>
        "Indexed by language name. Records the version of the toolchain used "
      & "for the language.");

   --  Required_Toolchain_Version
   Set_Attribute_Description
     (Key         => PRA.Required_Toolchain_Version,
      Description =>
        "Indexed by language name. Expected value for ``Toolchain_Version`` "
      & "for the language, typically set in an auto-generated configuration "
      & "project. Project processing aborts if ``Required_Toolchain_Version``"
      & " and ``Toolchain_Version`` do not match.");

   --  Toolchain_Description
   Set_Attribute_Description
     (Key         => PRA.Toolchain_Description,
      Description => "Obsolescent. No longer used.");

   --  Object_Generated
   Set_Attribute_Description
     (Key         => PRA.Object_Generated,
      Description =>
        "Indexed by language name. Indicates whether compiling a source of "
      & "the language produces an object file. Accepted values "
      & "(case-insensitive): ``""true""`` (default) or ``""false""``.");

   --  Objects_Linked
   Set_Attribute_Description
     (Key         => PRA.Objects_Linked,
      Description =>
        "Indexed by language name. Indicates whether object files produced "
      & "for the language are linked into executables. Accepted values "
      & "(case-insensitive): ``""true""`` (default) or ``""false""``.");

   --  Target
   Set_Attribute_Description
     (Key         => PRA.Target,
      Description =>
        "Applies to the main project only. Name of the target platform. When "
      & "``--target=`` is specified on the command line, ``'Target`` reflects "
      & "that value instead.");

   --  Runtime
   Set_Attribute_Description
     (Key         => PRA.Runtime,
      Description =>
        "Indexed by language name. Applies to the main project and any "
      & "extended projects. Specifies the runtime directory for the "
      & "language's compiler. When ``--RTS`` is specified on the command "
      & "line, ``'Runtime`` for that language reflects the command-line "
      & "value instead.");

   --  Runtime_Dir
   Set_Attribute_Description
     (Key         => PRA.Runtime_Dir,
      Description =>
        "Indexed by language name. Path of the runtime directory for the "
      & "language.");

   --  Runtime_Library_Dir
   Set_Attribute_Description
     (Key         => PRA.Runtime_Library_Dir,
      Description =>
        "Indexed by language name. Path of the directory containing runtime "
      & "libraries. Obsolete.");

   --  Runtime_Source_Dirs
   Set_Attribute_Description
     (Key         => PRA.Runtime_Source_Dirs,
      Description =>
        "Indexed by language name. Paths of the directories containing "
      & "runtime library sources. Not normally declared directly.");

   --  Runtime_Source_Dir
   Set_Attribute_Description
     (Key         => PRA.Runtime_Source_Dir,
      Description =>
        "Indexed by language name. Path of the directory containing runtime "
      & "library sources. Obsolete.");

   --  Toolchain_Name
   Set_Attribute_Description
     (Key         => PRA.Toolchain_Name,
      Description =>
        "Indexed by language name. Applies to the main project and any "
      & "extended projects. Identifies the toolchain used for the language.");

   --  Library_Builder
   Set_Attribute_Description
     (Key         => PRA.Library_Builder,
      Description =>
        "Path of the application used to build libraries (typically "
      & "``gprlib``).");

   --  Library_Support
   Set_Attribute_Description
     (Key         => PRA.Library_Support,
      Description =>
        "Accepted values (case-insensitive): ``""none""`` (default), "
      & "``""static_only""``, or ``""full""``.");

   --  Disable_Linking
   Set_Attribute_Description
     (Key         => PRA.Disable_Linking,
      Description =>
        "Indicates whether linking is disabled on the platform. Accepted "
      & "values (case-insensitive): ``""true""`` or ``""false""`` (default).");

   --  Archive_Builder
   Set_Attribute_Description
     (Key         => PRA.Archive_Builder,
      Description =>
        "Name of the application used to create a static library (archive), "
      & "followed by its required options. If the value is empty, the object "
      & "files listed in the archive recipe are copied to the library "
      & "directory instead.");

   --  Archive_Builder_Append_Option
   Set_Attribute_Description
     (Key         => PRA.Archive_Builder_Append_Option,
      Description =>
        "Options passed to the archive builder when appending files to an "
      & "existing archive.");

   --  Archive_Indexer
   Set_Attribute_Description
     (Key         => PRA.Archive_Indexer,
      Description =>
        "Name of the archive indexer executable, followed by its required "
      & "options.");

   --  Archive_Prefix
   Set_Attribute_Description
     (Key         => PRA.Archive_Prefix,
      Description =>
        "Prefix for archive file names. Defaults to ``""lib""``.");

   --  Archive_Suffix
   Set_Attribute_Description
     (Key         => PRA.Archive_Suffix,
      Description =>
        "Extension for archive file names. Defaults to ``"".a""``.");

   --  Library_Partial_Linker
   Set_Attribute_Description
     (Key         => PRA.Library_Partial_Linker,
      Description =>
        "Name of the partial linker executable, followed by its required "
      & "options. An empty list disables partial linking.");

   --  Shared_Library_Prefix
   Set_Attribute_Description
     (Key         => PRA.Shared_Library_Prefix,
      Description =>
        "Prefix for shared library file names. Defaults to ``""lib""``.");

   --  Shared_Library_Suffix
   Set_Attribute_Description
     (Key         => PRA.Shared_Library_Suffix,
      Description =>
        "Extension for shared library file names. Defaults to ``"".so""``.");

   --  Symbolic_Link_Supported
   Set_Attribute_Description
     (Key         => PRA.Symbolic_Link_Supported,
      Description =>
        "Indicates whether symbolic links are supported on the platform. "
      & "Accepted values (case-insensitive): ``""true""`` or ``""false""`` "
      & "(default).");

   --  Library_Major_Minor_Id_Supported
   Set_Attribute_Description
     (Key         => PRA.Library_Major_Minor_Id_Supported,
      Description =>
        "Indicates whether major/minor version identifiers in shared library "
      & "names are supported on the platform. Accepted values "
      & "(case-insensitive): ``""true""`` or ``""false""`` (default).");

   --  Library_Auto_Init_Supported
   Set_Attribute_Description
     (Key         => PRA.Library_Auto_Init_Supported,
      Description =>
        "Indicates whether automatic initialization of Stand-Alone Libraries "
      & "is supported on the platform. Accepted values (case-insensitive): "
      & "``""true""`` or ``""false""`` (default).");

   --  Shared_Library_Minimum_Switches
   Set_Attribute_Description
     (Key         => PRA.Shared_Library_Minimum_Switches,
      Description =>
        "Minimum required switches when linking a shared library.");

   --  Library_Version_Switches
   Set_Attribute_Description
     (Key         => PRA.Library_Version_Switches,
      Description =>
        "Switches used to set the internal (soname) name of a shared "
      & "library.");

   --  Library_Install_Name_Option
   Set_Attribute_Description
     (Key         => PRA.Library_Install_Name_Option,
      Description =>
        "Option prefix that, concatenated with the library file path, sets "
      & "the install name of a shared library at link time.");

   --  Binder.Default_Switches
   Set_Attribute_Description
     (Key         => PRA.Binder.Default_Switches,
      Description =>
        "Indexed by language name. Switches passed to the binder for the "
      & "language when no ``Switches`` entry matches.");

   --  Binder.Switches
   Set_Attribute_Description
     (Key         => PRA.Binder.Switches,
      Description =>
        "Indexed by source file name or language name. Switches passed to "
      & "the binder for the matching executable or language.");

   --  Binder.Driver
   Set_Attribute_Description
     (Key         => PRA.Binder.Driver,
      Description =>
        "Indexed by language name. Name of the binder executable for the "
      & "language.");

   --  Binder.Required_Switches
   Set_Attribute_Description
     (Key         => PRA.Binder.Required_Switches,
      Description =>
        "Indexed by language name. Mandatory switches always passed to the "
      & "binder for the language.");

   --  Binder.Prefix
   Set_Attribute_Description
     (Key         => PRA.Binder.Prefix,
      Description =>
        "Indexed by language name. Prefix applied to binder exchange file "
      & "names for the language, allowing distinct exchange files when "
      & "binding multiple languages.");

   --  Binder.Objects_Path
   Set_Attribute_Description
     (Key         => PRA.Binder.Objects_Path,
      Description =>
        "Indexed by language name. Name of the environment variable that "
      & "holds the object directory search path for the binder.");

   --  Binder.Object_Path_File
   Set_Attribute_Description
     (Key         => PRA.Binder.Objects_Path_File,
      Description =>
        "Indexed by language name. Name of the environment variable whose "
      & "value is a text file listing the object directories.");

   --  Builder.Default_Switches
   Set_Attribute_Description
     (Key         => PRA.Builder.Default_Switches,
      Description =>
        "Indexed by language name. Builder switches used when building an "
      & "executable for the language and no ``Switches`` entry applies.");

   --  Builder.Switches
   Set_Attribute_Description
     (Key         => PRA.Builder.Switches,
      Description =>
        "Indexed by source file name or language name. Builder switches "
      & "applied when building the matching executable.");

   --  Builder.Global_Compilation_Switches
   Set_Attribute_Description
     (Key         => PRA.Builder.Global_Compilation_Switches,
      Description =>
        "Indexed by language name. Compilation switches applied globally "
      & "when building an executable.");

   --  Builder.Executable
   Set_Attribute_Description
     (Key         => PRA.Builder.Executable,
      Description =>
        "Indexed by executable source file name. Simple file name of the "
      & "resulting executable.");

   --  Builder.Executable_Suffix
   Set_Attribute_Description
     (Key         => PRA.Builder.Executable_Suffix,
      Description =>
        "Extension appended to executable file names. Defaults to ``.exe`` "
      & "on Windows, empty on other platforms.");

   --  Builder.Global_Configuration_Pragmas
   Set_Attribute_Description
     (Key         => PRA.Builder.Global_Configuration_Pragmas,
      Description =>
        "File name of a configuration pragmas file passed to the Ada compiler"
      & " for every Ada source in the project tree.");

   --  Builder.Global_Config_File
   Set_Attribute_Description
     (Key         => PRA.Builder.Global_Config_File,
      Description =>
        "Indexed by language name. File name of a configuration file passed "
      & "to the compiler for every source of the language in the project "
      & "tree.");

   --  Clean.Switches
   Set_Attribute_Description
     (Key         => PRA.Clean.Switches,
      Description =>
        "Applies to the main project only. Switches passed to the cleaning "
      & "application.");

   --  Clean.Source_Artifact_Extensions
   Set_Attribute_Description
     (Key         => PRA.Clean.Source_Artifact_Extensions,
      Description =>
        "Indexed by language name. Extensions of files derived from object "
      & "file names that ``gprclean`` removes from the object directory.");

   --  Clean.Object_Artifact_Extensions
   Set_Attribute_Description
     (Key         => PRA.Clean.Object_Artifact_Extensions,
      Description =>
        "Indexed by language name. Extensions of files derived from source "
      & "file names that ``gprclean`` removes from the object directory.");

   --  Clean.Artifacts_In_Object_Dir
   Set_Attribute_Description
     (Key         => PRA.Clean.Artifacts_In_Object_Dir,
      Description =>
        "List of file name patterns (regular expressions) deleted by "
      & "``gprclean`` in the project's object directory.");

   --  Clean.Artifacts_In_Exec_Dir
   Set_Attribute_Description
     (Key         => PRA.Clean.Artifacts_In_Exec_Dir,
      Description =>
        "List of file name patterns (regular expressions) deleted by "
      & "``gprclean`` in the main project's executable directory.");

   --  Compiler.Default_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Default_Switches,
      Description =>
        "Indexed by language name. Switches passed to the compiler for the "
      & "language when no ``Switches`` entry applies.");

   --  Compiler.Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Switches,
      Description =>
        "Indexed by source file name or language name. Switches passed to "
      & "the compiler for the matching source or language.");

   --  Compiler.Local_Configuration_Pragmas
   Set_Attribute_Description
     (Key         => PRA.Compiler.Local_Configuration_Pragmas,
      Description =>
        "File name of a configuration pragmas file passed to the Ada compiler"
      & " for every Ada source in this project.");

   --  Compiler.Local_Config_File
   Set_Attribute_Description
     (Key         => PRA.Compiler.Local_Config_File,
      Description =>
        "Indexed by language name. File name of a configuration file passed "
      & "to the compiler for every source of the language in this project.");

   --  Compiler.Driver
   Set_Attribute_Description
     (Key         => PRA.Compiler.Driver,
      Description =>
        "Indexed by language name. Name of the compiler executable for the "
      & "language.");

   --  Compiler.Language_Kind
   Set_Attribute_Description
     (Key         => PRA.Compiler.Language_Kind,
      Description =>
        "Indexed by language name. Accepted values (case-insensitive): "
      & "``""file_based""`` (default) or ``""unit_based""``.");

   --  Compiler.Dependency_Kind
   Set_Attribute_Description
     (Key         => PRA.Compiler.Dependency_Kind,
      Description =>
        "Indexed by language name. Accepted values (case-insensitive): "
      & "``""none""`` (default), ``""makefile""``, ``""ali_file""``, or "
      & "``""ali_closure""``.");

   --  Compiler.Required_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Required_Switches,
      Description => "Equivalent to ``Leading_Required_Switches``.");

   --  Compiler.Leading_Required_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Leading_Required_Switches,
      Description =>
        "Indexed by language name. Mandatory switches placed at the beginning"
      & " of the compiler command line for the language.");

   --  Compiler.Trailing_Required_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Trailing_Required_Switches,
      Description =>
        "Indexed by language name. Mandatory switches placed at the end of "
      & "the compiler command line for the language.");

   --  Compiler.PIC_Option
   Set_Attribute_Description
     (Key         => PRA.Compiler.Pic_Option,
      Description =>
        "Indexed by language name. Switches added when compiling sources of "
      & "the language for a shared library project (position-independent "
      & "code).");

   --  Compiler.Source_File_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Source_File_Switches,
      Description =>
        "Indexed by language name. Switches placed immediately before the "
      & "source file path when invoking the compiler.");

   --  Compiler.Object_File_Suffix
   Set_Attribute_Description
     (Key         => PRA.Compiler.Object_File_Suffix,
      Description =>
        "Indexed by language name. Extension of object files produced by the "
      & "compiler. Defaults to the platform's standard object file "
      & "extension.");

   --  Compiler.Object_File_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Object_File_Switches,
      Description =>
        "Indexed by language name. Switches used to pass the output object "
      & "file path to the compiler. Defaults to ``""-o""``.");

   --  Compiler.Multi_Unit_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Multi_Unit_Switches,
      Description =>
        "Indexed by language name. Switches used to identify the unit to "
      & "compile in a multi-unit source. The unit's index within the source "
      & "is appended to the last switch in the list.");

   --  Compiler.Multi_Unit_Object_Separator
   Set_Attribute_Description
     (Key         => PRA.Compiler.Multi_Unit_Object_Separator,
      Description =>
        "Indexed by language name. String inserted in the object file name "
      & "before the unit index when compiling a unit from a multi-unit "
      & "source.");

   --  Compiler.Mapping_File_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Mapping_File_Switches,
      Description =>
        "Indexed by language name. Switches used to pass a mapping file to "
      & "the compiler.");

   --  Compiler.Mapping_Spec_Suffix
   Set_Attribute_Description
     (Key         => PRA.Compiler.Mapping_Spec_Suffix,
      Description =>
        "Indexed by language name. Suffix used in mapping files to mark a "
      & "source as a spec.");

   --  Compiler.Mapping_Body_Suffix
   Set_Attribute_Description
     (Key         => PRA.Compiler.Mapping_Body_Suffix,
      Description =>
        "Indexed by language name. Suffix used in mapping files to mark a "
      & "source as a body.");

   --  Compiler.Config_File_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_File_Switches,
      Description =>
        "Indexed by language name. Switches used to pass a configuration "
      & "file to the compiler.");

   --  Compiler.Config_Body_File_Name
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_Body_File_Name,
      Description =>
        "Indexed by language name. Template for identifying a body-specific "
      & "configuration entry in a configuration file.");

   --  Compiler.Config_Body_File_Name_Index
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_Body_File_Name_Index,
      Description =>
        "Indexed by language name. Template for identifying a body-specific "
      & "configuration entry for a unit within a multi-unit source in a "
      & "configuration file.");

   --  Compiler.Config_Body_File_Name_Pattern
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_Body_File_Name_Pattern,
      Description =>
        "Indexed by language name. Template matching all body configuration "
      & "entries for the language in a configuration file.");

   --  Compiler.Config_Spec_File_Name
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_Spec_File_Name,
      Description =>
        "Indexed by language name. Template for identifying a spec-specific "
      & "configuration entry in a configuration file.");

   --  Compiler.Config_Spec_File_Name_Index
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_Spec_File_Name_Index,
      Description =>
        "Indexed by language name. Template for identifying a spec-specific "
      & "configuration entry for a unit within a multi-unit source in a "
      & "configuration file.");

   --  Compiler.Config_Spec_File_Name_Pattern
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_Spec_File_Name_Pattern,
      Description =>
        "Indexed by language name. Template matching all spec configuration "
      & "entries for the language in a configuration file.");

   --  Compiler.Config_File_Unique
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_File_Unique,
      Description =>
        "Indexed by language name. When ``""true""``, only one configuration "
      & "file is passed to the compiler. Accepted values (case-insensitive): "
      & "``""true""`` or ``""false""`` (default).");

   --  Compiler.Config_File_Dependency_Support
   Set_Attribute_Description
     (Key         => PRA.Compiler.Config_File_Dependency_Support,
      Description =>
        "Indexed by language name. Indicates whether the compiler tracks "
      & "dependencies on configuration pragma files (requires GNAT > 7.2.2)."
      & " Accepted values (case-insensitive): ``""true""`` (default) or "
      & "``""false""``.");

   --  Compiler.Dependency_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Dependency_Switches,
      Description =>
        "Indexed by language name. Switches used to specify the dependency "
      & "file to the compiler, when the dependency kind is file-based and "
      & "``Dependency_Driver`` is not set.");

   --  Compiler.Dependency_Driver
   Set_Attribute_Description
     (Key         => PRA.Compiler.Dependency_Driver,
      Description =>
        "Indexed by language name. Executable, followed by required switches,"
      & " used to generate dependency files for sources of the language.");

   --  Compiler.Include_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Include_Switches,
      Description =>
        "Indexed by language name. Switches used to pass a source search "
      & "directory to the compiler.");

   --  Compiler.Include_Path
   Set_Attribute_Description
     (Key         => PRA.Compiler.Include_Path,
      Description =>
        "Indexed by language name. Name of the environment variable that "
      & "holds all source search directories for the compiler.");

   --  Compiler.Include_Path_File
   Set_Attribute_Description
     (Key         => PRA.Compiler.Include_Path_File,
      Description =>
        "Indexed by language name. Name of the environment variable whose "
      & "value is a text file listing the source search directories for the "
      & "compiler.");

   --  Compiler.Object_Path_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Object_Path_Switches,
      Description =>
        "Indexed by language name. Switches used to pass a text file listing "
      & "object directories to the compiler. When not declared, no such file "
      & "is created.");

   --  Compiler.Max_Command_Line_Length
   Set_Attribute_Description
     (Key         => PRA.Compiler.Max_Command_Line_Length,
      Description =>
        "Maximum number of characters in the compiler command line before a "
      & "response file is used.");

   --  Compiler.Response_File_Format
   Set_Attribute_Description
     (Key         => PRA.Compiler.Response_File_Format,
      Description =>
        "Indexed by language name. Format of the response file generated when"
      & " the compiler command line exceeds ``Max_Command_Line_Length``."
      & " Accepted values (case-insensitive): ``""none""``, ``""gnu""``, "
      & "``""object_list""``, ``""gcc_gnu""``, ``""gcc_option_list""``, or "
      & "``""gcc_object_list""``.");

   --  Compiler.Response_File_Switches
   Set_Attribute_Description
     (Key         => PRA.Compiler.Response_File_Switches,
      Description =>
        "Indexed by language name. Switches used to pass a response file to "
      & "the compiler.");

   --  Gnatls.Switches
   Set_Attribute_Description
     (Key         => PRA.Gnatls.Switches,
      Description =>
        "Applies to the main project only. Switches passed to ``gnatls``.");

   --  Install.Artifacts
   Set_Attribute_Description
     (Key         => PRA.Install.Artifacts,
      Description =>
        "Indexed by installation directory. Lists non-source files to "
      & "install. Relative directory indexes are resolved against ``Prefix``."
      & " If the same file name appears more than once, the last entry wins. "
      & "A missing artifact produces a warning.");

   --  Install.Required_Artifacts
   Set_Attribute_Description
     (Key         => PRA.Install.Required_Artifacts,
      Description =>
        "Same as ``Artifacts``, but a missing file raises an error instead "
      & "of a warning.");

   --  Install.Prefix
   Set_Attribute_Description
     (Key         => PRA.Install.Prefix,
      Description =>
        "Installation destination directory. A relative path is resolved "
      & "against the global prefix (``--prefix`` option value, or the "
      & "default installation prefix).");

   --  Install.Sources_Subdir
   Set_Attribute_Description
     (Key         => PRA.Install.Sources_Subdir,
      Description =>
        "Subdirectory under ``Prefix`` where source files are installed. "
      & "Default: ``include/``.");

   --  Install.Exec_Subdir
   Set_Attribute_Description
     (Key         => PRA.Install.Exec_Subdir,
      Description =>
        "Subdirectory under ``Prefix`` where executables are installed. "
      & "Default: ``bin/``.");

   --  Install.ALI_Subdir
   Set_Attribute_Description
     (Key         => PRA.Install.ALI_Subdir,
      Description =>
        "Subdirectory under ``Prefix`` where Ada ALI files are installed. "
      & "Defaults to ``Lib_Subdir`` when not set explicitly.");

   --  Install.Lib_Subdir
   Set_Attribute_Description
     (Key         => PRA.Install.Lib_Subdir,
      Description =>
        "Subdirectory under ``Prefix`` where library files are installed. "
      & "Default: ``lib/``.");

   --  Install.Link_Lib_Subdir
   Set_Attribute_Description
     (Key         => PRA.Install.Link_Lib_Subdir,
      Description =>
        "Subdirectory under ``Prefix`` where shared library compatibility "
      & "symlinks are created (Unix only). Default: ``lib/``.");

   --  Install.Project_Subdir
   Set_Attribute_Description
     (Key         => PRA.Install.Project_Subdir,
      Description =>
        "Subdirectory under ``Prefix`` where the generated GPR project file "
      & "and installation manifests are placed. Default: ``share/gpr/``.");

   --  Install.Active
   Set_Attribute_Description
     (Key         => PRA.Install.Active,
      Description =>
        "Controls whether the project is installed. Setting this to "
      & "``""false""`` (case-insensitive) skips installation; all other "
      & "values enable it.");

   --  Install.Mode
   Set_Attribute_Description
     (Key         => PRA.Install.Mode,
      Description =>
        "Installation mode. Accepted values (case-insensitive): "
      & "``""dev""`` (default) — full developer installation including "
      & "sources, ALI files, and libraries; ``""usage""`` — end-user "
      & "installation including only shared libraries and executables.");

   --  Install.Install_Name
   Set_Attribute_Description
     (Key         => PRA.Install.Install_Name,
      Description =>
        "Name used to record the installation. Defaults to the project name "
      & "without extension.");

   --  Install.Side_Debug
   Set_Attribute_Description
     (Key         => PRA.Install.Side_Debug,
      Description =>
        "When set to ``""true""``, strips debug symbols from executables and "
      & "shared libraries and writes them to a side file with a ``.debug`` "
      & "extension. Accepted values (case-insensitive): ``""false""`` "
      & "(default) or ``""true""``.");

   --  Install.Install_Project
   Set_Attribute_Description
     (Key         => PRA.Install.Install_Project,
      Description =>
        "Controls whether a relocatable project file is generated and "
      & "installed. Accepted values: ``""true""`` (default) or "
      & "``""false""``.");

   --  Linker.Required_Switches
   Set_Attribute_Description
     (Key         => PRA.Linker.Required_Switches,
      Description => "Mandatory switches always passed to the linker.");

   --  Linker.Default_Switches
   Set_Attribute_Description
     (Key         => PRA.Linker.Default_Switches,
      Description =>
        "Indexed by language name. Linker switches used when no ``Switches`` "
      & "entry applies for an executable of the language.");

   --  Linker.Leading_Switches
   Set_Attribute_Description
     (Key         => PRA.Linker.Leading_Switches,
      Description =>
        "Indexed by source file name or language name. Switches placed at "
      & "the beginning of the linker command line for the matching "
      & "executable.");

   --  Linker.Switches
   Set_Attribute_Description
     (Key         => PRA.Linker.Switches,
      Description =>
        "Indexed by source file name or language name. Switches passed to "
      & "the linker for the matching executable.");

   --  Linker.Trailing_Switches
   Set_Attribute_Description
     (Key         => PRA.Linker.Trailing_Switches,
      Description =>
        "Indexed by source file name or language name. Switches placed at "
      & "the end of the linker command line for the matching executable. "
      & "These may override ``Required_Switches``.");

   --  Linker.Linker_Options
   Set_Attribute_Description
     (Key         => PRA.Linker.Linker_Options,
      Description =>
        "Additional linker switches for imported subsystems. Ignored when "
      & "set in the main project; applied when set in any directly or "
      & "indirectly imported project. Complements ``Linker'Switches`` in the "
      & "main project. Use this to declare per-subsystem external library "
      & "dependencies instead of repeating them in every top-level project.");

   --  Linker.Map_File_Option
   Set_Attribute_Description
     (Key         => PRA.Linker.Map_File_Option,
      Description =>
        "Switch used to pass the map file name to the linker.");

   --  Linker.Driver
   Set_Attribute_Description
     (Key         => PRA.Linker.Driver,
      Description => "Name of the linker executable.");

   --  Linker.Max_Command_Line_Length
   Set_Attribute_Description
     (Key         => PRA.Linker.Max_Command_Line_Length,
      Description =>
        "Maximum number of characters in the linker command line before a "
      & "response file is used.");

   --  Linker.Response_File_Format
   Set_Attribute_Description
     (Key         => PRA.Linker.Response_File_Format,
      Description =>
        "Format of the response file generated when the linker command line "
      & "exceeds ``Max_Command_Line_Length``. Accepted values "
      & "(case-insensitive): ``""none""``, ``""gnu""``, ``""object_list""``, "
      & "``""gcc_gnu""``, ``""gcc_option_list""``, or "
      & "``""gcc_object_list""``.");

   --  Linker.Response_File_Switches
   Set_Attribute_Description
     (Key         => PRA.Linker.Response_File_Switches,
      Description => "Switches used to pass a response file to the linker.");

   --  Linker.Group_Start_Switch
   Set_Attribute_Description
     (Key         => PRA.Linker.Group_Start_Switch,
      Description =>
        "Switch that begins a link group (a set of libraries linked with "
      & "recursive symbol resolution).");

   --  Linker.Group_End_Switch
   Set_Attribute_Description
     (Key         => PRA.Linker.Group_End_Switch,
      Description => "Switch that ends a link group.");

   --  Linker.Unconditional_Linking
   Set_Attribute_Description
     (Key         => PRA.Linker.Unconditional_Linking,
      Description =>
        "Indexed by language name. When set, the link phase always explicitly"
      & " includes all object files produced for the language.");

   --  Naming.Specification_Suffix
   Set_Attribute_Description
     (Key         => PRA.Naming.Specification_Suffix,
      Description => "Equivalent to ``Spec_Suffix``.");

   --  Naming.Spec_Suffix
   Set_Attribute_Description
     (Key         => PRA.Naming.Spec_Suffix,
      Description =>
        "Indexed by language name. File name extension for spec files of the "
      & "language.");

   --  Naming.Implementation_Suffix
   Set_Attribute_Description
     (Key         => PRA.Naming.Implementation_Suffix,
      Description => "Equivalent to ``Body_Suffix``.");

   --  Naming.Body_Suffix
   Set_Attribute_Description
     (Key         => PRA.Naming.Body_Suffix,
      Description =>
        "Indexed by language name. File name extension for body files of the "
      & "language.");

   --  Naming.Separate_Suffix
   Set_Attribute_Description
     (Key         => PRA.Naming.Separate_Suffix,
      Description => "File name extension for Ada subunit files.");

   --  Naming.Casing
   Set_Attribute_Description
     (Key         => PRA.Naming.Casing,
      Description =>
        "Accepted values (case-insensitive): ``""lowercase""``, "
      & "``""uppercase""``, or ``""mixedcase""``. Specifies the expected "
      & "casing of Ada source file names.");

   --  Naming.Dot_Replacement
   Set_Attribute_Description
     (Key         => PRA.Naming.Dot_Replacement,
      Description =>
        "String that replaces the dot separator in Ada unit names when "
      & "forming source file names.");

   --  Naming.Specification
   Set_Attribute_Description
     (Key         => PRA.Naming.Specification,
      Description => "Equivalent to ``Spec``.");

   --  Naming.Spec
   Set_Attribute_Description
     (Key         => PRA.Naming.Spec,
      Description =>
        "Indexed by unit name. File name of the unit's spec.");

   --  Naming.Implementation
   Set_Attribute_Description
     (Key         => PRA.Naming.Implementation,
      Description => "Equivalent to ``Body``.");

   --  Naming.Specification_Exceptions
   Set_Attribute_Description
     (Key         => PRA.Naming.Specification_Exceptions,
      Description =>
        "Indexed by language name. List of spec files for the language that "
      & "do not follow the standard naming scheme and may reside outside the "
      & "declared source directories.");

   --  Naming.Implementation_Exceptions
   Set_Attribute_Description
     (Key         => PRA.Naming.Implementation_Exceptions,
      Description =>
        "Indexed by language name. List of body files for the language that "
      & "do not follow the standard naming scheme and may reside outside the "
      & "declared source directories.");

   --  Naming.Body
   Set_Attribute_Description
     (Key         => PRA.Naming.Body_N,
      Description => "Indexed by unit name. File name of the unit's body.");

end GPR2.Project.Registry.Attribute.Description;
