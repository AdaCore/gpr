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
     (Key          => PRA.Name,
      Description  =>
        "The name of the project."
     );

   --  Project_Dir
   Set_Attribute_Description
     (Key          => PRA.Project_Dir,
      Description  =>
        "The path name of the project directory."
     );

   --  Main
   Set_Attribute_Description
     (Key          => PRA.Main,
      Description  =>
        "The list of main sources for the executables."
     );

   --  Languages
   Set_Attribute_Description
     (Key          => PRA.Languages,
      Description  =>
        "The list of languages of the sources of the project."
     );

   --  Config_Prj_File
   Set_Attribute_Description
     (Key          => PRA.Config_Prj_File,
      Description  =>
        "The main configuration project file."
     );

   --  Roots
   Set_Attribute_Description
     (Key          => PRA.Roots,
      Description  =>
        "The index is the file name of an executable source. Indicates the "
      & "list of units from the main project that need to be bound and "
      & "linked with their closures with the executable. The index is either"
      & " a file name, a language name or '*'. The roots for an executable "
      & "source are those in Roots with an index that is the executable "
      & "source file name, if declared. Otherwise, they are those in Roots "
      & "with an index that is the language name of the executable source, "
      & "if present. Otherwise, they are those in Roots ('*'), if declared. "
      & "If none of these three possibilities are declared, then there are "
      & "no roots for the executable source.");

   --  Externally_Built
   Set_Attribute_Description
     (Key          => PRA.Externally_Built,
      Description  =>
        "Indicates if the project is externally built. Only case-insensitive"
      & " values allowed are 'true' and 'false', the default.");

   --  Warning_Message
   Set_Attribute_Description
     (Key          => PRA.Warning_Message,
      Description  =>
        "Causes gprbuild to emit a user-defined warning message.");

   --  Object_Dir
   Set_Attribute_Description
     (Key          => PRA.Object_Dir,
      Description  =>
        "Indicates the object directory for the project.");

   --  Exec_Dir
   Set_Attribute_Description
     (Key          => PRA.Exec_Dir,
      Description  =>
        "Indicates the exec directory for the project, that is the directory"
      & " where the executables are.");

   --  Create_Missing_Dirs
   Set_Attribute_Description
     (Key          => PRA.Create_Missing_Dirs,
      Description  =>
        "Indicates if the missing object, library and executable directories"
      & " should be created automatically by the project-aware tool. Taken "
      & "into account only in the main project. Only authorized "
      & "case-insensitive values are 'true' and 'false'.");

   --  Source_Dirs
   Set_Attribute_Description
     (Key          => PRA.Source_Dirs,
      Description  =>
        "The list of source directories of the project.");

   --  Inherit_Source_Path
   Set_Attribute_Description
     (Key          => PRA.Inherit_Source_Path,
      Description  =>
        "Index is a language name. Value is a list of language names. "
      & "Indicates that in the source search path of the index language the "
      & "source directories of the languages in the list should be included.");

   --  Exclude_Source_Dirs
   Set_Attribute_Description
     (Key          => PRA.Excluded_Source_Dirs,
      Description  =>
        "The list of directories that are included in Source_Dirs but are "
      & "not source directories of the project.");

   --  Ignore_Source_Sub_Dirs
   Set_Attribute_Description
     (Key          => PRA.Ignore_Source_Sub_Dirs,
      Description  =>
        "Value is a list of simple names or patterns for subdirectories that"
      & " are removed from the list of source directories, including their "
      & "subdirectories.");

   --  Source_Files
   Set_Attribute_Description
     (Key          => PRA.Source_Files,
      Description  =>
        "Value is a list of source file simple names.");

   --  Locally_Removed_Files
   Set_Attribute_Description
     (Key          => PRA.Locally_Removed_Files,
      Description  =>
        "Obsolescent. Equivalent to Excluded_Source_Files.");

   --  Excluded_Source_Files
   Set_Attribute_Description
     (Key          => PRA.Excluded_Source_Files,
      Description  =>
        "Value is a list of simple file names that are not sources of the "
      & "project. Allows to remove sources that are inherited or found in "
      & "the source directories and that match the naming scheme.");

   --  Source_List_File
   Set_Attribute_Description
     (Key          => PRA.Source_List_File,
      Description  =>
        "Value is a text file name that contains a list of source file "
      & "simple names, one on each line.");

   --  Excluded_Source_List_File
   Set_Attribute_Description
     (Key          => PRA.Excluded_Source_List_File,
      Description  =>
        "Value is a text file name that contains a list of file simple names"
      & " that are not sources of the project.");

   --  Interfaces
   Set_Attribute_Description
     (Key          => PRA.Interfaces,
      Description  =>
        "Value is a list of file names that constitutes the interfaces of "
      & "the project.");

   --  Project_Files
   Set_Attribute_Description
     (Key          => PRA.Project_Files,
      Description  =>
        "Value is the list of aggregated projects.");

   --  Project_Path
   Set_Attribute_Description
     (Key          => PRA.Project_Path,
      Description  =>
        "Value is a list of directories that are added to the project search"
      & " path when looking for the aggregated projects.");

   --  External
   Set_Attribute_Description
     (Key          => PRA.External,
      Description  =>
        "Index is the name of an external reference. Value is the value of "
      & "the external reference to be used when parsing the aggregated "
      & "projects.");

   --  Library_Dir
   Set_Attribute_Description
     (Key          => PRA.Library_Dir,
      Description  =>
        "Value is the name of the library directory. This attribute needs to"
      & " be declared for each library project.");

   --  Library_Name
   Set_Attribute_Description
     (Key          => PRA.Library_Name,
      Description  =>
        "Value is the name of the library. This attribute needs to be "
      & "declared or inherited for each library project.");

   --  Library_Kind
   Set_Attribute_Description
     (Key          => PRA.Library_Kind,
      Description  =>
        "Specifies the kind of library: static library (archive) or shared "
      & "library. Case-insensitive values must be one of 'static' for "
      & "archives (the default), 'static-pic' for archives of Position "
      & "Independent Code, or 'dynamic' or 'relocatable' for shared "
      & "libraries.");

   --  Library_Version
   Set_Attribute_Description
     (Key          => PRA.Library_Version,
      Description  =>
        "Value is the name of the library file.");

   --  Library_Interface
   Set_Attribute_Description
     (Key          => PRA.Library_Interface,
      Description  =>
        "Value is the list of unit names that constitutes the interfaces of "
      & "a Stand-Alone Library project.");

   --  Library_Standalone
   Set_Attribute_Description
     (Key          => PRA.Library_Standalone,
      Description  =>
        "Specifies if a Stand-Alone Library (SAL) is encapsulated or not. "
      & "Only authorized case-insensitive values are 'standard' for non "
      & "encapsulated SALs, 'encapsulated' for encapsulated SALs or 'no' for"
      & " non SAL library project.");

   --  Library_Encapsulated_Options
   Set_Attribute_Description
     (Key          => PRA.Library_Encapsulated_Options,
      Description  =>
        "Value is a list of options that need to be used when linking an "
      & "encapsulated Stand-Alone Library.");

   --  Library_Encapsulated_Supported
   Set_Attribute_Description
     (Key          => PRA.Library_Encapsulated_Supported,
      Description  =>
        "Indicates if encapsulated Stand-Alone Libraries are supported. Only"
      & " authorized case-insensitive values are 'true' and 'false' (the "
      & "default).");

   --  Library_Auto_Init
   Set_Attribute_Description
     (Key          => PRA.Library_Auto_Init,
      Description  =>
        "Indicates if a Stand-Alone Library is auto-initialized. Only "
      & "authorized case-insensitive values are 'true' and 'false'. "
      & "Default to Library_Auto_Init_Supported value. Can not "
      & "be set to true if Library_Auto_Init_Supported is false.");

   --  Leading_Library_Options
   Set_Attribute_Description
     (Key          => PRA.Leading_Library_Options,
      Description  =>
        "Value is a list of options that are to be used at the beginning of "
      & "the command line when linking a shared library.");

   --  Library_Options
   Set_Attribute_Description
     (Key          => PRA.Library_Options,
      Description  =>
        "Value is a list of options that are to be used when linking a "
      & "shared library.");

   --  Library_Rpath_Options
   Set_Attribute_Description
     (Key          => PRA.Library_Rpath_Options,
      Description  =>
        "Index is a language name. Value is a list of options for an "
      & "invocation of the compiler of the language. This invocation is done"
      & " for a shared library project with sources of the language. The "
      & "output of the invocation is the path name of a shared library file."
      & " The directory name is to be put in the run path option switch when"
      & " linking the shared library for the project.");

   --  Library_Src_Dir
   Set_Attribute_Description
     (Key          => PRA.Library_Src_Dir,
      Description  =>
        "Value is the name of the directory where copies of the sources of "
      & "the interfaces of a Stand-Alone Library are to be copied.");

   --  Library_ALI_Dir
   Set_Attribute_Description
     (Key          => PRA.Library_Ali_Dir,
      Description  =>
        "Value is the name of the directory where the ALI files of the "
      & "interfaces of a Stand-Alone Library are to be copied. When this "
      & "attribute is not declared, the directory is the library directory.");

   --  Library_Symbol_File
   Set_Attribute_Description
     (Key          => PRA.Library_Symbol_File,
      Description  =>
        "Value is the name of the library symbol file.");

   --  Library_Symbol_Policy
   Set_Attribute_Description
     (Key          => PRA.Library_Symbol_Policy,
      Description  =>
        "Indicates the symbol policy kind. Only authorized case-insensitive "
      & "values are 'restricted', 'unrestricted'.");

   --  Library_Reference_Symbol_File
   Set_Attribute_Description
     (Key          => PRA.Library_Reference_Symbol_File,
      Description  =>
        "Value is the name of the reference symbol file.");

   --  Linker_Lib_Dir_Option
   Set_Attribute_Description
     (Key => PRA.Linker_Lib_Dir_Option,
      Description => "Option used to add a library directory to the linker "
        & "search path.");

   --  Default_Language
   Set_Attribute_Description
     (Key          => PRA.Default_Language,
      Description  =>
        "Value is the case-insensitive name of the language of a project "
      & "when attribute Languages is not specified.");

   --  Run_Path_Option
   Set_Attribute_Description
     (Key          => PRA.Run_Path_Option,
      Description  =>
        "Value is the list of switches to be used when specifying the run "
      & "path option in an executable.");

   --  Run_Path_Origin
   Set_Attribute_Description
     (Key          => PRA.Run_Path_Origin,
      Description  =>
        "Value is the string that may replace the path name of the "
      & "executable directory in the run path options.");

   --  Separate_Run_Path_Options
   Set_Attribute_Description
     (Key          => PRA.Separate_Run_Path_Options,
      Description  =>
        "Indicates if there may be several run path options specified when "
      & "linking an executable. Only authorized case-insensitive values are "
      & "'true' or 'false' (the default).");

   --  Toolchain_Version
   Set_Attribute_Description
     (Key          => PRA.Toolchain_Version,
      Description  =>
        "Index is a language name. Specify the version of a toolchain for a "
      & "language.");

   --  Required_Toolchain_Version
   Set_Attribute_Description
     (Key          => PRA.Required_Toolchain_Version,
      Description  =>
        "Index is a language name. Specify the value expected for the "
      & "Toolchain_Version attribute for this language, typically provided "
      & "by an auto-generated configuration project. If "
      & "Required_Toolchain_Version and Toolchain_Version do not match, the "
      & "project processing aborts with an error.");

   --  Toolchain_Description
   Set_Attribute_Description
     (Key          => PRA.Toolchain_Description,
      Description  =>
        "Obsolescent. No longer used.");

   --  Object_Generated
   Set_Attribute_Description
     (Key          => PRA.Object_Generated,
      Description  =>
        "Index is a language name. Indicates if invoking the compiler for a "
      & "language produces an object file. Only authorized case-insensitive "
      & "values are 'false' and 'true' (the default).");

   --  Objects_Linked
   Set_Attribute_Description
     (Key          => PRA.Objects_Linked,
      Description  =>
        "Index is a language name. Indicates if the object files created by "
      & "the compiler for a language need to be linked in the executable. "
      & "Only authorized case-insensitive values are 'false' and 'true' (the"
      & " default).");

   --  Target
   Set_Attribute_Description
     (Key          => PRA.Target,
      Description  =>
        "Value is the name of the target platform. Taken into account only "
      & "in the main project. Note that when the target is specified on the "
      & "command line (usually with a switch ``--target=``), the value of "
      & "attribute reference 'Target is the one specified on the command "
      & "line.");

   --  Runtime
   Set_Attribute_Description
     (Key          => PRA.Runtime,
      Description  =>
        "Index is a language name. Indicates the runtime directory that is "
      & "to be used when using the compiler of the language. Taken into "
      & "account only in the main project, or its extended projects if any. "
      & "Note that when the runtime is specified for a language on the "
      & "command line (usually with a switch ``--RTS``), the value of "
      & "attribute reference 'Runtime for this language is the one "
      & "specified on the command line.");

   --  Runtime_Dir
   Set_Attribute_Description
     (Key          => PRA.Runtime_Dir,
      Description  =>
        "Index is a language name. Value is the path name of the runtime "
      & "directory for the language.");

   --  Runtime_Library_Dir
   Set_Attribute_Description
     (Key          => PRA.Runtime_Library_Dir,
      Description  =>
        "Index is a language name. Value is the path name of the directory "
      & "where the runtime libraries are located. This attribute is "
      & "obsolete.");

   --  Runtime_Source_Dirs
   Set_Attribute_Description
     (Key          => PRA.Runtime_Source_Dirs,
      Description  =>
        "Index is a language name. Value is the path names of the "
      & "directories where the sources of runtime libraries are located. "
      & "This attribute is not normally declared.");

   --  Runtime_Source_Dir
   Set_Attribute_Description
     (Key          => PRA.Runtime_Source_Dir,
      Description  =>
        "Index is a language name. Value is the path name of the directory "
      & "where the sources of runtime libraries are located. This attribute "
      & "is obsolete.");

   --  Toolchain_Name
   Set_Attribute_Description
     (Key          => PRA.Toolchain_Name,
      Description  =>
        "Index is a language name. Indicates the toolchain name that is to "
      & "be used when using the compiler of the language. Taken into account"
      & " only in the main project, or its extended projects if any.");

   --  Library_Builder
   Set_Attribute_Description
     (Key          => PRA.Library_Builder,
      Description  =>
        "Value is the path name of the application that is to be used to "
      & "build libraries. Usually the path name of 'gprlib'.");

   --  Library_Support
   Set_Attribute_Description
     (Key          => PRA.Library_Support,
      Description  =>
        "Indicates the level of support of libraries. Only authorized "
      & "case-insensitive values are 'static_only', 'full' or 'none' (the "
      & "default).");

   --  Archive_Builder
   Set_Attribute_Description
     (Key          => PRA.Archive_Builder,
      Description  =>
        "Value is the name of the application to be used to create a static "
      & "library (archive), followed by the options to be used. If an empty "
      & "value is provided, object files used as a recipe for the archive "
      & "are copied to the library directory instead.");

   --  Archive_Builder_Append_Option
   Set_Attribute_Description
     (Key          => PRA.Archive_Builder_Append_Option,
      Description  =>
        "Value is the list of options to be used when invoking the archive "
      & "builder to add project files into an archive.");

   --  Archive_Indexer
   Set_Attribute_Description
     (Key          => PRA.Archive_Indexer,
      Description  =>
        "Value is the name of the archive indexer, followed by the required "
      & "options.");

   --  Archive_Prefix
   Set_Attribute_Description
     (Key          => PRA.Archive_Prefix,
      Description  =>
        "Value is the prefix of archives. When not declared, the "
      & "prefix is 'lib'.");

   --  Archive_Suffix
   Set_Attribute_Description
     (Key          => PRA.Archive_Suffix,
      Description  =>
        "Value is the extension of archives. When not declared, the "
      & "extension is '.a'.");

   --  Library_Partial_Linker
   Set_Attribute_Description
     (Key          => PRA.Library_Partial_Linker,
      Description  =>
        "Value is the name of the partial linker executable, followed by the"
      & " required options. If set to an empty list, partial linking is not"
      & " performed.");

   --  Shared_Library_Prefix
   Set_Attribute_Description
     (Key          => PRA.Shared_Library_Prefix,
      Description  =>
        "Value is the prefix in the name of shared library files. When not "
      & "declared, the prefix is 'lib'.");

   --  Shared_Library_Suffix
   Set_Attribute_Description
     (Key          => PRA.Shared_Library_Suffix,
      Description  =>
        "Value is the extension of the name of shared library files. When "
      & "not declared, the extension is '.so'.");

   --  Symbolic_Link_Supported
   Set_Attribute_Description
     (Key          => PRA.Symbolic_Link_Supported,
      Description  =>
        "Indicates if symbolic links are supported on the platform. Only "
      & "authorized case-insensitive values are 'true' and 'false' (the "
      & "default).");

   --  Library_Major_Minor_Id_Supported
   Set_Attribute_Description
     (Key          => PRA.Library_Major_Minor_Id_Supported,
      Description  =>
        "Indicates if major and minor ids for shared library names are "
      & "supported on the platform. Only authorized case-insensitive values "
      & "are 'true' and 'false' (the default).");

   --  Library_Auto_Init_Supported
   Set_Attribute_Description
     (Key          => PRA.Library_Auto_Init_Supported,
      Description  =>
        "Indicates if auto-initialization of Stand-Alone Libraries is "
      & "supported. Only authorized case-insensitive values are 'true' and "
      & "'false' (the default).");

   --  Shared_Library_Minimum_Switches
   Set_Attribute_Description
     (Key          => PRA.Shared_Library_Minimum_Switches,
      Description  =>
        "Value is the list of required switches when linking a shared "
      & "library.");

   --  Library_Version_Switches
   Set_Attribute_Description
     (Key          => PRA.Library_Version_Switches,
      Description  =>
        "Value is the list of switches to specify a internal name for a "
      & "shared library.");

   --  Library_Install_Name_Option
   Set_Attribute_Description
     (Key          => PRA.Library_Install_Name_Option,
      Description  =>
        "Value is the name of the option that needs to be used, concatenated"
      & " with the path name of the library file, when linking a shared "
      & "library.");

   --  Binder.Default_Switches
   Set_Attribute_Description
     (Key          => PRA.Binder.Default_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to be used "
      & "when binding code of the language, if there is no applicable "
      & "attribute Switches.");

   --  Binder.Switches
   Set_Attribute_Description
     (Key          => PRA.Binder.Switches,
      Description  =>
        "Index is either a language name or a source file name. Value is the"
      & " list of switches to be used when binding code. Index is either the"
      & " source file name of the executable to be bound or the language "
      & "name of the code to be bound.");

   --  Binder.Driver
   Set_Attribute_Description
     (Key          => PRA.Binder.Driver,
      Description  =>
        "Index is a language name. Value is the name of the application to "
      & "be used when binding code of the language.");

   --  Binder.Required_Switches
   Set_Attribute_Description
     (Key          => PRA.Binder.Required_Switches,
      Description  =>
        "Index is a language name. Value is the list of the required "
      & "switches to be used when binding code of the language.");

   --  Binder.Prefix
   Set_Attribute_Description
     (Key          => PRA.Binder.Prefix,
      Description  =>
        "Index is a language name. Value is a prefix to be used for the "
      & "binder exchange file name for the language. Used to have different "
      & "binder exchange file names when binding different languages.");

   --  Binder.Objects_Path
   Set_Attribute_Description
     (Key          => PRA.Binder.Objects_Path,
      Description  =>
        "Index is a language name. Value is the name of the environment "
      & "variable that contains the path for the object directories.");

   --  Binder.Object_Path_File
   Set_Attribute_Description
     (Key          => PRA.Binder.Objects_Path_File,
      Description  =>
        "Index is a language name. Value is the name of the environment "
      & "variable. The value of the environment variable is the path name of"
      & " a text file that contains the list of object directories.");

   --  Builder.Default_Switches
   Set_Attribute_Description
     (Key          => PRA.Builder.Default_Switches,
      Description  =>
        "Index is a language name. Value is the list of builder switches to "
      & "be used when building an executable of the language, if there is no"
      & " applicable attribute Switches.");

   --  Builder.Switches
   Set_Attribute_Description
     (Key          => PRA.Builder.Switches,
      Description  =>
        "Index is either a language name or a source file name. Value is the"
      & " list of builder switches to be used when building an executable. "
      & "Index is either the source file name of the executable to be built "
      & "or its language name.");

   --  Builder.Global_Compilation_Switches
   Set_Attribute_Description
     (Key          => PRA.Builder.Global_Compilation_Switches,
      Description  =>
        "Index is a language name. Value is the list of compilation switches"
      & " to be used when building an executable. Index is either the source"
      & " file name of the executable to be built or its language name.");

   --  Builder.Executable
   Set_Attribute_Description
     (Key          => PRA.Builder.Executable,
      Description  =>
        "Index is an executable source file name. Value is the simple file "
      & "name of the executable to be built.");

   --  Builder.Executable_Suffix
   Set_Attribute_Description
     (Key          => PRA.Builder.Executable_Suffix,
      Description  =>
        "Value is the extension of the file name of executables. The actual "
      & "default value for the extension depends on the host: ``.exe`` on "
      & "windows, else an empty string.");

   --  Builder.Global_Configuration_Pragmas
   Set_Attribute_Description
     (Key          => PRA.Builder.Global_Configuration_Pragmas,
      Description  =>
        "Value is the file name of a configuration pragmas file that is "
      & "specified to the Ada compiler when compiling any Ada source in the "
      & "project tree.");

   --  Builder.Global_Config_File
   Set_Attribute_Description
     (Key          => PRA.Builder.Global_Config_File,
      Description  =>
        "Index is a language name. Value is the file name of a configuration"
      & " file that is specified to the compiler when compiling any source "
      & "of the language in the project tree.");

   --  Clean.Switches
   Set_Attribute_Description
     (Key          => PRA.Clean.Switches,
      Description  =>
        "Taken into account only in the main project. Value is a list of "
      & "switches to be used by the cleaning application.");

   --  Clean.Source_Artifact_Extensions
   Set_Attribute_Description
     (Key          => PRA.Clean.Source_Artifact_Extensions,
      Description  =>
        "Index is a language names. Value is the list of extensions for file"
      & " names derived from object file names that need to be cleaned in "
      & "the object directory of the project.");

   --  Clean.Object_Artifact_Extensions
   Set_Attribute_Description
     (Key          => PRA.Clean.Object_Artifact_Extensions,
      Description  =>
        "Index is a language names. Value is the list of extensions for file"
      & " names derived from source file names that need to be cleaned in "
      & "the object directory of the project.");

   --  Clean.Artifacts_In_Object_Dir
   Set_Attribute_Description
     (Key          => PRA.Clean.Artifacts_In_Object_Dir,
      Description  =>
        "Value is a list of file names expressed as regular expressions that"
      & " are to be deleted by gprclean in the object directory of the "
      & "project.");

   --  Clean.Artifacts_In_Exec_Dir
   Set_Attribute_Description
     (Key          => PRA.Clean.Artifacts_In_Exec_Dir,
      Description  =>
        "Value is list of file names expressed as regular expressions that "
      & "are to be deleted by gprclean in the exec directory of the main "
      & "project.");

   --  Compiler.Default_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Default_Switches,
      Description  =>
        "Index is a language name. Value is a list of switches to be used "
      & "when invoking the compiler for the language for a source of the "
      & "project, if there is no applicable attribute Switches.");

   --  Compiler.Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Switches,
      Description  =>
        "Index is a source file name or a language name. Value is the list "
      & "of switches to be used when invoking the compiler for the source or"
      & " for its language.");

   --  Compiler.Local_Configuration_Pragmas
   Set_Attribute_Description
     (Key          => PRA.Compiler.Local_Configuration_Pragmas,
      Description  =>
        "Value is the file name of a configuration pragmas file that is "
      & "specified to the Ada compiler when compiling any Ada source in the "
      & "project.");

   --  Compiler.Local_Config_File
   Set_Attribute_Description
     (Key          => PRA.Compiler.Local_Config_File,
      Description  =>
        "Index is a language name. Value is the file name of a configuration"
      & " file that is specified to the compiler when compiling any source "
      & "of the language in the project.");

   --  Compiler.Driver
   Set_Attribute_Description
     (Key          => PRA.Compiler.Driver,
      Description  =>
        "Index is a language name. Value is the name of the executable for "
      & "the compiler of the language.");

   --  Compiler.Language_Kind
   Set_Attribute_Description
     (Key          => PRA.Compiler.Language_Kind,
      Description  =>
        "Index is a language name. Indicates the kind of the language, "
      & "either file based or unit based. Only authorized case-insensitive "
      & "values are 'unit_based' and 'file_based' (the default).");

   --  Compiler.Dependency_Kind
   Set_Attribute_Description
     (Key          => PRA.Compiler.Dependency_Kind,
      Description  =>
        "Index is a language name. Indicates how the dependencies are "
      & "handled for the language. Only authorized case-insensitive values "
      & "are 'makefile', 'ali_file', 'ali_closure' or 'none' (the default).");

   --  Compiler.Required_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Required_Switches,
      Description  =>
        "Equivalent to attribute Leading_Required_Switches.");

   --  Compiler.Leading_Required_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Leading_Required_Switches,
      Description  =>
        "Index is a language name. Value is the list of the minimum switches"
      & " to be used at the beginning of the command line when invoking the "
      & "compiler for the language.");

   --  Compiler.Trailing_Required_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Trailing_Required_Switches,
      Description  =>
        "Index is a language name. Value is the list of the minimum switches"
      & " to be used at the end of the command line when invoking the "
      & "compiler for the language.");

   --  Compiler.PIC_Option
   Set_Attribute_Description
     (Key          => PRA.Compiler.Pic_Option,
      Description  =>
        "Index is a language name. Value is the list of switches to be used "
      & "when compiling a source of the language when the project is a "
      & "shared library project.");

   --  Compiler.Source_File_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Source_File_Switches,
      Description  =>
        "Index is a language name. Value is a list of switches to be used "
      & "just before the path name of the source to compile when invoking "
      & "the compiler for a source of the language.");

   --  Compiler.Object_File_Suffix
   Set_Attribute_Description
     (Key          => PRA.Compiler.Object_File_Suffix,
      Description  =>
        "Index is a language name. Value is the extension of the object "
      & "files created by the compiler of the language. When not specified, "
      & "the extension is the default one for the platform.");

   --  Compiler.Object_File_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Object_File_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to be used "
      & "by the compiler of the language to specify the path name of the "
      & "object file. When not specified, the switch used is '-o'.");

   --  Compiler.Multi_Unit_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Multi_Unit_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to be used "
      & "to compile a unit in a multi unit source of the language. The index"
      & " of the unit in the source is concatenated with the last switches "
      & "in the list.");

   --  Compiler.Multi_Unit_Object_Separator
   Set_Attribute_Description
     (Key          => PRA.Compiler.Multi_Unit_Object_Separator,
      Description  =>
        "Index is a language name. Value is the string to be used in the "
      & "object file name before the index of the unit, when compiling a "
      & "unit in a multi unit source of the language.");

   --  Compiler.Mapping_File_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Mapping_File_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to be used "
      & "to specify a mapping file when invoking the compiler for a source "
      & "of the language.");

   --  Compiler.Mapping_Spec_Suffix
   Set_Attribute_Description
     (Key          => PRA.Compiler.Mapping_Spec_Suffix,
      Description  =>
        "Index is a language name. Value is the suffix to be used in a "
      & "mapping file to indicate that the source is a spec.");

   --  Compiler.Mapping_Body_Suffix
   Set_Attribute_Description
     (Key          => PRA.Compiler.Mapping_Body_Suffix,
      Description  =>
        "Index is a language name. Value is the suffix to be used in a "
      & "mapping file to indicate that the source is a body.");

   --  Compiler.Config_File_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_File_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to specify "
      & "to the compiler of the language a configuration file.");

   --  Compiler.Config_Body_File_Name
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_Body_File_Name,
      Description  =>
        "Index is a language name. Value is the template to be used to "
      & "indicate a configuration specific to a body of the language in a "
      & "configuration file.");

   --  Compiler.Config_Body_File_Name_Index
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_Body_File_Name_Index,
      Description  =>
        "Index is a language name. Value is the template to be used to "
      & "indicate a configuration specific to the body a unit in a multi "
      & "unit source of the language in a configuration file.");

   --  Compiler.Config_Body_File_Name_Pattern
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_Body_File_Name_Pattern,
      Description  =>
        "Index is a language name. Value is the template to be used to "
      & "indicate a configuration for all bodies of the languages in a "
      & "configuration file.");

   --  Compiler.Config_Spec_File_Name
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_Spec_File_Name,
      Description  =>
        "Index is a language name. Value is the template to be used to "
      & "indicate a configuration specific to a spec of the language in a "
      & "configuration file.");

   --  Compiler.Config_Spec_File_Name_Index
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_Spec_File_Name_Index,
      Description  =>
        "Index is a language name. Value is the template to be used to "
      & "indicate a configuration specific to the spec a unit in a multi "
      & "unit source of the language in a configuration file.");

   --  Compiler.Config_Spec_File_Name_Pattern
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_Spec_File_Name_Pattern,
      Description  =>
        "Index is a language name. Value is the template to be used to "
      & "indicate a configuration for all specs of the languages in a "
      & "configuration file.");

   --  Compiler.Config_File_Unique
   Set_Attribute_Description
     (Key          => PRA.Compiler.Config_File_Unique,
      Description  =>
        "Index is a language name. Indicates if there should be only one "
      & "configuration file specified to the compiler of the language. Only "
      & "authorized case-insensitive values are 'true' and 'false' (the "
      & "default).");

   --  Compiler.Dependency_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Dependency_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to be used "
      & "to specify to the compiler the dependency file when the dependency "
      & "kind of the language is file based, and when Dependency_Driver is "
      & "not specified for the language.");

   --  Compiler.Dependency_Driver
   Set_Attribute_Description
     (Key          => PRA.Compiler.Dependency_Driver,
      Description  =>
        "Index is a language name. Value is the name of the executable to be"
      & " used to create the dependency file for a source of the language, "
      & "followed by the required switches.");

   --  Compiler.Include_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Include_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to specify "
      & "to the compiler of the language to indicate a directory to look for"
      & " sources.");

   --  Compiler.Include_Path
   Set_Attribute_Description
     (Key          => PRA.Compiler.Include_Path,
      Description  =>
        "Index is a language name. Value is the name of an environment "
      & "variable that contains the path of all the directories that the "
      & "compiler of the language may search for sources.");

   --  Compiler.Include_Path_File
   Set_Attribute_Description
     (Key          => PRA.Compiler.Include_Path_File,
      Description  =>
        "Index is a language name. Value is the name of an environment "
      & "variable the value of which is the path name of a text file that "
      & "contains the directories that the compiler of the language may "
      & "search for sources.");

   --  Compiler.Object_Path_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Object_Path_Switches,
      Description  =>
        "Index is a language name. Value is the list of switches to specify "
      & "to the compiler of the language the name of a text file that "
      & "contains the list of object directories. When this attribute is not"
      & " declared, the text file is not created.");

   --  Compiler.Max_Command_Line_Length
   Set_Attribute_Description
     (Key          => PRA.Compiler.Max_Command_Line_Length,
      Description  =>
        "Value is the maximum number of character in the command line when "
      & "invoking a compiler that supports response files.");

   --  Compiler.Response_File_Format
   Set_Attribute_Description
     (Key          => PRA.Compiler.Response_File_Format,
      Description  =>
        "Indicates the kind of response file to create when the length of "
      & "the compiling command line is too large. The index is the name of "
      & "the language for the compiler. Only authorized case-insensitive "
      & "values are 'none', 'gnu', 'object_list', 'gcc_gnu', "
      & "'gcc_option_list' and 'gcc_object_list'.");

   --  Compiler.Response_File_Switches
   Set_Attribute_Description
     (Key          => PRA.Compiler.Response_File_Switches,
      Description  =>
        "Value is the list of switches to specify a response file for a "
      & "compiler. The index is the name of the language for the compiler.");

   --  Gnatls.Switches
   Set_Attribute_Description
     (Key          => PRA.Gnatls.Switches,
      Description  =>
        "Taken into account only in the main project. Value is a list of "
      & "switches to be used when invoking gnatls.");

   --  Install.Artifacts
   Set_Attribute_Description
     (Key          => PRA.Install.Artifacts,
      Description  =>
        "An indexed attribute to declare a set of files not part of the "
      & "sources to be installed. The array index is the directory where the"
      & " file is to be installed. If a relative directory then Prefix (see "
      & "below) is prepended. Note also that if the same file name occurs "
      & "multiple time in the attribute list, the last one will be the one "
      & "installed. If an artifact is not found a warning is displayed.");

   --  Install.Required_Artifacts
   Set_Attribute_Description
     (Key          => PRA.Install.Required_Artifacts,
      Description  =>
        "As above, but artifacts must be present or an error is reported.");

   --  Install.Prefix
   Set_Attribute_Description
     (Key          => PRA.Install.Prefix,
      Description  =>
        "Value is the install destination directory. If the value is a "
      & "relative path, it is taken as relative to the global prefix "
      & "directory. That is, either the value passed to --prefix option or "
      & "the default installation prefix.");

   --  Install.Sources_Subdir
   Set_Attribute_Description
     (Key          => PRA.Install.Sources_Subdir,
      Description  =>
        "Value is the sources directory or subdirectory of Prefix.");

   --  Install.Exec_Subdir
   Set_Attribute_Description
     (Key          => PRA.Install.Exec_Subdir,
      Description  =>
        "Value is the executables directory or subdirectory of Prefix.");

   --  Install.ALI_Subdir
   Set_Attribute_Description
     (Key          => PRA.Install.ALI_Subdir,
      Description  =>
        "Value is ALI directory or subdirectory of Prefix.");

   --  Install.Lib_Subdir
   Set_Attribute_Description
     (Key          => PRA.Install.Lib_Subdir,
      Description  =>
        "Value is library directory or subdirectory of Prefix.");

   --  Install.Project_Subdir
   Set_Attribute_Description
     (Key          => PRA.Install.Project_Subdir,
      Description  =>
        "Value is the project directory or subdirectory of Prefix.");

   --  Install.Active
   Set_Attribute_Description
     (Key          => PRA.Install.Active,
      Description  =>
        "Indicates that the project is to be installed or not. "
      & "Case-insensitive value 'false' means that the project is not to be "
      & "installed, all other values mean that the project is to be "
      & "installed.");

   --  Install.Mode
   Set_Attribute_Description
     (Key          => PRA.Install.Mode,
      Description  =>
        "Value is the installation mode, it is either 'dev' (default) or "
      & "'usage'.");

   --  Install.Install_Name
   Set_Attribute_Description
     (Key          => PRA.Install.Install_Name,
      Description  =>
        "Specify the name to use for recording the installation. The default"
      & " is the project name without the extension.");

   --  Install.Side_Debug
   Set_Attribute_Description
     (Key          => PRA.Install.Side_Debug,
      Description  =>
        "Indicates that the project's executable and shared libraries are to"
      & " be stripped of the debug symbols. Those debug symbols are written "
      & "into a side file named after the original file with the '.debug' "
      & "extension added. Case-insensitive value 'false' (default) disables "
      & "this feature. Set it to 'true' to activate.");

   --  Install.Install_Project
   Set_Attribute_Description
     (Key          => PRA.Install.Install_Project,
      Description  =>
        "Indicates that a project is to be generated and installed. The "
      & "value is either 'true' to 'false'. Default is 'true'.");

   --  Linker.Required_Switches
   Set_Attribute_Description
     (Key          => PRA.Linker.Required_Switches,
      Description  =>
        "Value is a list of switches that are required when invoking the "
      & "linker to link an executable.");

   --  Linker.Default_Switches
   Set_Attribute_Description
     (Key          => PRA.Linker.Default_Switches,
      Description  =>
        "Index is a language name. Value is a list of switches for the "
      & "linker when linking an executable for a main source of the "
      & "language, when there is no applicable Switches.");

   --  Linker.Leading_Switches
   Set_Attribute_Description
     (Key          => PRA.Linker.Leading_Switches,
      Description  =>
        "Index is a source file name or a language name. Value is the list "
      & "of switches to be used at the beginning of the command line when "
      & "invoking the linker to build an executable for the source or for "
      & "its language.");

   --  Linker.Switches
   Set_Attribute_Description
     (Key          => PRA.Linker.Switches,
      Description  =>
        "Index is a source file name or a language name. Value is the list "
      & "of switches to be used when invoking the linker to build an "
      & "executable for the source or for its language.");

   --  Linker.Trailing_Switches
   Set_Attribute_Description
     (Key          => PRA.Linker.Trailing_Switches,
      Description  =>
        "Index is a source file name or a language name. Value is the list "
      & "of switches to be used at the end of the command line when invoking"
      & " the linker to build an executable for the source or for its "
      & "language. These switches may override the Required_Switches.");

   --  Linker.Linker_Options
   Set_Attribute_Description
     (Key          => PRA.Linker.Linker_Options,
      Description  =>
        "This attribute specifies a list of additional switches to be given "
      & "to the linker when linking an executable. It is ignored when "
      & "defined in the main project and taken into account in all other "
      & "projects that are imported directly or indirectly. These switches "
      & "complement the Linker'Switches defined in the main project. This is"
      & " useful when a particular subsystem depends on an external library:"
      & " adding this dependency as a Linker_Options in the project of the "
      & "subsystem is more convenient than adding it to all the "
      & "Linker'Switches of the main projects that depend upon this "
      & "subsystem.");

   --  Linker. Map_File_Option
   Set_Attribute_Description
     (Key          => PRA.Linker.Map_File_Option,
      Description  =>
        "Value is the switch to specify the map file name that the linker "
      & "needs to create.");

   --  Linker.Driver
   Set_Attribute_Description
     (Key          => PRA.Linker.Driver,
      Description  =>
        "Value is the name of the linker executable.");

   --  Linker.Max_Command_Line_Length
   Set_Attribute_Description
     (Key          => PRA.Linker.Max_Command_Line_Length,
      Description  =>
        "Value is the maximum number of character in the command line when "
      & "invoking the linker to link an executable.");

   --  Linker.Response_File_Format
   Set_Attribute_Description
     (Key          => PRA.Linker.Response_File_Format,
      Description  =>
        "Indicates the kind of response file to create when the length of "
      & "the linking command line is too large. Only authorized "
      & "case-insensitive values are 'none', 'gnu', 'object_list', "
      & "'gcc_gnu', 'gcc_option_list' and 'gcc_object_list'.");

   --  Linker.Response_File_Switches
   Set_Attribute_Description
     (Key          => PRA.Linker.Response_File_Switches,
      Description  =>
        "Value is the list of switches to specify a response file to the "
      & "linker.");

   --  Linker.Group_Start_Switch
   Set_Attribute_Description
     (Key         => PRA.Linker.Group_Start_Switch,
      Description =>
        "Value is the switch to use to start a link group, a group of "
      & "libraries to be linked with recursively");

   --  Linker.Group_End_Switch
   Set_Attribute_Description
     (Key         => PRA.Linker.Group_End_Switch,
      Description =>
        "Value is the switch to use to end a link group, a group of "
      & "libraries to be linked with recursively");

   Set_Attribute_Description
     (Key         => PRA.Linker.Unconditional_Linking,
      Description => "When set for a language, this instructs the link phase "
      & "to always explicitly link with the produced objects");

   --  Naming.Specification_Suffix
   Set_Attribute_Description
     (Key          => PRA.Naming.Specification_Suffix,
      Description  =>
        "Equivalent to attribute Spec_Suffix.");

   --  Naming.Spec_Suffix
   Set_Attribute_Description
     (Key          => PRA.Naming.Spec_Suffix,
      Description  =>
        "Index is a language name. Value is the extension of file names for "
      & "specs of the language.");

   --  Naming.Implementation_Suffix
   Set_Attribute_Description
     (Key          => PRA.Naming.Implementation_Suffix,
      Description  =>
        "Equivalent to attribute Body_Suffix.");

   --  Naming.Body_Suffix
   Set_Attribute_Description
     (Key          => PRA.Naming.Body_Suffix,
      Description  =>
        "Index is a language name. Value is the extension of file names for "
      & "bodies of the language.");

   --  Naming.Separate_Suffix
   Set_Attribute_Description
     (Key          => PRA.Naming.Separate_Suffix,
      Description  =>
        "Value is the extension of file names for subunits of Ada.");

   --  Naming.Casing
   Set_Attribute_Description
     (Key          => PRA.Naming.Casing,
      Description  =>
        "Indicates the casing of sources of the Ada language. Only "
      & "authorized case-insensitive values are 'lowercase', 'uppercase' and"
      & " 'mixedcase'.");

   --  Naming.Dot_Replacement
   Set_Attribute_Description
     (Key          => PRA.Naming.Dot_Replacement,
      Description  =>
        "Value is the string that replace the dot of unit names in the "
      & "source file names of the Ada language.");

   --  Naming.Specification
   Set_Attribute_Description
     (Key          => PRA.Naming.Specification,
      Description  =>
        "Equivalent to attribute Spec.");

   --  Naming.Spec
   Set_Attribute_Description
     (Key          => PRA.Naming.Spec,
      Description  =>
        "Index is a unit name. Value is the file name of the spec of the "
      & "unit.");

   --  Naming.Implementation
   Set_Attribute_Description
     (Key          => PRA.Naming.Implementation,
      Description  =>
        "Equivalent to attribute Body.");

   --  Naming.Specification_Exceptions
   Set_Attribute_Description
     (Key          => PRA.Naming.Specification_Exceptions,
      Description  =>
        "Index is a language name. Value is a list of specs for the language"
      & " that do not necessarily follow the naming scheme for the language "
      & "and that may or may not be found in the source directories of the "
      & "project.");

   --  Naming.Implementation_Exceptions
   Set_Attribute_Description
     (Key          => PRA.Naming.Implementation_Exceptions,
      Description  =>
        "Index is a language name. Value is a list of bodies for the "
      & "language that do not necessarily follow the naming scheme for the "
      & "language and that may or may not be found in the source directories"
      & " of the project.");

   --  Naming.Body
   Set_Attribute_Description
     (Key          => PRA.Naming.Body_N,
      Description  =>
        "Index is a unit name. Value is the file name of"
      & " the body of the unit.");

end GPR2.Project.Registry.Attribute.Description;
