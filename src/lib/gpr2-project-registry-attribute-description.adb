------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GPR2.Project.Registry.Pack;

package body GPR2.Project.Registry.Attribute.Description is

   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;

   function Get_Attribute_Description (Key : Qualified_Name)
                                       return String is
   begin

      if Pack_Attribute_Description.Contains
        (Container =>  Attribute_Description,
         Key       =>  Key)
      then
         return Pack_Attribute_Description.Element
           (Container => Attribute_Description,
            Key       => Key);
      end if;

      return "Not available !";

   end Get_Attribute_Description;

   function Hash (Key : Qualified_Name)
                  return Hash_Type is (Ada.Strings.Hash (Image (Key)));

begin

   --  No_Package.Name
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Name),
      New_Item  =>
        "The name of the project."
     );

   --  No_Package.Project_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Project_Dir),
      New_Item  =>
        "The path name of the project directory."
     );

   --  No_Package.Main
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Main),
      New_Item  =>
        "The list of main sources for the executables."
     );

   --  No_Package.Languages
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Languages),
      New_Item  =>
        "The list of languages of the sources of the project."
     );

   --  No_Package.Roots
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Roots),
      New_Item  =>
        "The index is the file name of an executable source. Indicates the " &
        "list of units from the main project that need to be bound and " &
        "linked with their closures with the executable. The index is either" &
        " a file name, a language name or '*'. The roots for an executable " &
        "source are those in Roots with an index that is the executable " &
        "source file name, if declared. Otherwise, they are those in Roots " &
        "with an index that is the language name of the executable source, " &
        "if present. Otherwise, they are those in Roots ('*'), if declared. " &
        "If none of these three possibilities are declared, then there are " &
        "no roots for the executable source."
     );

   --  No_Package.Externally_Built
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Externally_Built),
      New_Item  =>
        "Indicates if the project is externally built. Only case-insensitive" &
        " values allowed are 'true' and 'false', the default."
     );

   --  No_Package.Warning_Message
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Warning_Message),
      New_Item  =>
        "Causes gprbuild to emit a user-defined warning message."
     );

   --  No_Package.Object_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Object_Dir),
      New_Item  =>
        "Indicates the object directory for the project."
     );

   --  No_Package.Exec_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Exec_Dir),
      New_Item  =>
        "Indicates the exec directory for the project, that is the directory" &
        " where the executables are."
     );

   --  No_Package.Create_Missing_Dirs
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Create_Missing_Dirs),
      New_Item  =>
        "Indicates if the missing object, library and executable directories" &
        " should be created automatically by the project-aware tool. Taken " &
        "into account only in the main project. Only authorized " &
        "case-insensitive values are 'true' and 'false'."
     );

   --  No_Package.Source_Dirs
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Source_Dirs),
      New_Item  =>
        "The list of source directories of the project."
     );

   --  No_Package.Inherit_Source_Path
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Inherit_Source_Path),
      New_Item  =>
        "Index is a language name. Value is a list of language names. " &
        "Indicates that in the source search path of the index language the " &
        "source directories of the languages in the list should be included."
     );

   --  No_Package.Exclude_Source_Dirs
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Excluded_Source_Dirs),
      New_Item  =>
        "The list of directories that are included in Source_Dirs but are " &
        "not source directories of the project."
     );

   --  No_Package.Ignore_Source_Sub_Dirs
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Ignore_Source_Sub_Dirs),
      New_Item  =>
        "Value is a list of simple names or patterns for subdirectories that" &
        " are removed from the list of source directories, including their " &
        "subdirectories."
     );

   --  No_Package.Source_Files
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Source_Files),
      New_Item  =>
        "Value is a list of source file simple names."
     );

   --  No_Package.Locally_Removed_Files
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Locally_Removed_Files),
      New_Item  =>
        "Obsolescent. Equivalent to Excluded_Source_Files."
     );

   --  No_Package.Excluded_Source_Files
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Excluded_Source_Files),
      New_Item  =>
        "Value is a list of simple file names that are not sources of the " &
        "project. Allows to remove sources that are inherited or found in " &
        "the source directories and that match the naming scheme."
     );

   --  No_Package.Source_List_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Source_List_File),
      New_Item  =>
        "Value is a text file name that contains a list of source file " &
        "simple names, one on each line."
     );

   --  No_Package.Excluded_Source_List_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Excluded_Source_List_File),
      New_Item  =>
        "Value is a text file name that contains a list of file simple names" &
        " that are not sources of the project."
     );

   --  No_Package.Interfaces
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Interfaces),
      New_Item  =>
        "Value is a list of file names that constitutes the interfaces of " &
        "the project."
     );

   --  No_Package.Project_Files
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Project_Files),
      New_Item  =>
        "Value is the list of aggregated projects."
     );

   --  No_Package.Project_Path
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Project_Path),
      New_Item  =>
        "Value is a list of directories that are added to the project search" &
        " path when looking for the aggregated projects."
     );

   --  No_Package.External
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.External),
      New_Item  =>
        "Index is the name of an external reference. Value is the value of " &
        "the external reference to be used when parsing the aggregated " &
        "projects."
     );

   --  No_Package.Library_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Dir),
      New_Item  =>
        "Value is the name of the library directory. This attribute needs to" &
        " be declared for each library project."
     );

   --  No_Package.Library_Name
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Name),
      New_Item  =>
        "Value is the name of the library. This attribute needs to be " &
        "declared or inherited for each library project."
     );

   --  No_Package.Library_Kind
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Kind),
      New_Item  =>
        "Specifies the kind of library: static library (archive) or shared " &
        "library. Case-insensitive values must be one of 'static' for " &
        "archives (the default), 'static-pic' for archives of Position " &
        "Independent Code, or 'dynamic' or 'relocatable' for shared " &
        "libraries."
     );

   --  No_Package.Library_Version
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Version),
      New_Item  =>
        "Value is the name of the library file."
     );

   --  No_Package.Library_Interface
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Interface),
      New_Item  =>
        "Value is the list of unit names that constitutes the interfaces of " &
        "a Stand-Alone Library project."
     );

   --  No_Package.Library_Standalone
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Standalone),
      New_Item  =>
        "Specifies if a Stand-Alone Library (SAL) is encapsulated or not. " &
        "Only authorized case-insensitive values are 'standard' for non " &
        "encapsulated SALs, 'encapsulated' for encapsulated SALs or 'no' for" &
        " non SAL library project."
     );

   --  No_Package.Library_Encapsulated_Options
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Encapsulated_Options),
      New_Item  =>
        "Value is a list of options that need to be used when linking an " &
        "encapsulated Stand-Alone Library."
     );

   --  No_Package.Library_Encapsulated_Supported
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Encapsulated_Supported),
      New_Item  =>
        "Indicates if encapsulated Stand-Alone Libraries are supported. Only" &
        " authorized case-insensitive values are 'true' and 'false' (the " &
        "default)."
     );

   --  No_Package.Library_Auto_Init
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Auto_Init),
      New_Item  =>
        "Indicates if a Stand-Alone Library is auto-initialized. Only " &
        "authorized case-insensitive values are 'true' and 'false'."
     );

   --  No_Package.Leading_Library_Options
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Leading_Library_Options),
      New_Item  =>
        "Value is a list of options that are to be used at the beginning of " &
        "the command line when linking a shared library."
     );

   --  No_Package.Library_Options
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Options),
      New_Item  =>
        "Value is a list of options that are to be used when linking a " &
        "shared library."
     );

   --  No_Package.Library_Rpath_Options
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Rpath_Options),
      New_Item  =>
        "Index is a language name. Value is a list of options for an " &
        "invocation of the compiler of the language. This invocation is done" &
        " for a shared library project with sources of the language. The " &
        "output of the invocation is the path name of a shared library file." &
        " The directory name is to be put in the run path option switch when" &
        " linking the shared library for the project."
     );

   --  No_Package.Library_Src_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Src_Dir),
      New_Item  =>
        "Value is the name of the directory where copies of the sources of " &
        "the interfaces of a Stand-Alone Library are to be copied."
     );

   --  No_Package.Library_ALI_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Ali_Dir),
      New_Item  =>
        "Value is the name of the directory where the ALI files of the " &
        "interfaces of a Stand-Alone Library are to be copied. When this " &
        "attribute is not declared, the directory is the library directory."
     );

   --  No_Package.Library_gcc
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Gcc),
      New_Item  =>
        "Obsolescent attribute. Specify the linker driver used to link a " &
        "shared library. Use instead attribute Linker'Driver."
     );

   --  No_Package.Library_Symbol_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Symbol_File),
      New_Item  =>
        "Value is the name of the library symbol file."
     );

   --  No_Package.Library_Symbol_Policy
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Symbol_Policy),
      New_Item  =>
        "Indicates the symbol policy kind. Only authorized case-insensitive " &
        "values are 'restricted', 'unrestricted'."
     );

   --  No_Package.Library_Reference_Symbol_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Reference_Symbol_File),
      New_Item  =>
        "Value is the name of the reference symbol file."
     );

   --  No_Package.Default_Language
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Default_Language),
      New_Item  =>
        "Value is the case-insensitive name of the language of a project " &
        "when attribute Languages is not specified."
     );

   --  No_Package.Run_Path_Option
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Run_Path_Option),
      New_Item  =>
        "Value is the list of switches to be used when specifying the run " &
        "path option in an executable."
     );

   --  No_Package.Run_Path_Origin
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Run_Path_Origin),
      New_Item  =>
        "Value is the string that may replace the path name of the " &
        "executable directory in the run path options."
     );

   --  No_Package.Separate_Run_Path_Options
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Separate_Run_Path_Options),
      New_Item  =>
        "Indicates if there may be several run path options specified when " &
        "linking an executable. Only authorized case-insensitive values are " &
        "'true' or 'false' (the default)."
     );

   --  No_Package.Toolchain_Version
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Toolchain_Version),
      New_Item  =>
        "Index is a language name. Specify the version of a toolchain for a " &
        "language."
     );

   --  No_Package.Required_Toolchain_Version
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Required_Toolchain_Version),
      New_Item  =>
        "Index is a language name. Specify the value expected for the " &
        "Toolchain_Version attribute for this language, typically provided " &
        "by an auto-generated configuration project. If " &
        "Required_Toolchain_Version and Toolchain_Version do not match, the " &
        "project processing aborts with an error."
     );

   --  No_Package.Toolchain_Description
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Toolchain_Description),
      New_Item  =>
        "Obsolescent. No longer used."
     );

   --  No_Package.Object_Generated
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Object_Generated),
      New_Item  =>
        "Index is a language name. Indicates if invoking the compiler for a " &
        "language produces an object file. Only authorized case-insensitive " &
        "values are 'false' and 'true' (the default)."
     );

   --  No_Package.Objects_Linked
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Objects_Linked),
      New_Item  =>
        "Index is a language name. Indicates if the object files created by " &
        "the compiler for a language need to be linked in the executable. " &
        "Only authorized case-insensitive values are 'false' and 'true' (the" &
        " default)."
     );

   --  No_Package.Target
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Target),
      New_Item  =>
        "Value is the name of the target platform. Taken into account only " &
        "in the main project. ote that when the target is specified on the " &
        "command line (usually with a switch --target=), the value of " &
        "attribute reference 'Target is the one specified on the command " &
        "line."
     );

   --  No_Package.Runtime
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Runtime),
      New_Item  =>
        "Index is a language name. Indicates the runtime directory that is " &
        "to be used when using the compiler of the language. Taken into " &
        "account only in the main project, or its extended projects if any. " &
        "Note that when the runtime is specified for a language on the " &
        "command line (usually with a switch --RTS), the value of attribute " &
        "reference 'Runtime for this language is the one specified on the " &
        "command line."
     );

   --  No_Package.Runtime_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Runtime_Dir),
      New_Item  =>
        "Index is a language name. Value is the path name of the runtime " &
        "directory for the language."
     );

   --  No_Package.Runtime_Library_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Runtime_Library_Dir),
      New_Item  =>
        "Index is a language name. Value is the path name of the directory " &
        "where the runtime libraries are located. This attribute is " &
        "obsolete."
     );

   --  No_Package.Runtime_Source_Dirs
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Runtime_Source_Dirs),
      New_Item  =>
        "Index is a language name. Value is the path names of the " &
        "directories where the sources of runtime libraries are located. " &
        "This attribute is not normally declared."
     );

   --  No_Package.Runtime_Source_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Runtime_Source_Dir),
      New_Item  =>
        "Index is a language name. Value is the path name of the directory " &
        "where the sources of runtime libraries are located. This attribute " &
        "is obsolete."
     );

   --  No_Package.Toolchain_Name
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Toolchain_Name),
      New_Item  =>
        "Index is a language name. Indicates the toolchain name that is to " &
        "be used when using the compiler of the language. Taken into account" &
        " only in the main project, or its extended projects if any."
     );

   --  No_Package.Library_Builder
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Builder),
      New_Item  =>
        "Value is the path name of the application that is to be used to " &
        "build libraries. Usually the path name of 'gprlib'."
     );

   --  No_Package.Library_Support
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Support),
      New_Item  =>
        "Indicates the level of support of libraries. Only authorized " &
        "case-insensitive values are 'static_only', 'full' or 'none' (the " &
        "default)."
     );

   --  No_Package.Archive_Builder
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Archive_Builder),
      New_Item  =>
        "Value is the name of the application to be used to create a static " &
        "library (archive), followed by the options to be used."
     );

   --  No_Package.Archive_Builder_Append_Option
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Archive_Builder_Append_Option),
      New_Item  =>
        "Value is the list of options to be used when invoking the archive " &
        "builder to add project files into an archive."
     );

   --  No_Package.Archive_Indexer
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Archive_Indexer),
      New_Item  =>
        "Value is the name of the archive indexer, followed by the required " &
        "options."
     );

   --  No_Package.Archive_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Archive_Suffix),
      New_Item  =>
        "Value is the extension of archives. When not declared, the " &
        "extension is '.a'."
     );

   --  No_Package.Library_Partial_Linker
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Partial_Linker),
      New_Item  =>
        "Value is the name of the partial linker executable, followed by the" &
        " required options."
     );

   --  No_Package.Shared_Library_Prefix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Shared_Library_Prefix),
      New_Item  =>
        "Value is the prefix in the name of shared library files. When not " &
        "declared, the prefix is 'lib'."
     );

   --  No_Package.Shared_Library_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Shared_Library_Suffix),
      New_Item  =>
        "Value is the extension of the name of shared library files. When " &
        "not declared, the extension is '.so'."
     );

   --  No_Package.Symbolic_Link_Supported
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Symbolic_Link_Supported),
      New_Item  =>
        "Indicates if symbolic links are supported on the platform. Only " &
        "authorized case-insensitive values are 'true' and 'false' (the " &
        "default)."
     );

   --  No_Package.Library_Major_Minor_Id_Supported
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Major_Minor_Id_Supported),
      New_Item  =>
        "Indicates if major and minor ids for shared library names are " &
        "supported on the platform. Only authorized case-insensitive values " &
        "are 'true' and 'false' (the default)."
     );

   --  No_Package.Library_Auto_Init_Supported
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Auto_Init_Supported),
      New_Item  =>
        "Indicates if auto-initialization of Stand-Alone Libraries is " &
        "supported. Only authorized case-insensitive values are 'true' and " &
        "'false' (the default)."
     );

   --  No_Package.Shared_Library_Minimum_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Shared_Library_Minimum_Switches),
      New_Item  =>
        "Value is the list of required switches when linking a shared " &
        "library."
     );

   --  No_Package.Library_Version_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Version_Switches),
      New_Item  =>
        "Value is the list of switches to specify a internal name for a " &
        "shared library."
     );

   --  No_Package.Library_Install_Name_Option
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => No_Package,
                    Attr => PRA.Library_Install_Name_Option),
      New_Item  =>
        "Value is the name of the option that needs to be used, concatenated" &
        " with the path name of the library file, when linking a shared " &
        "library."
     );

   --  Binder.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to be used " &
        "when binding code of the language, if there is no applicable " &
        "attribute Switches."
     );

   --  Binder.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is either a language name or a source file name. Value is the" &
        " list of switches to be used when binding code. Index is either the" &
        " source file name of the executable to be bound or the language " &
        "name of the code to be bound."
     );

   --  Binder.Driver
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Driver),
      New_Item  =>
        "Index is a language name. Value is the name of the application to " &
        "be used when binding code of the language."
     );

   --  Binder.Required_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Required_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of the required " &
        "switches to be used when binding code of the language."
     );

   --  Binder.Prefix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Prefix),
      New_Item  =>
        "Index is a language name. Value is a prefix to be used for the " &
        "binder exchange file name for the language. Used to have different " &
        "binder exchange file names when binding different languages."
     );

   --  Binder.Objects_Path
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Objects_Path),
      New_Item  =>
        "Index is a language name. Value is the name of the environment " &
        "variable that contains the path for the object directories."
     );

   --  Binder.Object_Path_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Binder,
                    Attr => PRA.Objects_Path_File),
      New_Item  =>
        "Index is a language name. Value is the name of the environment " &
        "variable. The value of the environment variable is the path name of" &
        " a text file that contains the list of object directories."
     );

   --  Builder.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of builder switches to " &
        "be used when building an executable of the language, if there is no" &
        " applicable attribute Switches."
     );

   --  Builder.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is either a language name or a source file name. Value is the" &
        " list of builder switches to be used when building an executable. " &
        "Index is either the source file name of the executable to be built " &
        "or its language name."
     );

   --  Builder.Global_Compilation_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Global_Compilation_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of compilation switches" &
        " to be used when building an executable. Index is either the source" &
        " file name of the executable to be built or its language name."
     );

   --  Builder.Executable
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Executable),
      New_Item  =>
        "Index is an executable source file name. Value is the simple file " &
        "name of the executable to be built."
     );

   --  Builder.Executable_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Executable_Suffix),
      New_Item  =>
        "Value is the extension of the file names of executable. When not " &
        "specified, the extension is the default extension of executables on" &
        " the platform."
     );

   --  Builder.Global_Configuration_Pragmas
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Global_Configuration_Pragmas),
      New_Item  =>
        "Value is the file name of a configuration pragmas file that is " &
        "specified to the Ada compiler when compiling any Ada source in the " &
        "project tree."
     );

   --  Builder.Global_Config_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Builder,
                    Attr => PRA.Global_Config_File),
      New_Item  =>
        "Index is a language name. Value is the file name of a configuration" &
        " file that is specified to the compiler when compiling any source " &
        "of the language in the project tree."
     );

   --  Check.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Check,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking gnatcheck for a source of the language, if there is " &
        "no applicable attribute Switches."
     );

   --  Check.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Check,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name. Value is the list of switches to be " &
        "used when invoking gnatcheck for the source."
     );

   --  Clean.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Clean,
                    Attr => PRA.Switches),
      New_Item  =>
        "Taken into account only in the main project. Value is a list of " &
        "switches to be used by the cleaning application."
     );

   --  Clean.Source_Artifact_Extensions
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Clean,
                    Attr => PRA.Source_Artifact_Extensions),
      New_Item  =>
        "Index is a language names. Value is the list of extensions for file" &
        " names derived from object file names that need to be cleaned in " &
        "the object directory of the project."
     );

   --  Clean.Object_Artifact_Extensions
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Clean,
                    Attr => PRA.Object_Artifact_Extensions),
      New_Item  =>
        "Index is a language names. Value is the list of extensions for file" &
        " names derived from source file names that need to be cleaned in " &
        "the object directory of the project."
     );

   --  Clean.Artifacts_In_Object_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Clean,
                    Attr => PRA.Artifacts_In_Object_Dir),
      New_Item  =>
        "Value is a list of file names expressed as regular expressions that" &
        " are to be deleted by gprclean in the object directory of the " &
        "project."
     );

   --  Clean.Artifacts_In_Exec_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Clean,
                    Attr => PRA.Artifacts_In_Exec_Dir),
      New_Item  =>
        "Value is list of file names expressed as regular expressions that " &
        "are to be deleted by gprclean in the exec directory of the main " &
        "project."
     );

   --  Compiler.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking the compiler for the language for a source of the " &
        "project, if there is no applicable attribute Switches."
     );

   --  Compiler.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name or a language name. Value is the list " &
        "of switches to be used when invoking the compiler for the source or" &
        " for its language."
     );

   --  Compiler.Local_Configuration_Pragmas
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Local_Configuration_Pragmas),
      New_Item  =>
        "Value is the file name of a configuration pragmas file that is " &
        "specified to the Ada compiler when compiling any Ada source in the " &
        "project."
     );

   --  Compiler.Local_Config_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Local_Config_File),
      New_Item  =>
        "Index is a language name. Value is the file name of a configuration" &
        " file that is specified to the compiler when compiling any source " &
        "of the language in the project."
     );

   --  Compiler.Driver
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Driver),
      New_Item  =>
        "Index is a language name. Value is the name of the executable for " &
        "the compiler of the language."
     );

   --  Compiler.Language_Kind
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Language_Kind),
      New_Item  =>
        "Index is a language name. Indicates the kind of the language, " &
        "either file based or unit based. Only authorized case-insensitive " &
        "values are 'unit_based' and 'file_based' (the default)."
     );

   --  Compiler.Dependency_Kind
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Dependency_Kind),
      New_Item  =>
        "Index is a language name. Indicates how the dependencies are " &
        "handled for the language. Only authorized case-insensitive values " &
        "are 'makefile', 'ali_file', 'ali_closure' or 'none' (the default)."
     );

   --  Compiler.Required_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Required_Switches),
      New_Item  =>
        "Equivalent to attribute Leading_Required_Switches."
     );

   --  Compiler.Leading_Required_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Leading_Required_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of the minimum switches" &
        " to be used at the beginning of the command line when invoking the " &
        "compiler for the language."
     );

   --  Compiler.Trailing_Required_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Trailing_Required_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of the minimum switches" &
        " to be used at the end of the command line when invoking the " &
        "compiler for the language."
     );

   --  Compiler.PIC_Option
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Pic_Option),
      New_Item  =>
        "Index is a language name. Value is the list of switches to be used " &
        "when compiling a source of the language when the project is a " &
        "shared library project."
     );

   --  Compiler.Source_File_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Source_File_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "just before the path name of the source to compile when invoking " &
        "the compiler for a source of the language."
     );

   --  Compiler.Object_File_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Object_File_Suffix),
      New_Item  =>
        "Index is a language name. Value is the extension of the object " &
        "files created by the compiler of the language. When not specified, " &
        "the extension is the default one for the platform."
     );

   --  Compiler.Object_File_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Object_File_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to be used " &
        "by the compiler of the language to specify the path name of the " &
        "object file. When not specified, the switch used is '-o'."
     );

   --  Compiler.Multi_Unit_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Multi_Unit_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to be used " &
        "to compile a unit in a multi unit source of the language. The index" &
        " of the unit in the source is concatenated with the last switches " &
        "in the list."
     );

   --  Compiler.Multi_Unit_Object_Separator
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Multi_Unit_Object_Separator),
      New_Item  =>
        "Index is a language name. Value is the string to be used in the " &
        "object file name before the index of the unit, when compiling a " &
        "unit in a multi unit source of the language."
     );

   --  Compiler.Mapping_File_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Mapping_File_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to be used " &
        "to specify a mapping file when invoking the compiler for a source " &
        "of the language."
     );

   --  Compiler.Mapping_Spec_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Mapping_Spec_Suffix),
      New_Item  =>
        "Index is a language name. Value is the suffix to be used in a " &
        "mapping file to indicate that the source is a spec."
     );

   --  Compiler.Mapping_Body_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Mapping_Body_Suffix),
      New_Item  =>
        "Index is a language name. Value is the suffix to be used in a " &
        "mapping file to indicate that the source is a body."
     );

   --  Compiler.Config_File_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_File_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to specify " &
        "to the compiler of the language a configuration file."
     );

   --  Compiler.Config_Body_File_Name
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_Body_File_Name),
      New_Item  =>
        "Index is a language name. Value is the template to be used to " &
        "indicate a configuration specific to a body of the language in a " &
        "configuration file."
     );

   --  Compiler.Config_Body_File_Name_Index
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_Body_File_Name_Index),
      New_Item  =>
        "Index is a language name. Value is the template to be used to " &
        "indicate a configuration specific to the body a unit in a multi " &
        "unit source of the language in a configuration file."
     );

   --  Compiler.Config_Body_File_Name_Pattern
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_Body_File_Name_Pattern),
      New_Item  =>
        "Index is a language name. Value is the template to be used to " &
        "indicate a configuration for all bodies of the languages in a " &
        "configuration file."
     );

   --  Compiler.Config_Spec_File_Name
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_Spec_File_Name),
      New_Item  =>
        "Index is a language name. Value is the template to be used to " &
        "indicate a configuration specific to a spec of the language in a " &
        "configuration file."
     );

   --  Compiler.Config_Spec_File_Name_Index
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_Spec_File_Name_Index),
      New_Item  =>
        "Index is a language name. Value is the template to be used to " &
        "indicate a configuration specific to the spec a unit in a multi " &
        "unit source of the language in a configuration file."
     );

   --  Compiler.Config_Spec_File_Name_Pattern
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_Spec_File_Name_Pattern),
      New_Item  =>
        "Index is a language name. Value is the template to be used to " &
        "indicate a configuration for all specs of the languages in a " &
        "configuration file."
     );

   --  Compiler.Config_File_Unique
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Config_File_Unique),
      New_Item  =>
        "Index is a language name. Indicates if there should be only one " &
        "configuration file specified to the compiler of the language. Only " &
        "authorized case-insensitive values are 'true' and 'false' (the " &
        "default)."
     );

   --  Compiler.Dependency_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Dependency_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to be used " &
        "to specify to the compiler the dependency file when the dependency " &
        "kind of the language is file based, and when Dependency_Driver is " &
        "not specified for the language."
     );

   --  Compiler.Dependency_Driver
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Dependency_Driver),
      New_Item  =>
        "Index is a language name. Value is the name of the executable to be" &
        " used to create the dependency file for a source of the language, " &
        "followed by the required switches."
     );

   --  Compiler.Include_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Include_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to specify " &
        "to the compiler of the language to indicate a directory to look for" &
        " sources."
     );

   --  Compiler.Include_Path
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Include_Path),
      New_Item  =>
        "Index is a language name. Value is the name of an environment " &
        "variable that contains the path of all the directories that the " &
        "compiler of the language may search for sources."
     );

   --  Compiler.Include_Path_File
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Include_Path_File),
      New_Item  =>
        "Index is a language name. Value is the name of an environment " &
        "variable the value of which is the path name of a text file that " &
        "contains the directories that the compiler of the language may " &
        "search for sources."
     );

   --  Compiler.Object_Path_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Object_Path_Switches),
      New_Item  =>
        "Index is a language name. Value is the list of switches to specify " &
        "to the compiler of the language the name of a text file that " &
        "contains the list of object directories. When this attribute is not" &
        " declared, the text file is not created."
     );

   --  Compiler.Max_Command_Line_Length
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Max_Command_Line_Length),
      New_Item  =>
        "Value is the maximum number of character in the command line when " &
        "invoking a compiler that supports response files."
     );

   --  Compiler.Response_File_Format
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Response_File_Format),
      New_Item  =>
        "Indicates the kind of response file to create when the length of " &
        "the compiling command line is too large. The index is the name of " &
        "the language for the compiler. Only authorized case-insensitive " &
        "values are 'none', 'gnu', 'object_list', 'gcc_gnu', " &
        "'gcc_option_list' and 'gcc_object_list'."
     );

   --  Compiler.Response_File_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Compiler,
                    Attr => PRA.Response_File_Switches),
      New_Item  =>
        "Value is the list of switches to specify a response file for a " &
        "compiler. The index is the name of the language for the compiler."
     );

   --  Cross_Reference.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Cross_Reference,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking gnatxref for a source of the language, if there is no" &
        " applicable attribute Switches."
     );

   --  Cross_Reference.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Cross_Reference,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name. Value is the list of switches to be " &
        "used when invoking gnatxref for the source."
     );

   --  Finder.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Finder,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking gnatfind for a source of the language, if there is no" &
        " applicable attribute Switches."
     );

   --  Finder.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Finder,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name. Value is the list of switches to be " &
        "used when invoking gnatfind for the source."
     );

   --  Gnatls.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Gnatls,
                    Attr => PRA.Switches),
      New_Item  =>
        "Taken into account only in the main project. Value is a list of " &
        "switches to be used when invoking gnatls."
     );

   --  gnatstub.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Gnatstub,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking gnatstub for a source of the language, if there is no" &
        " applicable attribute Switches."
     );

   --  gnatstub.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Gnatstub,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name. Value is the list of switches to be " &
        "used when invoking gnatstub for the source."
     );

   --  Install.Artifacts
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Artifacts),
      New_Item  =>
        "An indexed attribute to declare a set of files not part of the " &
        "sources to be installed. The array index is the directory where the" &
        " file is to be installed. If a relative directory then Prefix (see " &
        "below) is prepended. Note also that if the same file name occurs " &
        "multiple time in the attribute list, the last one will be the one " &
        "installed. If an artifact is not found a warning is displayed."
     );

   --  Install.Required_Artifacts
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Required_Artifacts),
      New_Item  =>
        "As above, but artifacts must be present or an error is reported."
     );

   --  Install.Prefix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Prefix),
      New_Item  =>
        "Value is the install destination directory. If the value is a " &
        "relative path, it is taken as relative to the global prefix " &
        "directory. That is, either the value passed to --prefix option or " &
        "the default installation prefix."
     );

   --  Install.Sources_Subdir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Sources_Subdir),
      New_Item  =>
        "Value is the sources directory or subdirectory of Prefix."
     );

   --  Install.Exec_Subdir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Exec_Subdir),
      New_Item  =>
        "Value is the executables directory or subdirectory of Prefix."
     );

   --  Install.ALI_Subdir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.ALI_Subdir),
      New_Item  =>
        "Value is ALI directory or subdirectory of Prefix."
     );

   --  Install.Lib_Subdir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Lib_Subdir),
      New_Item  =>
        "Value is library directory or subdirectory of Prefix."
     );

   --  Install.Project_Subdir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Project_Subdir),
      New_Item  =>
        "Value is the project directory or subdirectory of Prefix."
     );

   --  Install.Active
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Active),
      New_Item  =>
        "Indicates that the project is to be installed or not. " &
        "Case-insensitive value 'false' means that the project is not to be " &
        "installed, all other values mean that the project is to be " &
        "installed."
     );

   --  Install.Mode
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Mode),
      New_Item  =>
        "Value is the installation mode, it is either dev (default) or " &
        "usage."
     );

   --  Install.Install_Name
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Install_Name),
      New_Item  =>
        "Specify the name to use for recording the installation. The default" &
        " is the project name without the extension."
     );

   --  Install.Side_Debug
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Side_Debug),
      New_Item  =>
        "Indicates that the project's executable and shared libraries are to" &
        " be stripped of the debug symbols. Those debug symbols are written " &
        "into a side file named after the original file with the '.debug' " &
        "extension added. Case-insensitive value 'false' (default) disables " &
        "this feature. Set it to 'true' to activate."
     );

   --  Install.Install_Project
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Install,
                    Attr => PRA.Install_Project),
      New_Item  =>
        "Indicates that a project is to be generated and installed. The " &
        "value is either 'true' to 'false'. Default is 'true'."
     );

   --  Linker.Required_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Required_Switches),
      New_Item  =>
        "Value is a list of switches that are required when invoking the " &
        "linker to link an executable."
     );

   --  Linker.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches for the " &
        "linker when linking an executable for a main source of the " &
        "language, when there is no applicable Switches."
     );

   --  Linker.Leading_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Leading_Switches),
      New_Item  =>
        "Index is a source file name or a language name. Value is the list " &
        "of switches to be used at the beginning of the command line when " &
        "invoking the linker to build an executable for the source or for " &
        "its language."
     );

   --  Linker.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name or a language name. Value is the list " &
        "of switches to be used when invoking the linker to build an " &
        "executable for the source or for its language."
     );

   --  Linker.Trailing_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Trailing_Switches),
      New_Item  =>
        "Index is a source file name or a language name. Value is the list " &
        "of switches to be used at the end of the command line when invoking" &
        " the linker to build an executable for the source or for its " &
        "language. These switches may override the Required_Switches."
     );

   --  Linker.Linker_Options
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Linker_Options),
      New_Item  =>
        "This attribute specifies a list of additional switches to be given " &
        "to the linker when linking an executable. It is ignored when " &
        "defined in the main project and taken into account in all other " &
        "projects that are imported directly or indirectly. These switches " &
        "complement the Linker'Switches defined in the main project. This is" &
        " useful when a particular subsystem depends on an external library:" &
        " adding this dependency as a Linker_Options in the project of the " &
        "subsystem is more convenient than adding it to all the " &
        "Linker'Switches of the main projects that depend upon this " &
        "subsystem."
     );

   --  Linker. Map_File_Option
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA. Map_File_Option),
      New_Item  =>
        "Value is the switch to specify the map file name that the linker " &
        "needs to create."
     );

   --  Linker.Driver
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Driver),
      New_Item  =>
        "Value is the name of the linker executable."
     );

   --  Linker.Max_Command_Line_Length
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Max_Command_Line_Length),
      New_Item  =>
        "Value is the maximum number of character in the command line when " &
        "invoking the linker to link an executable."
     );

   --  Linker.Response_File_Format
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Response_File_Format),
      New_Item  =>
        "Indicates the kind of response file to create when the length of " &
        "the linking command line is too large. Only authorized " &
        "case-insensitive values are 'none', 'gnu', 'object_list', " &
        "'gcc_gnu', 'gcc_option_list' and 'gcc_object_list'."
     );

   --  Linker.Response_File_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Linker,
                    Attr => PRA.Response_File_Switches),
      New_Item  =>
        "Value is the list of switches to specify a response file to the " &
        "linker."
     );

   --  Metrics.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Metrics,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking gnatmetric for a source of the language, if there is " &
        "no applicable attribute Switches."
     );

   --  Metrics.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Metrics,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name. Value is the list of switches to be " &
        "used when invoking gnatmetric for the source."
     );

   --  Naming.Specification_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Specification_Suffix),
      New_Item  =>
        "Equivalent to attribute Spec_Suffix."
     );

   --  Naming.Spec_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Spec_Suffix),
      New_Item  =>
        "Index is a language name. Value is the extension of file names for " &
        "specs of the language."
     );

   --  Naming.Implementation_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Implementation_Suffix),
      New_Item  =>
        "Equivalent to attribute Body_Suffix."
     );

   --  Naming.Body_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Body_Suffix),
      New_Item  =>
        "Index is a language name. Value is the extension of file names for " &
        "bodies of the language."
     );

   --  Naming.Separate_Suffix
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Separate_Suffix),
      New_Item  =>
        "Value is the extension of file names for subunits of Ada."
     );

   --  Naming.Casing
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Casing),
      New_Item  =>
        "Indicates the casing of sources of the Ada language. Only " &
        "authorized case-insensitive values are 'lowercase', 'uppercase' and" &
        " 'mixedcase'."
     );

   --  Naming.Dot_Replacement
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Dot_Replacement),
      New_Item  =>
        "Value is the string that replace the dot of unit names in the " &
        "source file names of the Ada language."
     );

   --  Naming.Specification
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Specification),
      New_Item  =>
        "Equivalent to attribute Spec."
     );

   --  Naming.Spec
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Spec),
      New_Item  =>
        "Index is a unit name. Value is the file name of the spec of the " &
        "unit."
     );

   --  Naming.Implementation
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Implementation),
      New_Item  =>
        "Equivalent to attribute Body."
     );

   --  Naming.Specification_Exceptions
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Specification_Exceptions),
      New_Item  =>
        "Index is a language name. Value is a list of specs for the language" &
        " that do not necessarily follow the naming scheme for the language " &
        "and that may or may not be found in the source directories of the " &
        "project."
     );

   --  Naming.Implementation_Exceptions
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Naming,
                    Attr => PRA.Implementation_Exceptions),
      New_Item  =>
        "Index is a language name. Value is a list of bodies for the " &
        "language that do not necessarily follow the naming scheme for the " &
        "language and that may or may not be found in the source directories" &
        " of the project."
     );

   --  Pretty_Printer.Default_Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Pretty_Printer,
                    Attr => PRA.Default_Switches),
      New_Item  =>
        "Index is a language name. Value is a list of switches to be used " &
        "when invoking gnatpp for a source of the language, if there is no " &
        "applicable attribute Switches."
     );

   --  Pretty_Printer.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Pretty_Printer,
                    Attr => PRA.Switches),
      New_Item  =>
        "Index is a source file name. Value is the list of switches to be " &
        "used when invoking gnatpp for the source."
     );

   --  Remote.Included_Patterns
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Remote,
                    Attr => PRA.Included_Patterns),
      New_Item  =>
        "If this attribute is defined it sets the patterns to synchronized " &
        "from the master to the slaves. It is exclusive with " &
        "Excluded_Patterns, that is it is an error to define both."
     );

   --  Remote.Included_Artifact_Patterns
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Remote,
                    Attr => PRA.Included_Artifact_Patterns),
      New_Item  =>
        "If this attribute is defined it sets the patterns of compilation " &
        "artifacts to synchronized from the slaves to the build master. This" &
        " attribute replace the default hard-coded patterns."
     );

   --  Remote.Excluded_Patterns
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Remote,
                    Attr => PRA.Excluded_Patterns),
      New_Item  =>
        "Set of patterns to ignore when synchronizing sources from the build" &
        " master to the slaves. A set of predefined patterns are supported " &
        "(e.g. *.o, *.ali, *.exe, etc.), this attribute makes it possible" &
        " to add some more patterns."
     );

   --  Remote.Root_Dir
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Remote,
                    Attr => PRA.Root_Dir),
      New_Item  =>
        "Value is the root directory used by the slave machines."
     );

   --  Stack.Switches
   Pack_Attribute_Description.Insert
     (Container => Attribute_Description,
      Key       => (Pack => PRP.Stack,
                    Attr => PRA.Switches),
      New_Item  =>
        "Taken into account only in the main project. Value is the list of " &
        "switches to be used when invoking gnatstack."
     );

end GPR2.Project.Registry.Attribute.Description;
