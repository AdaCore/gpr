------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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

--  This package provides to GPR library the common attributes names
--  and attribute definition accessors.
--  Custom package's attributes definition can be added by custom tools.

with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Containers;

package GPR2.Project.Registry.Attribute is

   type Index_Kind is (No, Yes, Optional);

   subtype Index_Allowed is Index_Kind range Yes .. Optional;

   type Value_Kind is (Single, List);

   type Empty_Value_Status is (Allow, Ignore, Error);
   --  Allow  : an empty value is allowed for the attribute.
   --  Ignore : an empty value is ignored and reported as warning.
   --  Error  : an empty value is erroneous and reported as error.

   type Qualified_Name (<>) is private;
   --  A qualified name is an attribute name possibly prefixed with a package
   --  name. It is the only way to create a non-ambiguous reference to an
   --  attribute.

   function Create
     (Name : Name_Type;
      Pack : Optional_Name_Type := No_Name) return Qualified_Name;
   --  Returns a fully qualified name for the given attribute and package names

   function Image (Name : Qualified_Name) return String;
   --  Returns quailified name image

   type Allowed_In is array (Project_Kind) of Boolean with Pack;

   Everywhere : constant Allowed_In := (others => True);
   Nowhere    : constant Allowed_In := (others => False);

   package VSR renames Containers.Name_Value_Map_Package;

   type Def is record
      Index                : Index_Kind         := Optional;
      Others_Allowed       : Boolean            := False;
      Index_Case_Sensitive : Boolean            := False;
      Value                : Value_Kind         := Single;
      Value_Case_Sensitive : Boolean            := False;
      Empty_Value          : Empty_Value_Status := Allow;
      Read_Only            : Boolean            := False;
      Is_Allowed_In        : Allowed_In         := (K_Abstract => True,
                                                    others     => False);
      Default              : VSR.Map;
      Default_Is_Reference : Boolean            := False;
      Has_Default_In       : Allowed_In         := (others => False);
      Is_Toolchain_Config  : Boolean            := False;
      --  When set, the attribute is used to during the gprconfig stage to
      --  configure toolchains (for example the attributes Target or Runtime
      --  are toolchain config attributes). Due to elaboration constraints,
      --  such attributes need to be global to the project tree, and so
      --  should not be modified after being referenced. So for example
      --  using "for Target use project'Target & "suffix"" is not allowed.
   end record
     with Dynamic_Predicate =>
       --  Either Index is allowed or the other parts are default
       (Def.Index in Index_Allowed
        or else (not Def.Others_Allowed
                 and then not Def.Index_Case_Sensitive))
     and then
       --  Must be usable somewhere
       Def.Is_Allowed_In /= (Project_Kind => False);

   type Default_Rules is private;

   function Exists (Q_Name : Qualified_Name) return Boolean;
   --  The qualified name comprise the package name and attribute name, both
   --  parts are separated by a dot which is mandatory even if the package
   --  name is empty (for a top level attribute).

   function Get (Q_Name : Qualified_Name) return Def
     with Pre => Exists (Q_Name);
   --  Returns the definition data for the given attribute fully qualified name

   function Get_Default_Rules (Pack : Optional_Name_Type) return Default_Rules;
   --  Get default rules by package name. If package name is empty get the root
   --  default rules.

   procedure For_Each_Default
     (Rules  : Default_Rules;
      Action : not null access procedure
        (Attribute : Name_Type; Definition : Def));
   --  Call Action routine for each definition with defaults in package.
   --  If Pack is empty, call Action for each root attribute with defaults.

   procedure Add
     (Name                 : Qualified_Name;
      Index                : Index_Kind;
      Others_Allowed       : Boolean;
      Index_Case_Sensitive : Boolean;
      Value                : Value_Kind;
      Value_Case_Sensitive : Boolean;
      Read_Only            : Boolean;
      Is_Allowed_In        : Allowed_In;
      Empty_Value          : Empty_Value_Status := Allow;
      Default              : VSR.Map            := VSR.Empty_Map;
      Default_Is_Reference : Boolean            := False;
      Has_Default_In       : Allowed_In         := Nowhere;
      Is_Toolchain_Config  : Boolean            := False);
   --  add package/attribute definition in database for attribute checks

   --  Some common attribute names

   Additional_Patterns         : constant Name_Type := "additional_patterns";
   ALI_Subdir                  : constant Name_Type := "ali_subdir";
   Active                      : constant Name_Type := "active";
   Archive_Suffix              : constant Name_Type := "archive_suffix";
   Archive_Builder             : constant Name_Type := "archive_builder";
   Archive_Builder_Append_Option : constant Name_Type :=
                                     "archive_builder_append_option";
   Archive_Indexer             : constant Name_Type := "archive_indexer";
   Artifacts                   : constant Name_Type := "artifacts";
   Artifacts_In_Exec_Dir       : constant Name_Type := "artifacts_in_exec_dir";
   Artifacts_In_Object_Dir     : constant Name_Type :=
                                   "artifacts_in_object_dir";
   Body_N                      : constant Name_Type := "body";
   Body_Suffix                 : constant Name_Type := "body_suffix";
   Casing                      : constant Name_Type := "casing";
   Canonical_Target            : constant Name_Type := "canonical_target";
   Communication_Protocol      : constant Name_Type :=
                                   "communication_protocol";
   Compiler_Command            : constant Name_Type := "compiler_command";
   Config_Body_File_Name       : constant Name_Type := "config_body_file_name";
   Config_Body_File_Name_Index : constant Name_Type :=
                                   "config_body_file_name_index";
   Config_Body_File_Name_Pattern : constant Name_Type :=
                                     "config_body_file_name_pattern";
   Config_File_Switches        : constant Name_Type := "config_file_switches";
   Config_File_Unique          : constant Name_Type := "config_file_unique";
   Config_Spec_File_Name       : constant Name_Type := "config_spec_file_name";
   Config_Spec_File_Name_Index : constant Name_Type :=
                                   "config_spec_file_name_index";
   Config_Spec_File_Name_Pattern : constant Name_Type :=
                                     "config_spec_file_name_pattern";
   Create_Missing_Dirs         : constant Name_Type := "create_missing_dirs";
   Database_Directory          : constant Name_Type := "database_directory";
   Debugger_Command            : constant Name_Type := "debugger_command";
   Default_Language            : constant Name_Type := "default_language";
   Default_Switches            : constant Name_Type := "default_switches";
   Dependency_Driver           : constant Name_Type := "dependency_driver";
   Dependency_Kind             : constant Name_Type := "dependency_kind";
   Dependency_Switches         : constant Name_Type := "dependency_switches";
   Documentation_Dir           : constant Name_Type := "documentation_dir";
   Dot_Replacement             : constant Name_Type := "dot_replacement";
   Driver                      : constant Name_Type := "driver";
   Excluded_Patterns           : constant Name_Type := "excluded_patterns";
   Excluded_Source_Files       : constant Name_Type := "excluded_source_files";
   Excluded_Source_Dirs        : constant Name_Type := "excluded_source_dirs";
   Excluded_Source_List_File   : constant Name_Type :=
                                   "excluded_source_list_file";
   Exec_Dir                    : constant Name_Type := "exec_dir";
   Exec_Subdir                 : constant Name_Type := "exec_subdir";
   Executable                  : constant Name_Type := "executable";
   Executable_Suffix           : constant Name_Type := "executable_suffix";
   Export_File_Format          : constant Name_Type := "export_file_format";
   Export_File_Switch          : constant Name_Type := "export_file_switch";
   External                    : constant Name_Type := "external";
   Externally_Built            : constant Name_Type := "externally_built";
   Global_Compilation_Switches : constant Name_Type :=
                                   "global_compilation_switches";
   Global_Config_File          : constant Name_Type := "global_config_file";
   Global_Configuration_Pragmas : constant Name_Type :=
                                    "global_configuration_pragmas";
   Gnatlist                    : constant Name_Type := "gnatlist";
   Ignore_Source_Sub_Dirs      : constant Name_Type :=
                                   "ignore_source_sub_dirs";
   Implementation              : constant Name_Type := "implementation";
   Implementation_Exceptions   : constant Name_Type :=
                                   "implementation_exceptions";
   Implementation_Suffix       : constant Name_Type := "implementation_suffix";
   Included_Artifacts_Patterns : constant Name_Type :=
                                   "included_artifacts_patterns";
   Included_Patterns           : constant Name_Type := "included_patterns";
   Include_Switches_Via_Spec   : constant Name_Type :=
                                   "include_switches_via_spec";
   Include_Path                : constant Name_Type := "include_path";
   Include_Path_File           : constant Name_Type := "include_path_file";
   Include_Switches            : constant Name_Type := "include_switches";
   Included_Artifact_Patterns  : constant Name_Type :=
                                   "included_artifact_patterns";
   Inherit_Source_Path         : constant Name_Type := "inherit_source_path";
   Install_Name                : constant Name_Type := "install_name";
   Install_Project             : constant Name_Type := "install_project";
   Interfaces                  : constant Name_Type := "interfaces";
   Languages                   : constant Name_Type := "languages";
   Language_Kind               : constant Name_Type := "language_kind";
   Leading_Library_Options     : constant Name_Type :=
                                   "leading_library_options";
   Leading_Required_Switches   : constant Name_Type :=
                                   "leading_required_switches";
   Leading_Switches            : constant Name_Type := "leading_switches";
   Library_Encapsulated_Options : constant Name_Type :=
                                    "library_encapsulated_options";
   Library_Encapsulated_Supported : constant Name_Type :=
                                      "library_encapsulated_supported";
   Library_Auto_Init           : constant Name_Type := "library_auto_init";
   Lib_Subdir                  : constant Name_Type := "lib_subdir";
   Library_Ali_Dir             : constant Name_Type := "library_ali_dir";
   Library_Dir                 : constant Name_Type := "library_dir";
   Library_Builder             : constant Name_Type := "library_builder";
   Library_Interface           : constant Name_Type := "library_interface";
   Library_Kind                : constant Name_Type := "library_kind";
   Library_Name                : constant Name_Type := "library_name";
   Library_Options             : constant Name_Type := "library_options";
   Library_Standalone          : constant Name_Type := "library_standalone";
   Library_Version             : constant Name_Type := "library_version";
   Library_Rpath_Options       : constant Name_Type := "library_rpath_options";
   Library_Src_Dir             : constant Name_Type := "library_src_dir";
   Library_Gcc                 : constant Name_Type := "library_gcc";
   Library_Partial_Linker      : constant Name_Type :=
                                   "library_partial_linker";
   Library_Support             : constant Name_Type := "library_support";
   Library_Symbol_File         : constant Name_Type := "library_symbol_file";
   Library_Symbol_Policy       : constant Name_Type := "library_symbol_policy";
   Library_Reference_Symbol_File : constant Name_Type :=
                                     "library_reference_symbol_file";
   Library_Major_Minor_Id_Supported : constant Name_Type :=
                                        "library_major_minor_id_supported";
   Library_Auto_Init_Supported : constant Name_Type :=
                                   "library_auto_init_supported";
   Library_Version_Switches    : constant Name_Type :=
                                   "library_version_switches";
   Library_Install_Name_Option : constant Name_Type :=
                                   "library_install_name_option";
   Link_Lib_Subdir             : constant Name_Type := "link_lib_subdir";
   Linker_Options              : constant Name_Type := "linker_options";
   Locally_Removed_Files       : constant Name_Type := "locally_removed_files";
   Local_Config_File           : constant Name_Type := "local_config_file";
   Local_Configuration_Pragmas : constant Name_Type :=
                                   "local_configuration_pragmas";
   Main                        : constant Name_Type := "main";
   Map_File_Option             : constant Name_Type := "map_file_option";
   Mapping_Body_Suffix         : constant Name_Type := "mapping_body_suffix";
   Mapping_File_Switches       : constant Name_Type := "mapping_file_switches";
   Mapping_Spec_Suffix         : constant Name_Type := "mapping_spec_suffix";
   Max_Command_Line_Length     : constant Name_Type :=
                                   "max_command_line_length";
   Message_Patterns            : constant Name_Type := "message_patterns";
   Mode                        : constant Name_Type := "mode";
   Multi_Unit_Object_Separator : constant Name_Type :=
                                   "multi_unit_object_separator";
   Multi_Unit_Switches         : constant Name_Type := "multi_unit_switches";
   Name                        : constant Name_Type := "name";
   Object_Dir                  : constant Name_Type := "object_dir";
   Object_File_Suffix          : constant Name_Type := "object_file_suffix";
   Object_Generated            : constant Name_Type := "object_generated";
   Objects_Linked              : constant Name_Type := "objects_linked";
   Object_Lister               : constant Name_Type := "object_lister";
   Object_Lister_Matcher       : constant Name_Type := "object_lister_matcher";
   Object_Artifact_Extensions  : constant Name_Type :=
                                   "object_artifact_extensions";
   Object_File_Switches        : constant Name_Type := "object_file_switches";
   Object_Path_Switches        : constant Name_Type := "object_path_switches";
   Objects_Path                : constant Name_Type := "objects_path";
   Objects_Path_File           : constant Name_Type := "objects_path_file";
   Origin_Project              : constant Name_Type := "origin_project";
   Only_Dirs_With_Sources      : constant Name_Type :=
                                   "only_dirs_with_sources";
   Output_Directory            : constant Name_Type := "output_directory";
   Path_Syntax                 : constant Name_Type := "path_syntax";
   Pic_Option                  : constant Name_Type := "pic_option";
   Prefix                      : constant Name_Type := "prefix";
   Program_Host                : constant Name_Type := "program_host";
   Project_Dir                 : constant Name_Type := "project_dir";
   Project_Files               : constant Name_Type := "project_files";
   Project_Path                : constant Name_Type := "project_path";
   Project_Subdir              : constant Name_Type := "project_subdir";
   Required_Artifacts          : constant Name_Type := "required_artifacts";
   Remote_Host                 : constant Name_Type := "remote_host";
   Required_Switches           : constant Name_Type := "required_switches";
   Required_Toolchain_Version  : constant Name_Type :=
                                   "required_toolchain_version";
   Response_File_Format        : constant Name_Type := "response_file_format";
   Response_File_Switches      : constant Name_Type :=
                                   "response_file_switches";
   Root_Dir                    : constant Name_Type := "root_dir";
   Roots                       : constant Name_Type := "roots";
   Run_Path_Option             : constant Name_Type := "run_path_option";
   Run_Path_Origin             : constant Name_Type := "run_path_origin";
   Runtime                     : constant Name_Type := "runtime";
   Runtime_Dir                 : constant Name_Type := "runtime_dir";
   Runtime_Library_Dir         : constant Name_Type := "runtime_library_dir";
   Runtime_Source_Dir          : constant Name_Type := "runtime_source_dir";
   Runtime_Source_Dirs         : constant Name_Type := "runtime_source_dirs";
   Separate_Suffix             : constant Name_Type := "separate_suffix";
   Separate_Run_Path_Options   : constant Name_Type :=
                                   "separate_run_path_options";
   Shared_Library_Prefix       : constant Name_Type := "shared_library_prefix";
   Shared_Library_Suffix       : constant Name_Type := "shared_library_suffix";
   Shared_Library_Minimum_Switches : constant Name_Type :=
                                       "shared_library_minimum_switches";
   Side_Debug                  : constant Name_Type := "side_debug";
   Source_Artifact_Extensions  : constant Name_Type :=
                                   "source_artifact_extensions";
   Source_Dirs                 : constant Name_Type := "source_dirs";
   Source_Files                : constant Name_Type := "source_files";
   Source_File_Switches        : constant Name_Type := "source_file_switches";
   Source_List_File            : constant Name_Type := "source_list_file";
   Sources_Subdir              : constant Name_Type := "sources_subdir";
   Spec                        : constant Name_Type := "spec";
   Spec_Suffix                 : constant Name_Type := "spec_suffix";
   Specification               : constant Name_Type := "specification";
   Specification_Exceptions    : constant Name_Type :=
                                   "specification_exceptions";
   Specification_Suffix        : constant Name_Type := "specification_suffix";
   Symbolic_Link_Supported     : constant Name_Type :=
                                   "symbolic_link_supported";
   Switches                    : constant Name_Type := "switches";
   Target                      : constant Name_Type := "target";
   Toolchain_Version           : constant Name_Type := "toolchain_version";
   Toolchain_Name              : constant Name_Type := "toolchain_name";
   Toolchain_Description       : constant Name_Type := "toolchain_description";
   Toolchain_Path              : constant Name_Type := "toolchain_path";
   Trailing_Required_Switches  : constant Name_Type :=
                                   "trailing_required_switches";
   Trailing_Switches           : constant Name_Type := "trailing_switches";
   Vcs_File_Check              : constant Name_Type := "vcs_file_check";
   Vcs_Kind                    : constant Name_Type := "vcs_kind";
   Vcs_Log_Check               : constant Name_Type := "vcs_log_check";
   Warning_Message             : constant Name_Type := "warning_message";

private

   type Qualified_Name is new Name_Type;

   function Image (Name : Qualified_Name) return String is
     (String (Name));

   package Attribute_Definitions is new Ada.Containers.Indefinite_Ordered_Maps
     (Qualified_Name, Def);

   type Def_Access is access constant Def;

   package Default_References is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Def_Access);
   --  To keep references only to attribute definitions with default rules

   type Default_Rules is access constant Default_References.Map;

end GPR2.Project.Registry.Attribute;
