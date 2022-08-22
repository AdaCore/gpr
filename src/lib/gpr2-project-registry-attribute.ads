--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides to GPR library the common attributes names
--  and attribute definition accessors.
--  Custom package's attributes definition can be added by custom tools.

with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Ordered_Maps;

limited with GPR2.Project.View;

package GPR2.Project.Registry.Attribute is

   type Inherit_From_Extended_Type is
     (Inherited,
      Concatenated,
      Not_Inherited);
   --  Inherited means that if the attribute can be inherited from the extended
   --  project.
   --  Concatenated is like inherited, but the final value is the concatenation
   --  of the definition in the extended and the extending project.
   --  Not_Inherited means that the value from the extended project is never
   --  inherited.

   ------------------------
   --  Attribute Indexes --
   ------------------------

   type Index_Value_Type is
     (No_Index,
      String_Index,
      Unit_Index,
      Env_Var_Name_Index,
      File_Index,
      FileGlob_Index,
      Language_Index,
      FileGlob_Or_Language_Index);
   --  No_Index: attribute does not accept indexes
   --  String_Index: case sensitive index.
   --  Unit_Index: the attribute expects a unit name as index.
   --    Case insensitive.
   --  Env_Var_Name_Index: the attribute expects an environment variable
   --    name as index. Case sensitive.
   --  FileGlob_Index: accepts a source file simple name, or a glob pattern
   --    as index. Case sensitivity depends on the host, 'others' keyword
   --    is accepted.
   --  Language_Index: accepts a language identifier as index. Case
   --    insensitive.
   --  FileGlob_Or_Language_Index: accepts both FileGlob and Language as index.
   --    To determine case sensitivity, indexes with dots, and glob-specific
   --    characters (*?[]) are considered filenames, other indexes are
   --    considered language identifiers. The 'others' keyword is accepted.

   function Is_Case_Sensitive
     (Index_Value : Value_Type;
      Index_Type  : Index_Value_Type) return Boolean;
   --  Whether a give index value should be considered case sensitive or not

   -----------------------
   --  Attribute values --
   -----------------------

   type Value_Kind is (Single, List);

   type Empty_Value_Status is (Allow, Ignore, Error);
   --  Allow  : an empty value is allowed for the attribute.
   --  Ignore : an empty value is ignored and reported as warning.
   --  Error  : an empty value is erroneous and reported as error.

   type Qualified_Name is record
      Pack : Optional_Package_Id;
      Attr : Optional_Attribute_Id;
   end record;
   --  A qualified name is an attribute name possibly prefixed with a package
   --  name. It is the only way to create a non-ambiguous reference to an
   --  attribute.

   No_Name : constant Qualified_Name := (No_Package, No_Attribute);

   function Create
     (Name : Attribute_Id;
      Pack : Optional_Package_Id := No_Package) return Qualified_Name;
   --  Returns a fully qualified name for the given attribute and package names

   function Image (Name : Qualified_Name) return String;
   --  Returns quailified name image

   type Allowed_In is array (Project_Kind) of Boolean with Pack;

   Everywhere : constant Allowed_In := (others => True);
   Nowhere    : constant Allowed_In := (others => False);

   type Default_Value_Kind is
     (D_Attribute_Reference,
      D_Value,
      D_Callback);
   --  The various kind of default value definition:
   --  Attribute_Reference: the default value takes the value of the mentioned
   --   attribute.
   --  Value: the default value is a literal
   --  Callback: the default value is determined by a callback function (too
   --   dynamic to be statically described.
   --  Empty_List: the default value is an empty list.

   type Default_Value_Callback is access
     function (View : GPR2.Project.View.Object) return Value_Type;

   Any_Index : constant Value_Type := (1 => ASCII.NUL);

   package Value_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (Value_Type, Value_Type);
   --  Used to store indexed constants as default values.

   function Exists
     (Map : Value_Map.Map; Key : Value_Type := Any_Index) return Boolean;

   function Get
     (Map : Value_Map.Map; Key : Value_Type := Any_Index) return Value_Type;
   --  Returns value for key Key in Map. If Key is not present looks first for
   --  the Any_Index key. If no value is defined for Any_Index, returns empty.

   type Default_Value (Kind : Default_Value_Kind := D_Attribute_Reference)
   is record
      case Kind is
         when D_Attribute_Reference =>
            Attr : Optional_Attribute_Id;
         when D_Value =>
            Values : Value_Map.Map;
         when D_Callback =>
            Callback : Default_Value_Callback;
      end case;
   end record;

   No_Default_Value : constant Default_Value :=
      (Kind => D_Attribute_Reference, Attr => No_Attribute);

   type Def is record
      Index_Type            : Index_Value_Type   := No_Index;
      Index_Optional        : Boolean            := False;
      Value                 : Value_Kind         := Single;
      Value_Case_Sensitive  : Boolean            := False;
      Value_Is_Set          : Boolean            := False;
      --  When the value is a list, determine if the elements should be unique
      --  or not.

      Empty_Value           : Empty_Value_Status := Allow;
      Builtin               : Boolean            := False;
      Is_Allowed_In         : Allowed_In         := (K_Abstract => True,
                                                     others     => False);
      Default               : Default_Value;
      Has_Default_In        : Allowed_In         := (others => False);
      Is_Toolchain_Config   : Boolean            := False;
      --  When set, the attribute is used during the gprconfig stage to
      --  configure toolchains (for example the attributes Target or Runtime
      --  are toolchain config attributes). Due to elaboration constraints,
      --  such attributes need to be global to the project tree, and so
      --  should not be modified after being referenced. So for example
      --  using "for Target use project'Target & "suffix"" is not allowed.

      Config_Concatenable   : Boolean := False;
      --  When True the final value for the attribute is concatenated with the
      --  value found in the Config project (if it exists) rather than
      --  overriding it.

      Inherit_From_Extended : Inherit_From_Extended_Type := Inherited;
      --  Whether an attribute is inherited from an extended project or not
      --  See Inherited_From_Extended_Type definition for available behaviours.
   end record
     with Dynamic_Predicate =>
       --  Must be usable somewhere
       Def.Is_Allowed_In /= (Project_Kind => False);

   type Default_Rules is private;

   function All_Attributes
     (Pack : Optional_Package_Id) return Containers.Attribute_Id_List;
   --  Retrieve the Ids of all defined attributes for a given package, or
   --  the top-level attributes if Pack is No_Package.

   function Exists (Q_Name : Qualified_Name) return Boolean;
   --  The qualified name comprise the package name and attribute name, both
   --  parts are separated by a dot which is mandatory even if the package
   --  name is empty (for a top level attribute).

   function Get (Q_Name : Qualified_Name) return Def
     with Pre => Exists (Q_Name);
   --  Returns the definition data for the given attribute fully qualified name

   function Get_Default_Rules
     (Pack : Optional_Package_Id) return Default_Rules;
   --  Get default rules by package name. If package name is empty get the root
   --  default rules.

   function Get_Packages_With_Default return Containers.Package_Id_List;
   --  Get the list of packages that have attributes with default values

   procedure For_Each_Default
     (Rules  : Default_Rules;
      Action : not null access procedure
        (Attribute : Attribute_Id; Definition : Def));
   --  Call Action routine for each definition with defaults in package.
   --  If Pack is empty, call Action for each root attribute with defaults.

   procedure Add
     (Name                  : Qualified_Name;
      Index_Type            : Index_Value_Type;
      Value                 : Value_Kind;
      Value_Case_Sensitive  : Boolean;
      Is_Allowed_In         : Allowed_In;
      Is_Builtin            : Boolean                    := False;
      Index_Optional        : Boolean                    := False;
      Empty_Value           : Empty_Value_Status         := Allow;
      Default               : Default_Value              := No_Default_Value;
      Has_Default_In        : Allowed_In                 := Nowhere;
      Is_Toolchain_Config   : Boolean                    := False;
      Config_Concatenable   : Boolean                    := False;
      Inherit_From_Extended : Inherit_From_Extended_Type := Inherited;
      Is_Set                : Boolean                    := False)
     with Pre => (if Is_Set
                     or else Config_Concatenable
                     or else Inherit_From_Extended = Concatenated
                    then Value = List);
   --  Add package/attribute definition in database for attribute checks.
   --  Name: qualified name of the attribute.
   --  Index_Type: the kind of index expected.
   --  Value: whether the attribute holds a single value or a list.
   --  Value_Case_Sensitive: whether the value(s) are considered case
   --    sensitive.
   --  Is_Allowed_In: list of project kind where the attribute is valid.
   --  Is_Builtin: if set, denotes an attribute that is read-only and set
   --    automatically by libgpr.
   --  Index_Optional: whether the attribute's index is mandatory or optional.
   --    Ignored if Index_Type is No_Index.
   --  Empty_Value: whether empty values are allowed for the attribute.
   --  Default: the description of the default value taken by the attribute,
   --    if any.
   --  Has_Default_In: lists of project kind where this attribute has a
   --    default value. Ignored if no default value is defined.
   --  Is_Toolchain_Config: flags the attribute as being relevant when
   --    automatically configuring the project. Such attribute can't be
   --    written after being read to prevent bootstraping issues.
   --  Config_Concatenable: if set, the final values hold by the attribute
   --    will be the concatenation of the user-defined attribute and the
   --    configuration project value.
   --  Inherit_From_Extended: Whether the attribute is inherited from extended
   --    projects, its values concatenated or not inherited. Only relevant
   --    for toplevel attributes.
   --  Is_Set: if set, the attribute values is considered a set, so won't
   --    hold duplicated values.

   procedure Add_Alias
     (Name     : Qualified_Name;
      Alias_Of : Qualified_Name);

   function Has_Alias (Name : Qualified_Name) return Boolean;

   function Alias (Name : Qualified_Name) return Qualified_Name
     with Post => (if Has_Alias (Name) then Alias'Result /= No_Name
                   else Alias'Result = No_Name);

   --  Some common attribute names

   Additional_Patterns         : constant Attribute_Id :=
                                   +"additional_patterns";
   ALI_Subdir                  : constant Attribute_Id := +"ali_subdir";
   Active                      : constant Attribute_Id := +"active";
   Archive_Builder             : constant Attribute_Id := +"archive_builder";
   Archive_Builder_Append_Option : constant Attribute_Id :=
                                     +"archive_builder_append_option";
   Archive_Indexer             : constant Attribute_Id := +"archive_indexer";
   Archive_Suffix              : constant Attribute_Id := +"archive_suffix";
   Artifacts                   : constant Attribute_Id := +"artifacts";
   Artifacts_In_Exec_Dir       : constant Attribute_Id :=
                                   +"artifacts_in_exec_dir";
   Artifacts_In_Object_Dir     : constant Attribute_Id :=
                                   +"artifacts_in_object_dir";
   Bindfile_Option_Substitution : constant Attribute_Id :=
                                   +"bindfile_option_substitution";
   Body_N                      : constant Attribute_Id := +"body";
   Body_Suffix                 : constant Attribute_Id := +"body_suffix";
   Casing                      : constant Attribute_Id := +"casing";
   Canonical_Target            : constant Attribute_Id := +"canonical_target";
   Config_Body_File_Name       : constant Attribute_Id :=
                                   +"config_body_file_name";
   Config_Body_File_Name_Index : constant Attribute_Id :=
                                   +"config_body_file_name_index";
   Config_Body_File_Name_Pattern : constant Attribute_Id :=
                                     +"config_body_file_name_pattern";
   Config_File_Switches        : constant Attribute_Id :=
                                   +"config_file_switches";
   Config_File_Unique          : constant Attribute_Id :=
                                   +"config_file_unique";
   Config_Spec_File_Name       : constant Attribute_Id :=
                                   +"config_spec_file_name";
   Config_Spec_File_Name_Index : constant Attribute_Id :=
                                   +"config_spec_file_name_index";
   Config_Spec_File_Name_Pattern : constant Attribute_Id :=
                                     +"config_spec_file_name_pattern";
   Create_Missing_Dirs         : constant Attribute_Id :=
                                   +"create_missing_dirs";
   Database_Directory          : constant Attribute_Id :=
                                   +"database_directory";
   Default_Language            : constant Attribute_Id := +"default_language";
   Default_Switches            : constant Attribute_Id := +"default_switches";
   Dependency_Driver           : constant Attribute_Id := +"dependency_driver";
   Dependency_Kind             : constant Attribute_Id := +"dependency_kind";
   Dependency_Switches         : constant Attribute_Id :=
                                   +"dependency_switches";
   Dot_Replacement             : constant Attribute_Id := +"dot_replacement";
   Driver                      : constant Attribute_Id := +"driver";
   Excluded_Patterns           : constant Attribute_Id := +"excluded_patterns";
   Excluded_Source_Files       : constant Attribute_Id :=
                                   +"excluded_source_files";
   Excluded_Source_Dirs        : constant Attribute_Id :=
                                   +"excluded_source_dirs";
   Excluded_Source_List_File   : constant Attribute_Id :=
                                   +"excluded_source_list_file";
   Exec_Dir                    : constant Attribute_Id := +"exec_dir";
   Exec_Subdir                 : constant Attribute_Id := +"exec_subdir";
   Executable                  : constant Attribute_Id := +"executable";
   Executable_Suffix           : constant Attribute_Id := +"executable_suffix";
   Export_File_Format          : constant Attribute_Id :=
                                   +"export_file_format";
   Export_File_Switch          : constant Attribute_Id :=
                                   +"export_file_switch";
   External                    : constant Attribute_Id := +"external";
   Externally_Built            : constant Attribute_Id := +"externally_built";
   Global_Compilation_Switches : constant Attribute_Id :=
                                   +"global_compilation_switches";
   Global_Config_File          : constant Attribute_Id :=
                                   +"global_config_file";
   Global_Configuration_Pragmas : constant Attribute_Id :=
                                    +"global_configuration_pragmas";
   Ignore_Source_Sub_Dirs      : constant Attribute_Id :=
                                   +"ignore_source_sub_dirs";
   Implementation              : constant Attribute_Id := +"implementation";
   Implementation_Exceptions   : constant Attribute_Id :=
                                   +"implementation_exceptions";
   Implementation_Suffix       : constant Attribute_Id :=
                                   +"implementation_suffix";
   Included_Artifacts_Patterns : constant Attribute_Id :=
                                   +"included_artifacts_patterns";
   Included_Patterns           : constant Attribute_Id := +"included_patterns";
   Include_Switches_Via_Spec   : constant Attribute_Id :=
                                   +"include_switches_via_spec";
   Include_Path                : constant Attribute_Id := +"include_path";
   Include_Path_File           : constant Attribute_Id := +"include_path_file";
   Include_Switches            : constant Attribute_Id := +"include_switches";
   Included_Artifact_Patterns  : constant Attribute_Id :=
                                   +"included_artifact_patterns";
   Inherit_Source_Path         : constant Attribute_Id :=
                                   +"inherit_source_path";
   Install_Name                : constant Attribute_Id := +"install_name";
   Install_Project             : constant Attribute_Id := +"install_project";
   Interfaces                  : constant Attribute_Id := +"interfaces";
   Languages                   : constant Attribute_Id := +"languages";
   Language_Kind               : constant Attribute_Id := +"language_kind";
   Leading_Library_Options     : constant Attribute_Id :=
                                   +"leading_library_options";
   Leading_Required_Switches   : constant Attribute_Id :=
                                   +"leading_required_switches";
   Leading_Switches            : constant Attribute_Id := +"leading_switches";
   Lib_Subdir                  : constant Attribute_Id := +"lib_subdir";
   Library_Ali_Dir             : constant Attribute_Id := +"library_ali_dir";
   Library_Auto_Init           : constant Attribute_Id := +"library_auto_init";
   Library_Auto_Init_Supported : constant Attribute_Id :=
                                   +"library_auto_init_supported";
   Library_Dir                 : constant Attribute_Id := +"library_dir";
   Library_Builder             : constant Attribute_Id := +"library_builder";
   Library_Encapsulated_Options : constant Attribute_Id :=
                                    +"library_encapsulated_options";
   Library_Encapsulated_Supported : constant Attribute_Id :=
                                      +"library_encapsulated_supported";
   Library_Gcc                 : constant Attribute_Id := +"library_gcc";
   Library_Install_Name_Option : constant Attribute_Id :=
                                   +"library_install_name_option";
   Library_Interface           : constant Attribute_Id := +"library_interface";
   Library_Kind                : constant Attribute_Id := +"library_kind";
   Library_Major_Minor_Id_Supported : constant Attribute_Id :=
                                        +"library_major_minor_id_supported";
   Library_Name                : constant Attribute_Id := +"library_name";
   Library_Options             : constant Attribute_Id := +"library_options";
   Library_Partial_Linker      : constant Attribute_Id :=
                                   +"library_partial_linker";
   Library_Reference_Symbol_File : constant Attribute_Id :=
                                     +"library_reference_symbol_file";
   Library_Rpath_Options       : constant Attribute_Id :=
                                   +"library_rpath_options";
   Library_Src_Dir             : constant Attribute_Id := +"library_src_dir";
   Library_Standalone          : constant Attribute_Id :=
                                   +"library_standalone";
   Library_Support             : constant Attribute_Id := +"library_support";
   Library_Symbol_File         : constant Attribute_Id :=
                                   +"library_symbol_file";
   Library_Symbol_Policy       : constant Attribute_Id :=
                                   +"library_symbol_policy";
   Library_Version             : constant Attribute_Id := +"library_version";
   Library_Version_Switches    : constant Attribute_Id :=
                                   +"library_version_switches";
   Link_Lib_Subdir             : constant Attribute_Id := +"link_lib_subdir";
   Linker_Options              : constant Attribute_Id := +"linker_options";
   Locally_Removed_Files       : constant Attribute_Id :=
                                   +"locally_removed_files";
   Local_Config_File           : constant Attribute_Id := +"local_config_file";
   Local_Configuration_Pragmas : constant Attribute_Id :=
                                   +"local_configuration_pragmas";
   Main                        : constant Attribute_Id := +"main";
   Map_File_Option             : constant Attribute_Id := +"map_file_option";
   Mapping_Body_Suffix         : constant Attribute_Id :=
                                   +"mapping_body_suffix";
   Mapping_File_Switches       : constant Attribute_Id :=
                                   +"mapping_file_switches";
   Mapping_Spec_Suffix         : constant Attribute_Id :=
                                   +"mapping_spec_suffix";
   Max_Command_Line_Length     : constant Attribute_Id :=
                                   +"max_command_line_length";
   Message_Patterns            : constant Attribute_Id := +"message_patterns";
   Mode                        : constant Attribute_Id := +"mode";
   Multi_Unit_Object_Separator : constant Attribute_Id :=
                                   +"multi_unit_object_separator";
   Multi_Unit_Switches         : constant Attribute_Id :=
                                   +"multi_unit_switches";
   Name                        : constant Attribute_Id := +"name";
   Object_Dir                  : constant Attribute_Id := +"object_dir";
   Object_File_Suffix          : constant Attribute_Id :=
                                   +"object_file_suffix";
   Object_Generated            : constant Attribute_Id := +"object_generated";
   Objects_Linked              : constant Attribute_Id := +"objects_linked";
   Object_Lister               : constant Attribute_Id := +"object_lister";
   Object_Lister_Matcher       : constant Attribute_Id :=
                                   +"object_lister_matcher";
   Object_Artifact_Extensions  : constant Attribute_Id :=
                                   +"object_artifact_extensions";
   Object_File_Switches        : constant Attribute_Id :=
                                   +"object_file_switches";
   Object_Path_Switches        : constant Attribute_Id :=
                                   +"object_path_switches";
   Objects_Path                : constant Attribute_Id := +"objects_path";
   Objects_Path_File           : constant Attribute_Id := +"objects_path_file";
   Origin_Project              : constant Attribute_Id := +"origin_project";
   Only_Dirs_With_Sources      : constant Attribute_Id :=
                                   +"only_dirs_with_sources";
   Output_Directory            : constant Attribute_Id := +"output_directory";
   Path_Syntax                 : constant Attribute_Id := +"path_syntax";
   Pic_Option                  : constant Attribute_Id := +"pic_option";
   Prefix                      : constant Attribute_Id := +"prefix";
   Project_Dir                 : constant Attribute_Id := +"project_dir";
   Project_Files               : constant Attribute_Id := +"project_files";
   Project_Path                : constant Attribute_Id := +"project_path";
   Project_Subdir              : constant Attribute_Id := +"project_subdir";
   Required_Artifacts          : constant Attribute_Id :=
                                   +"required_artifacts";
   Required_Switches           : constant Attribute_Id := +"required_switches";
   Required_Toolchain_Version  : constant Attribute_Id :=
                                   +"required_toolchain_version";
   Response_File_Format        : constant Attribute_Id :=
                                   +"response_file_format";
   Response_File_Switches      : constant Attribute_Id :=
                                   +"response_file_switches";
   Root_Dir                    : constant Attribute_Id := +"root_dir";
   Roots                       : constant Attribute_Id := +"roots";
   Run_Path_Option             : constant Attribute_Id := +"run_path_option";
   Run_Path_Origin             : constant Attribute_Id := +"run_path_origin";
   Runtime                     : constant Attribute_Id := +"runtime";
   Runtime_Dir                 : constant Attribute_Id := +"runtime_dir";
   Runtime_Library_Dir         : constant Attribute_Id :=
                                   +"runtime_library_dir";
   Runtime_Source_Dir          : constant Attribute_Id :=
                                   +"runtime_source_dir";
   Runtime_Source_Dirs         : constant Attribute_Id :=
                                   +"runtime_source_dirs";
   Separate_Suffix             : constant Attribute_Id := +"separate_suffix";
   Separate_Run_Path_Options   : constant Attribute_Id :=
                                   +"separate_run_path_options";
   Shared_Library_Minimum_Switches : constant Attribute_Id :=
                                       +"shared_library_minimum_switches";
   Shared_Library_Prefix       : constant Attribute_Id :=
                                   +"shared_library_prefix";
   Shared_Library_Suffix       : constant Attribute_Id :=
                                   +"shared_library_suffix";
   Side_Debug                  : constant Attribute_Id := +"side_debug";
   Source_Artifact_Extensions  : constant Attribute_Id :=
                                   +"source_artifact_extensions";
   Source_Dirs                 : constant Attribute_Id := +"source_dirs";
   Source_Files                : constant Attribute_Id := +"source_files";
   Source_File_Switches        : constant Attribute_Id :=
                                   +"source_file_switches";
   Source_List_File            : constant Attribute_Id := +"source_list_file";
   Sources_Subdir              : constant Attribute_Id := +"sources_subdir";
   Spec                        : constant Attribute_Id := +"spec";
   Spec_Suffix                 : constant Attribute_Id := +"spec_suffix";
   Specification               : constant Attribute_Id := +"specification";
   Specification_Exceptions    : constant Attribute_Id :=
                                   +"specification_exceptions";
   Specification_Suffix        : constant Attribute_Id :=
                                   +"specification_suffix";
   Symbolic_Link_Supported     : constant Attribute_Id :=
                                   +"symbolic_link_supported";
   Switches                    : constant Attribute_Id := +"switches";
   Target                      : constant Attribute_Id := +"target";
   Toolchain_Version           : constant Attribute_Id := +"toolchain_version";
   Toolchain_Name              : constant Attribute_Id := +"toolchain_name";
   Toolchain_Description       : constant Attribute_Id :=
                                   +"toolchain_description";
   Toolchain_Path              : constant Attribute_Id := +"toolchain_path";
   Trailing_Required_Switches  : constant Attribute_Id :=
                                   +"trailing_required_switches";
   Trailing_Switches           : constant Attribute_Id := +"trailing_switches";
   Warning_Message             : constant Attribute_Id := +"warning_message";

private

   function Image (Name : Qualified_Name) return String is
     (if Name.Pack = No_Package
      then Image (Name.Attr)
      else Image (Name.Pack) & "'" & Image (Name.Attr));

   function "<" (Left, Right : Qualified_Name) return Boolean is
     (if Left.Pack /= Right.Pack then Left.Pack < Right.Pack
      else Left.Attr < Right.Attr);

   package Attribute_Definitions is new Ada.Containers.Ordered_Maps
     (Qualified_Name, Def);

   type Def_Access is access constant Def;

   package Default_References is new Ada.Containers.Ordered_Maps
     (Optional_Attribute_Id, Def_Access);
   --  To keep references only to attribute definitions with default rules

   type Default_Rules is access constant Default_References.Map;

   package Attribute_Aliases is new Ada.Containers.Ordered_Maps
     (Qualified_Name, Qualified_Name);
   --  Keeps track of Attributes that are aliased

end GPR2.Project.Registry.Attribute;
