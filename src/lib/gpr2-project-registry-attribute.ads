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
with GPR2.Project.Registry.Pack;

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
     (Pack : Package_Id) return Containers.Attribute_Id_List;
   --  Retrieve the Ids of all defined attributes for a given package, or
   --  the top-level attributes if Pack is Project_Level_Scope.

   function Exists (Q_Name : Q_Attribute_Id) return Boolean;
   --  The qualified name comprise the package name and attribute name, both
   --  parts are separated by a dot which is mandatory even if the package
   --  name is empty (for a top level attribute).

   function Get (Q_Name : Q_Attribute_Id) return Def
     with Pre => Exists (Q_Name);
   --  Returns the definition data for the given attribute fully qualified name

   function Get_Default_Rules (Pack : Package_Id) return Default_Rules;
   --  Get default rules by package name. If package name is empty get the root
   --  default rules.

   function Get_Packages_With_Default return Containers.Package_Id_List;
   --  Get the list of packages that have attributes with default values

   procedure For_Each_Default
     (Rules  : Default_Rules;
      Action : not null access procedure
        (Attribute : Q_Attribute_Id; Definition : Def));
   --  Call Action routine for each definition with defaults in package.
   --  If Pack is empty, call Action for each root attribute with defaults.

   procedure Add
     (Name                  : Q_Attribute_Id;
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
     with Pre => GPR2.Project.Registry.Pack.Exists (Name.Pack)
                 and then
                 (if Is_Set
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
     (Name     : Q_Attribute_Id;
      Alias_Of : Q_Attribute_Id);

   function Has_Alias (Name : Q_Attribute_Id) return Boolean;

   function Alias (Name : Q_Attribute_Id) return Q_Optional_Attribute_Id
     with Post => (if Has_Alias (Name)
                   then Alias'Result /= No_Attribute_Id
                   else Alias'Result = No_Attribute_Id);

   --  Project_Level_Scope attribute qualified names

   Archive_Builder                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"archive_builder");
   Archive_Builder_Append_Option    : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"archive_builder_append_option");
   Archive_Indexer                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"archive_indexer");
   Archive_Suffix                   : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"archive_suffix");
   Canonical_Target                 : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"canonical_target");
   Create_Missing_Dirs              : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"create_missing_dirs");
   Default_Language                 : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"default_language");
   Excluded_Source_Files            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"excluded_source_files");
   Excluded_Source_Dirs             : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"excluded_source_dirs");
   Excluded_Source_List_File        : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"excluded_source_list_file");
   Exec_Dir                         : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"exec_dir");
   External                         : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"external");
   Externally_Built                 : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"externally_built");
   Ignore_Source_Sub_Dirs           : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"ignore_source_sub_dirs");
   Include_Switches_Via_Spec        : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"include_switches_via_spec");
   Inherit_Source_Path              : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"inherit_source_path");
   Interfaces                       : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"interfaces");
   Languages                        : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"languages");
   Leading_Library_Options          : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"leading_library_options");
   Library_Ali_Dir                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_ali_dir");
   Library_Auto_Init                : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_auto_init");
   Library_Auto_Init_Supported      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_auto_init_supported");
   Library_Dir                      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"library_dir");
   Library_Builder                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_builder");
   Library_Encapsulated_Options     : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_encapsulated_options");
   Library_Encapsulated_Supported   : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_encapsulated_supported");
   Library_Gcc                      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"library_gcc");
   Library_Install_Name_Option      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_install_name_option");
   Library_Interface                : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_interface");
   Library_Kind                     : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"library_kind");
   Library_Major_Minor_Id_Supported : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_major_minor_id_supported");
   Library_Name                     : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"library_name");
   Library_Options                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_options");
   Library_Partial_Linker           : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_partial_linker");
   Library_Reference_Symbol_File    : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_reference_symbol_file");
   Library_Rpath_Options            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_rpath_options");
   Library_Src_Dir                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_src_dir");
   Library_Standalone               : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_standalone");
   Library_Support                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_support");
   Library_Symbol_File              : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_symbol_file");
   Library_Symbol_Policy            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_symbol_policy");
   Library_Version                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_version");
   Library_Version_Switches         : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"library_version_switches");
   Locally_Removed_Files            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"locally_removed_files");
   Main                             : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"main");
   Name                             : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"name");
   Object_Dir                       : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"object_dir");
   Object_Generated                 : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"object_generated");
   Objects_Linked                   : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"objects_linked");
   Object_Lister                    : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"object_lister");
   Object_Lister_Matcher            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"object_lister_matcher");
   Origin_Project                   : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"origin_project");
   Only_Dirs_With_Sources           : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"only_dirs_with_sources");
   Project_Dir                      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"project_dir");
   Project_Files                    : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"project_files");
   Project_Path                     : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"project_path");
   Required_Toolchain_Version       : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"required_toolchain_version");
   Roots                            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"roots");
   Run_Path_Option                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"run_path_option");
   Run_Path_Origin                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"run_path_origin");
   Runtime                          : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"runtime");
   Runtime_Dir                      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"runtime_dir");
   Runtime_Library_Dir              : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"runtime_library_dir");
   Runtime_Source_Dir               : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"runtime_source_dir");
   Runtime_Source_Dirs              : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"runtime_source_dirs");
   Separate_Run_Path_Options        : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"separate_run_path_options");
   Shared_Library_Minimum_Switches  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"shared_library_minimum_switches");
   Shared_Library_Prefix            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"shared_library_prefix");
   Shared_Library_Suffix            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"shared_library_suffix");
   Source_Dirs                      : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"source_dirs");
   Source_Files                     : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"source_files");
   Source_List_File                 : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"source_list_file");
   Symbolic_Link_Supported          : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"symbolic_link_supported");
   Target                           : constant Q_Attribute_Id :=
                                        (Project_Level_Scope, +"target");
   Toolchain_Version                : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"toolchain_version");
   Toolchain_Name                   : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"toolchain_name");
   Toolchain_Description            : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"toolchain_description");
   Toolchain_Path                   : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"toolchain_path");
   Warning_Message                  : constant Q_Attribute_Id :=
                                        (Project_Level_Scope,
                                         +"warning_message");

   --  Binder attribute qualified names

   package Binder is
      Bindfile_Option_Substitution : constant Q_Attribute_Id :=
                                       (Pack.Binder,
                                        +"bindfile_option_substitution");
      Default_Switches             : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"default_switches");
      Driver                       : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"driver");
      Objects_Path                 : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"objects_path");
      Objects_Path_File            : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"objects_path_file");
      Prefix                       : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"prefix");
      Required_Switches            : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"required_switches");
      Switches                     : constant Q_Attribute_Id :=
                                       (Pack.Binder, +"switches");
   end Binder;

   --  Builder attribute qualified names

   package Builder is
      Default_Switches             : constant Q_Attribute_Id :=
                                       (Pack.Builder, +"default_switches");
      Executable                   : constant Q_Attribute_Id :=
                                       (Pack.Builder, +"executable");
      Executable_Suffix            : constant Q_Attribute_Id :=
                                       (Pack.Builder, +"executable_suffix");
      Global_Compilation_Switches  : constant Q_Attribute_Id :=
                                       (Pack.Builder,
                                        +"global_compilation_switches");
      Global_Config_File           : constant Q_Attribute_Id :=
                                       (Pack.Builder, +"global_config_file");
      Global_Configuration_Pragmas : constant Q_Attribute_Id :=
                                       (Pack.Builder,
                                        +"global_configuration_pragmas");
      Switches                     : constant Q_Attribute_Id :=
                                       (Pack.Builder, +"switches");
   end Builder;

   --  Clean attribute qualified names

   package Clean is
      Artifacts_In_Exec_Dir      : constant Q_Attribute_Id :=
                                     (Pack.Clean, +"artifacts_in_exec_dir");
      Artifacts_In_Object_Dir    : constant Q_Attribute_Id :=
                                     (Pack.Clean, +"artifacts_in_object_dir");
      Object_Artifact_Extensions : constant Q_Attribute_Id :=
                                     (Pack.Clean,
                                      +"object_artifact_extensions");
      Source_Artifact_Extensions : constant Q_Attribute_Id :=
                                     (Pack.Clean,
                                      +"source_artifact_extensions");
      Switches                   : constant Q_Attribute_Id :=
                                     (Pack.Clean, +"switches");
   end Clean;

   --  Compiler attribute qualified names

   package Compiler is
      Config_Body_File_Name         : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_body_file_name");
      Config_Body_File_Name_Index   : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_body_file_name_index");
      Config_Body_File_Name_Pattern : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_body_file_name_pattern");
      Config_File_Switches          : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_file_switches");
      Config_File_Unique            : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"config_file_unique");
      Config_Spec_File_Name         : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_spec_file_name");
      Config_Spec_File_Name_Index   : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_spec_file_name_index");
      Config_Spec_File_Name_Pattern : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"config_spec_file_name_pattern");
      Default_Switches              : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"default_switches");
      Dependency_Driver             : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"dependency_driver");
      Dependency_Kind               : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"dependency_kind");
      Dependency_Switches           : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"dependency_switches");
      Driver                        : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"driver");
      Include_Path                  : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"include_path");
      Include_Path_File             : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"include_path_file");
      Include_Switches              : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"include_switches");
      Language_Kind                 : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"language_kind");
      Leading_Required_Switches     : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"leading_required_switches");
      Local_Config_File             : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"local_config_file");
      Local_Configuration_Pragmas   : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"local_configuration_pragmas");
      Mapping_Body_Suffix           : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"mapping_body_suffix");
      Mapping_File_Switches         : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"mapping_file_switches");
      Mapping_Spec_Suffix           : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"mapping_spec_suffix");
      Max_Command_Line_Length       : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"max_command_line_length");
      Multi_Unit_Object_Separator   : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"multi_unit_object_separator");
      Multi_Unit_Switches           : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"multi_unit_switches");
      Object_File_Suffix            : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"object_file_suffix");
      Object_File_Switches          : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"object_file_switches");
      Object_Path_Switches          : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"object_path_switches");
      Path_Syntax                   : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"path_syntax");
      Pic_Option                    : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"pic_option");
      Required_Switches             : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"required_switches");
      Response_File_Format          : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"response_file_format");
      Response_File_Switches        : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"response_file_switches");
      Source_File_Switches          : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"source_file_switches");
      Switches                      : constant Q_Attribute_Id :=
                                        (Pack.Compiler, +"switches");
      Trailing_Required_Switches    : constant Q_Attribute_Id :=
                                        (Pack.Compiler,
                                         +"trailing_required_switches");
   end Compiler;

   --  Gnatls attribute qualified names

   package Gnatls is
      Switches : constant Q_Attribute_Id := (Pack.Gnatls, +"switches");
   end Gnatls;

   --  Install attribute qualified names

   package Install is
      ALI_Subdir         : constant Q_Attribute_Id :=
                             (Pack.Install, +"ali_subdir");
      Active             : constant Q_Attribute_Id :=
                             (Pack.Install, +"active");
      Artifacts          : constant Q_Attribute_Id :=
                             (Pack.Install, +"artifacts");
      Exec_Subdir        : constant Q_Attribute_Id :=
                             (Pack.Install, +"exec_subdir");
      Install_Name       : constant Q_Attribute_Id :=
                             (Pack.Install, +"install_name");
      Install_Project    : constant Q_Attribute_Id :=
                             (Pack.Install, +"install_project");
      Lib_Subdir         : constant Q_Attribute_Id :=
                             (Pack.Install, +"lib_subdir");
      Link_Lib_Subdir    : constant Q_Attribute_Id :=
                             (Pack.Install, +"link_lib_subdir");
      Mode               : constant Q_Attribute_Id :=
                             (Pack.Install, +"mode");
      Prefix             : constant Q_Attribute_Id :=
                             (Pack.Install, +"prefix");
      Project_Subdir     : constant Q_Attribute_Id :=
                             (Pack.Install, +"project_subdir");
      Required_Artifacts : constant Q_Attribute_Id :=
                             (Pack.Install, +"required_artifacts");
      Side_Debug         : constant Q_Attribute_Id :=
                             (Pack.Install, +"side_debug");
      Sources_Subdir     : constant Q_Attribute_Id :=
                             (Pack.Install, +"sources_subdir");
   end Install;

   --  Linker attribute qualified names

   package Linker is
      Default_Switches        : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"default_switches");
      Driver                  : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"driver");
      Export_File_Format      : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"export_file_format");
      Export_File_Switch      : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"export_file_switch");
      Leading_Switches        : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"leading_switches");
      Linker_Options          : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"linker_options");
      Map_File_Option         : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"map_file_option");
      Max_Command_Line_Length : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"max_command_line_length");
      Required_Switches       : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"required_switches");
      Response_File_Format    : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"response_file_format");
      Response_File_Switches  : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"response_file_switches");
      Switches                : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"switches");
      Trailing_Switches       : constant Q_Attribute_Id :=
                                  (Pack.Linker, +"trailing_switches");
   end Linker;

   --  Naming attribute qualified names

   package Naming is
      Body_N                      : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"body");
      Body_Suffix                 : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"body_suffix");
      Casing                      : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"casing");
      Dot_Replacement             : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"dot_replacement");
      Implementation              : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"implementation");
      Implementation_Exceptions   : constant Q_Attribute_Id :=
                                      (Pack.Naming,
                                       +"implementation_exceptions");
      Implementation_Suffix       : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"implementation_suffix");
      Separate_Suffix             : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"separate_suffix");
      Spec                        : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"spec");
      Spec_Suffix                 : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"spec_suffix");
      Specification               : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"specification");
      Specification_Exceptions    : constant Q_Attribute_Id :=
                                      (Pack.Naming,
                                       +"specification_exceptions");
      Specification_Suffix        : constant Q_Attribute_Id :=
                                      (Pack.Naming, +"specification_suffix");
   end Naming;

   --  Remote attribute qualified names

   package Remote is
      Excluded_Patterns          : constant Q_Attribute_Id :=
                                     (Pack.Remote, +"excluded_patterns");
      Included_Patterns          : constant Q_Attribute_Id :=
                                     (Pack.Remote, +"included_patterns");
      Included_Artifact_Patterns : constant Q_Attribute_Id :=
                                     (Pack.Remote,
                                      +"included_artifact_patterns");
      Root_Dir                   : constant Q_Attribute_Id :=
                                     (Pack.Remote, +"root_dir");
   end Remote;

private

   package Attribute_Definitions is new Ada.Containers.Ordered_Maps
     (Q_Attribute_Id, Def);

   type Def_Access is access constant Def;

   package Default_References is new Ada.Containers.Ordered_Maps
     (Q_Attribute_Id, Def_Access);
   --  To keep references only to attribute definitions with default rules

   type Default_Rules is access constant Default_References.Map;

   package Attribute_Aliases is new Ada.Containers.Ordered_Maps
     (Q_Attribute_Id, Q_Attribute_Id);
   --  Keeps track of Attributes that are aliased

end GPR2.Project.Registry.Attribute;
