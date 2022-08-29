--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GNAT.OS_Lib;

with GPR2.Project.View;

package body GPR2.Project.Registry.Attribute is

   package Pack_Defaults is new Ada.Containers.Ordered_Maps
     (Package_Id, Default_References.Map,
      "=" => Default_References."=");

   Store    : Attribute_Definitions.Map;
   Defaults : Pack_Defaults.Map;
   Aliases  : Attribute_Aliases.Map;

   In_Library       : constant Allowed_In :=
                        (K_Library           |
                         K_Aggregate_Library |
                         K_Abstract => True,
                         others     => False);

   In_Aggregates    : constant Allowed_In :=
                        (Aggregate_Kind => True,
                         others         => False);

   No_Aggregates    : constant Allowed_In :=
                        (Aggregate_Kind => False,
                         others         => True);

   In_Configuration : constant Allowed_In :=
                        (K_Configuration => True, others => False);

   No_Aggregates_Abstract : constant Allowed_In :=
                        (Aggregate_Kind | K_Abstract => False,
                         others         => True);

   --  Constants for some common attribute definitions

   function Create (Index, Value : Value_Type) return Default_Value;
   function Create (Ref : Q_Attribute_Id) return Default_Value;
   function Create (Value : Value_Type) return Default_Value is
     (Create (Value_Type (Any_Index), Value));
   --  Create container for attribute default value

   function "+" (Left, Right : Default_Value) return Default_Value
     with Pre => Left.Kind = D_Value and then Right.Kind = D_Value;
   --  Concatenate 2 default values for different indexes into one container

   function Default_Library_Standalone
     (View : Project.View.Object) return Value_Type
   is (if View.Is_Library and then View.Has_Any_Interfaces
       then "standard" else "no");

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Default_Value) return Default_Value is
      Result : Value_Map.Map := Left.Values;
   begin
      for C in Right.Values.Iterate loop
         Result.Insert (Value_Map.Key (C), Right.Values (C));
      end loop;

      return Default_Value'(D_Value, Result);
   end "+";

   ---------
   -- Add --
   ---------

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
   is
      procedure Index_Default;
      --  Save definition with default value to Defaults index

      -------------------
      -- Index_Default --
      -------------------

      procedure Index_Default is
         CP     : Pack_Defaults.Cursor;
         OK     : Boolean;
      begin
         Defaults.Insert (Name.Pack, Default_References.Empty_Map, CP, OK);
         Defaults (CP).Insert (Name, Store (Name).Element);
      end Index_Default;

   begin
      Store.Insert
        (Name,
         Def'(Index_Type            => Index_Type,
              Index_Optional        => Index_Optional,
              Value                 => Value,
              Value_Case_Sensitive  => Value_Case_Sensitive,
              Empty_Value           => Empty_Value,
              Builtin               => Is_Builtin,
              Is_Allowed_In         => Is_Allowed_In,
              Default               => Default,
              Has_Default_In        => (if Default = No_Default_Value
                                        then Nowhere
                                        else (if Has_Default_In = Nowhere
                                          then Is_Allowed_In
                                          else Has_Default_In)),
              Is_Toolchain_Config   => Is_Toolchain_Config,
              Config_Concatenable   => Config_Concatenable,
              Inherit_From_Extended => (if Name.Pack = Project_Level_Scope
                                        then Inherit_From_Extended
                                        else Not_Inherited),
              Value_Is_Set          => Is_Set));

      if Default /= No_Default_Value then
         Index_Default;
      end if;
   end Add;

   ---------------
   -- Add_Alias --
   ---------------

   procedure Add_Alias
     (Name     : Q_Attribute_Id;
      Alias_Of : Q_Attribute_Id)
   is
   begin
      Aliases.Include (Name, Alias_Of);
      Aliases.Include (Alias_Of, Name);
   end Add_Alias;

   -----------
   -- Alias --
   -----------

   function Alias (Name : Q_Attribute_Id) return Q_Optional_Attribute_Id
   is
      C : constant Attribute_Aliases.Cursor := Aliases.Find (Name);
   begin
      if not Attribute_Aliases.Has_Element (C) then
         return No_Attribute_Id;
      else
         return Attribute_Aliases.Element (C);
      end if;
   end Alias;

   --------------------
   -- All_Attributes --
   --------------------

   function All_Attributes
     (Pack : Package_Id) return Containers.Attribute_Id_List
   is
      Result : Containers.Attribute_Id_List;
   begin
      for C in Store.Iterate loop
         declare
            Q_Name : constant Q_Attribute_Id := Attribute_Definitions.Key (C);
            A      : Q_Optional_Attribute_Id;
         begin
            if Q_Name.Pack = Pack then
               Result.Insert (Q_Name);
               A := Alias (Q_Name);

               if A /= No_Attribute_Id then
                  Result.Insert (A);
               end if;
            end if;
         end;
      end loop;

      return Result;
   end All_Attributes;

   ------------
   -- Create --
   ------------

   function Create (Index, Value : Value_Type) return Default_Value is
      Result : Default_Value (D_Value);
   begin
      Result.Values.Insert (Index, Value);

      return Result;
   end Create;

   function Create (Ref : Q_Attribute_Id) return Default_Value is
   begin
      return Default_Value'(D_Attribute_Reference, Ref.Attr);
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists (Q_Name : Q_Attribute_Id) return Boolean is
   begin
      return Store.Contains (Q_Name)
        or else (Aliases.Contains (Q_Name)
                 and then Store.Contains (Aliases (Q_Name)));
   end Exists;

   ------------
   -- Exists --
   ------------

   function Exists
     (Map : Value_Map.Map; Key : Value_Type := Any_Index) return Boolean
   is
      use Value_Map;
      C : Cursor := Map.Find (Key);

   begin
      if Has_Element (C) then
         return True;
      end if;

      C := Map.Find (Any_Index);

      if Has_Element (C) then
         return True;
      end if;

      return False;
   end Exists;

   ----------------------
   -- For_Each_Default --
   ----------------------

   procedure For_Each_Default
     (Rules  : Default_Rules;
      Action : not null access procedure
        (Attribute : Q_Attribute_Id; Definition : Def))
   is
      procedure Each_Element (C : Default_References.Cursor);

      ------------------
      -- Each_Element --
      ------------------

      procedure Each_Element (C : Default_References.Cursor) is
      begin
         Action
           (Default_References.Key (C),
            Default_References.Element (C).all);
      end Each_Element;

   begin
      Rules.Iterate (Each_Element'Access);
   end For_Each_Default;

   ---------
   -- Get --
   ---------

   function Get (Q_Name : Q_Attribute_Id) return Def
   is
      C : constant Attribute_Definitions.Cursor := Store.Find (Q_Name);
   begin
      if Attribute_Definitions.Has_Element (C) then
         return Attribute_Definitions.Element (C);
      else
         return Store (Aliases.Element (Q_Name));
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Map : Value_Map.Map; Key : Value_Type := Any_Index) return Value_Type
   is
      use Value_Map;
      C : Cursor := Map.Find (Key);

   begin
      if Has_Element (C) then
         return Element (C);
      end if;

      C := Map.Find (Any_Index);

      if Has_Element (C) then
         return Element (C);
      end if;

      return "";
   end Get;

   -----------------------
   -- Get_Default_Rules --
   -----------------------

   function Get_Default_Rules
     (Pack : Package_Id) return Default_Rules
   is
      CR : constant Pack_Defaults.Cursor := Defaults.Find (Pack);
   begin
      if Pack_Defaults.Has_Element (CR) then
         return Defaults (CR).Element;
      else
         return Default_References.Empty_Map'Unrestricted_Access;
      end if;
   end Get_Default_Rules;

   -------------------------------
   -- Get_Packages_With_Default --
   -------------------------------

   function Get_Packages_With_Default return Containers.Package_Id_List
   is
      Result : Containers.Package_Id_List;
   begin
      for C in Defaults.Iterate loop
         if Pack_Defaults.Key (C) /= Project_Level_Scope then
            Result.Include (Pack_Defaults.Key (C));
         end if;
      end loop;

      return Result;
   end Get_Packages_With_Default;

   ---------------
   -- Has_Alias --
   ---------------

   function Has_Alias (Name : Q_Attribute_Id) return Boolean
   is
   begin
      return Aliases.Contains (Name);
   end Has_Alias;

   -----------------------
   -- Is_Case_Sensitive --
   -----------------------

   function Is_Case_Sensitive
     (Index_Value : Value_Type;
      Index_Type  : Index_Value_Type) return Boolean is
   begin
      case Index_Type is
         when No_Index =>
            return False; -- Don't care

         when String_Index =>
            return True;

         when Unit_Index =>
            return False;

         when Env_Var_Name_Index =>
            return True; --  ??? to be checked

         when File_Index | FileGlob_Index =>
            return GPR2.File_Names_Case_Sensitive;

         when FileGlob_Or_Language_Index =>
            --  If host is case insensitive, return False whatever the
            --  index value.

            if not GPR2.File_Names_Case_Sensitive then
               return False;
            end if;

            for J in Index_Value'Range loop
               if J > Index_Value'First
                 and then Index_Value (J) = '.'
               then
                  --  most probably a file extension

                  return GPR2.File_Names_Case_Sensitive;

               elsif Index_Value (J) in '[' | ']' | '*' | '?' then
                  --  glob pattern
                  return GPR2.File_Names_Case_Sensitive;
               end if;
            end loop;

            --  No dots or glob pattern: consider it a language

            return False;

         when Language_Index =>
            return False;
      end case;
   end Is_Case_Sensitive;

begin
   --  name
   Add
     (Name                  => Name,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Is_Builtin            => True,
      Inherit_From_Extended => Not_Inherited);

   --  project_dir
   Add
     (Name                  => Project_Dir,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Is_Builtin            => True,
      Inherit_From_Extended => Not_Inherited);

   --  main
   Add
     (Name                 => Main,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates);

   --  languages
   Add
     (Name                  => Languages,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => No_Aggregates,
      Default               => Create ("Ada"),
      Has_Default_In        => No_Aggregates_Abstract,
      Inherit_From_Extended => Concatenated,
      Is_Set                => True);

   --  roots
   Add
     (Name                 => Roots,
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates);

   --  externally_built
   Add
     (Name                  => Externally_Built,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited,
      Default               => Create ("false"),
      Has_Default_In        => Everywhere);

   --  object_dir
   Add
     (Name                  => Object_Dir,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create ("."),
      Inherit_From_Extended => Not_Inherited);

   --  exec_dir
   Add
     (Name                  => Exec_Dir,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Default               => Create (Object_Dir),
      Inherit_From_Extended => Not_Inherited);

   --  source_dirs
   Add
     (Name                  => Source_Dirs,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Default               => Create ("."),
      Has_Default_In        => No_Aggregates_Abstract,
      Inherit_From_Extended => Not_Inherited);

   --  inherit_source_path
   Add
     (Name                 => Inherit_Source_Path,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_dirs
   Add
     (Name                  => Excluded_Source_Dirs,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  ignore_source_sub_dirs
   Add
     (Name                  => Ignore_Source_Sub_Dirs,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  source_files
   Add
     (Name                  => Source_Files,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  excluded_source_files, Locally_Removed_Files
   Add
     (Name                 => Excluded_Source_Files,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);
   Add_Alias (Name     => Locally_Removed_Files,
              Alias_Of => Excluded_Source_Files);

   --  source_list_file
   Add
     (Name                  => Source_List_File,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  excluded_source_list_file
   Add
     (Name                  => Excluded_Source_List_File,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  interfaces
   Add
     (Name                  => Interfaces,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => GPR2.File_Names_Case_Sensitive,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Inherited,
      Is_Set                => True);

   --  project_files
   Add
     (Name                  => Project_Files,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  project_path
   Add
     (Name                  => Project_Path,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  external
   Add
     (Name                 => External,
      Index_Type           => Env_Var_Name_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => (K_Aggregate => True, others => False));

   --  library_dir
   Add
     (Name                 => Library_Dir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library);

   --  library_name
   Add
     (Name                  => Library_Name,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library);

   --  library_kind
   Add
     (Name                  => Library_Kind,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited,
      Default               => Create ("static"));

   --  library_version
   Add
     (Name                  => Library_Version,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited);

   --  library_interface
   Add
     (Name                  => Library_Interface,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Inherited,
      Is_Set                => True);

   --  library_standalone
   Add
     (Name                  => Library_Standalone,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => In_Library,
      Default               => (D_Callback, Default_Library_Standalone'Access),
      Inherit_From_Extended => Not_Inherited);

   --  library_encapsulated_options
   Add
     (Name                  => Library_Encapsulated_Options,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_encapsulated_supported
   Add
     (Name                  => Library_Encapsulated_Supported,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Configuration,
      Config_Concatenable   => False,
      Inherit_From_Extended => Not_Inherited);

   --  library_auto_init
   Add
     (Name                  => Library_Auto_Init,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Default               => Create ("false"),
      Inherit_From_Extended => Not_Inherited);

   --  leading_library_options
   Add
     (Name                  => Leading_Library_Options,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_options
   Add
     (Name                  => Library_Options,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_rpath_options
   Add
     (Name                 => Library_Rpath_Options,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  library_src_dir
   Add
     (Name                 => Library_Src_Dir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library,
      Default              => Create (Library_Dir),
      Inherit_From_Extended => Not_Inherited);

   --  library_ali_dir
   Add
     (Name                 => Library_Ali_Dir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library,
      Default              => Create (Library_Dir),
      Inherit_From_Extended => Not_Inherited);

   --  library_gcc
   Add
     (Name                  => Library_Gcc,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_symbol_file
   Add
     (Name                  => Library_Symbol_File,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited);

   --  library_symbol_policy
   Add
     (Name                  => Library_Symbol_Policy,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited);

   --  library_reference_symbol_file
   Add
     (Name                  => Library_Reference_Symbol_File,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  default_language
   Add
     (Name                  => Default_Language,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  run_path_option
   Add
     (Name                  => Run_Path_Option,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  run_path_origin
   Add
     (Name                  => Run_Path_Origin,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  separate_run_path_options
   Add
     (Name                  => Separate_Run_Path_Options,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  toolchain_version
   Add
     (Name                 => Toolchain_Version,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  toolchain_name
   Add
     (Name                 => Toolchain_Name,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  toolchain_path
   Add
     (Name                 => Toolchain_Path,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  required_toolchain_version
   Add
     (Name                 => Required_Toolchain_Version,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  toolchain_description
   Add
     (Name                 => Toolchain_Description,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  object_generated
   Add
     (Name                 => Object_Generated,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  objects_linked
   Add
     (Name                 => Objects_Linked,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  target
   Add
     (Name                  => Target,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Is_Toolchain_Config   => True,
      Inherit_From_Extended => Inherited,
      Default               => Create ("all"));

   --  runtime
   Add
     (Name                 => Runtime,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  library_builder
   Add
     (Name                  => Library_Builder,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_support
   Add
     (Name                  => Library_Support,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_builder
   Add
     (Name                  => Archive_Builder,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_builder_append_option
   Add
     (Name                  => Archive_Builder_Append_Option,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_indexer
   Add
     (Name                  => Archive_Indexer,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_suffix
   Add
     (Name                  => Archive_Suffix,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create (".a"),
      Inherit_From_Extended => Not_Inherited);

   --  library_partial_linker
   Add
     (Name                  => Library_Partial_Linker,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  object_lister
   Add
     (Name                 => Object_Lister,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  object_lister_matcher
   Add
     (Name                 => Object_Lister_Matcher,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  shared_library_prefix
   Add
     (Name                  => Shared_Library_Prefix,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create ("lib"),
      Inherit_From_Extended => Not_Inherited);

   --  shared_library_suffix
   Add
     (Name                  => Shared_Library_Suffix,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create (".so"),
      Inherit_From_Extended => Not_Inherited);

   --  symbolic_link_supported
   Add
     (Name                  => Symbolic_Link_Supported,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_major_minor_id_supported
   Add
     (Name                  => Library_Major_Minor_Id_Supported,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_auto_init_supported
   Add
     (Name                  => Library_Auto_Init_Supported,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  shared_library_minimum_switches
   Add
     (Name                  => Shared_Library_Minimum_Switches,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_version_switches
   Add
     (Name                  => Library_Version_Switches,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_install_name_option
   Add
     (Name                  => Library_Install_Name_Option,
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  runtime_dir
   Add
     (Name                 => Runtime_Dir,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  runtime_library_dir
   Add
     (Name                  => Runtime_Library_Dir,
      Index_Type            => Language_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  runtime_source_dir
   Add
     (Name                 => Runtime_Source_Dir,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  runtime_source_dirs
   Add
     (Name                 => Runtime_Source_Dirs,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  naming.spec_suffix & specification_suffix
   Add
     (Name                 => Naming.Spec_Suffix,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".ads") + Create ("c", ".h"));
   Add_Alias (Name     => Naming.Spec_Suffix,
              Alias_Of => Naming.Specification_Suffix);

   --  naming.body_suffix & implementation_suffix
   Add
     (Name                 => Naming.Body_Suffix,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".adb") + Create ("c", ".c"));
   Add_Alias (Name     => Naming.Body_Suffix,
              Alias_Of => Naming.Implementation_Suffix);

   --  naming.separate_suffix
   Add
     (Name                 => Naming.Separate_Suffix,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Empty_Value          => Error,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Naming.Body_Suffix));

   --  naming.casing
   Add
     (Name                 => Naming.Casing,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("lowercase"));

   --  naming.dot_replacement
   Add
     (Name                 => Naming.Dot_Replacement,
      Index_Type           => No_Index,
      Empty_Value          => Error,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("-"));

   --  naming.spec & specification
   Add
     (Name                 => Naming.Spec,
      Index_Type           => Unit_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);
   Add_Alias (Name => Naming.Spec, Alias_Of => Naming.Specification);

   --  naming.body & implementation
   Add
     (Name                 => Naming.Body_N,
      Index_Type           => Unit_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);
   Add_Alias (Name => Naming.Body_N, Alias_Of => Naming.Implementation);

   --  naming.specification_exceptions
   Add
     (Name                 => Naming.Specification_Exceptions,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  naming.implementation_exceptions
   Add
     (Name                 => Naming.Implementation_Exceptions,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.default_switches
   --  compiler.switches
   Add
     (Name                 => Compiler.Switches,
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);
   Add_Alias (Name     => Compiler.Default_Switches,
              Alias_Of => Compiler.Switches);

   --  compiler.local_configuration_pragmas
   Add
     (Name                 => Compiler.Local_Configuration_Pragmas,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_config_file
   Add
     (Name                 => Compiler.Local_Config_File,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.driver
   Add
     (Name                 => Compiler.Driver,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.language_kind
   Add
     (Name                 => Compiler.Language_Kind,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_kind
   Add
     (Name                 => Compiler.Dependency_Kind,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.required_switches
   Add
     (Name                 => Compiler.Required_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.leading_required_switches
   Add
     (Name                 => Compiler.Leading_Required_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.trailing_required_switches
   Add
     (Name                 => Compiler.Trailing_Required_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.pic_option
   Add
     (Name                 => Compiler.Pic_Option,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.path_syntax
   Add
     (Name                 => Compiler.Path_Syntax,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.source_file_switches
   Add
     (Name                 => Compiler.Source_File_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.object_file_suffix
   Add
     (Name                 => Compiler.Object_File_Suffix,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create (".o"));

   --  compiler.object_file_switches
   Add
     (Name                 => Compiler.Object_File_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.multi_unit_switches
   Add
     (Name                 => Compiler.Multi_Unit_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.multi_unit_object_separator
   Add
     (Name                 => Compiler.Multi_Unit_Object_Separator,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_file_switches
   Add
     (Name                 => Compiler.Mapping_File_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.mapping_spec_suffix
   Add
     (Name                 => Compiler.Mapping_Spec_Suffix,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_body_suffix
   Add
     (Name                 => Compiler.Mapping_Body_Suffix,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_switches
   Add
     (Name                 => Compiler.Config_File_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.config_body_file_name
   Add
     (Name                 => Compiler.Config_Body_File_Name,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_index
   Add
     (Name                 => Compiler.Config_Body_File_Name_Index,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_pattern
   Add
     (Name                 => Compiler.Config_Body_File_Name_Pattern,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name
   Add
     (Name                 => Compiler.Config_Spec_File_Name,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_index
   Add
     (Name                 => Compiler.Config_Spec_File_Name_Index,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_pattern
   Add
     (Name                 => Compiler.Config_Spec_File_Name_Pattern,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_unique
   Add
     (Name                 => Compiler.Config_File_Unique,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_switches
   Add
     (Name                 => Compiler.Dependency_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.dependency_driver
   Add
     (Name                 => Compiler.Dependency_Driver,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_switches
   Add
     (Name                 => Compiler.Include_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.include_path
   Add
     (Name                 => Compiler.Include_Path,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_path_file
   Add
     (Name                 => Compiler.Include_Path_File,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_path_switches
   Add
     (Name                 => Compiler.Object_Path_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.max_command_line_length
   Add
     (Name                 => Compiler.Max_Command_Line_Length,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_format
   Add
     (Name                 => Compiler.Response_File_Format,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_switches
   Add
     (Name                 => Compiler.Response_File_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  builder.switches, builder.default_switches
   Add
     (Name                 => Builder.Switches,
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);
   Add_Alias (Name => Builder.Default_Switches, Alias_Of => Builder.Switches);

   --  builder.global_compilation_switches
   Add
     (Name                 => Builder.Global_Compilation_Switches,
      Index_Type           => Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  builder.executable
   Add
     (Name                 => Builder.Executable,
      Index_Type           => File_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Empty_Value          => Ignore,
      Is_Allowed_In        => No_Aggregates);

   --  builder.executable_suffix
   Add
     (Name                 => Builder.Executable_Suffix,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create (GNAT.OS_Lib.Get_Executable_Suffix.all));

   --  builder.global_configuration_pragmas
   Add
     (Name                 => Builder.Global_Configuration_Pragmas,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  builder.global_config_file
   Add
     (Name                 => Builder.Global_Config_File,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  gnatls.switches
   Add
     (Name                 => Gnatls.Switches,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.switches, binder.default_switches
   Add
     (Name                 => Binder.Switches,
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);
   Add_Alias (Name => Binder.Default_Switches, Alias_Of => Binder.Switches);

   --  binder.driver
   Add
     (Name                 => Binder.Driver,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.required_switches
   Add
     (Name                 => Binder.Required_Switches,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  binder.prefix
   Add
     (Name                 => Binder.Prefix,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create (""));

   --  binder.objects_path
   Add
     (Name                 => Binder.Objects_Path,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.objects_path_file
   Add
     (Name                 => Binder.Objects_Path_File,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.bindfile_option_substitution
   Add
     (Name                 => Binder.Bindfile_Option_Substitution,
      Index_Type           => String_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => (K_Configuration | K_Abstract
                               | K_Standard => True, others => False));

   --  linker.required_switches
   Add
     (Name                 => Linker.Required_Switches,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.leading_switches
   Add
     (Name                 => Linker.Leading_Switches,
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.switches, linker.default_switches
   Add
     (Name                 => Linker.Switches,
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);
   Add_Alias (Name => Linker.Default_Switches, Alias_Of => Linker.Switches);

   --  linker.trailing_switches
   Add
     (Name                 => Linker.Trailing_Switches,
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.linker_options
   Add
     (Name                  => Linker.Linker_Options,
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  linker.map_file_option
   Add
     (Name                 => Linker.Map_File_Option,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.driver
   Add
     (Name                 => Linker.Driver,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.max_command_line_length
   Add
     (Name                 => Linker.Max_Command_Line_Length,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_format
   Add
     (Name                 => Linker.Response_File_Format,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_switches
   Add
     (Name                 => Linker.Response_File_Switches,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.export_file_format
   Add
     (Name                 => Linker.Export_File_Format,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_switch
   Add
     (Name                 => Linker.Export_File_Switch,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.switches
   Add
     (Name                 => Clean.Switches,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  clean.source_artifact_extensions
   Add
     (Name                 => Clean.Source_Artifact_Extensions,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.object_artifact_extensions
   Add
     (Name                 => Clean.Object_Artifact_Extensions,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_exec_dir
   Add
     (Name                 => Clean.Artifacts_In_Exec_Dir,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_object_dir
   Add
     (Name                 => Clean.Artifacts_In_Object_Dir,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.prefix
   Add
     (Name                 => Install.Prefix,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.sources_subdir
   Add
     (Name                 => Install.Sources_Subdir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.exec_subdir
   Add
     (Name                 => Install.Exec_Subdir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Add
     (Name                 => Install.Lib_Subdir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Add
     (Name                 => Install.ALI_Subdir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.project_subdir
   Add
     (Name                 => Install.Project_Subdir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.active
   Add
     (Name                 => Install.Active,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.artifacts
   Add
     (Name                 => Install.Artifacts,
      Index_Type           => File_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.required_artifacts
   Add
     (Name                 => Install.Required_Artifacts,
      Index_Type           => File_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.mode
   Add
     (Name                 => Install.Mode,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.install_name
   Add
     (Name                 => Install.Install_Name,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.root_dir
   Add
     (Name                 => Remote.Root_Dir,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.excluded_patterns
   Add
     (Name                 => Remote.Excluded_Patterns,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.included_patterns
   Add
     (Name                 => Remote.Included_Patterns,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.included_artifact_patterns
   Add
     (Name                 => Remote.Included_Artifact_Patterns,
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  origin_project
   Add
     (Name                 => Origin_Project,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install'side_debug
   Add
     (Name                 => Install.Side_Debug,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => Everywhere);

   --  include_switches_via_spec
   Add
     (Name                 => Include_Switches_Via_Spec,
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  only_dirs_with_sources
   Add
     (Name                 => Only_Dirs_With_Sources,
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  warning_message
   Add
     (Name                 => Warning_Message,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  canonical_target
   Add
     (Name                 => Canonical_Target,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  GPR2.Create_missing_dirs
   Add
     (Name                 => Create_Missing_Dirs,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install_project
   Add
     (Name                 => Install.Install_Project,
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => Everywhere);

end GPR2.Project.Registry.Attribute;
