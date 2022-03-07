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

with GNAT.OS_Lib;

with GPR2.Project.Registry.Pack;
with GPR2.Project.View;

package body GPR2.Project.Registry.Attribute is

   package Pack_Defaults is new Ada.Containers.Ordered_Maps
     (Optional_Package_Id, Default_References.Map,
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
   function Create (Ref : Attribute_Id) return Default_Value;
   function Create (Value : Value_Type) return Default_Value is
     (Create (Value_Type (Any_Index), Value));
   --  Create container for attribute default value

   function "+" (Left, Right : Default_Value) return Default_Value
     with Pre => Left.Kind = D_Value and then Right.Kind = D_Value;
   --  Concatenate 2 default values for different indexes into one container

   function Default_Library_Standalone
     (View : Project.View.Object) return Value_Type is
     (if View.Is_Library and then View.Has_Any_Interfaces
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
         Defaults (CP).Insert (Name.Attr, Store (Name).Element);
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
              Inherit_From_Extended => (if Name.Pack = No_Package
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
     (Name     : Qualified_Name;
      Alias_Of : Qualified_Name)
   is
   begin
      Aliases.Include (Name, Alias_Of);
      Aliases.Include (Alias_Of, Name);
   end Add_Alias;

   -----------
   -- Alias --
   -----------

   function Alias (Name : Qualified_Name) return Qualified_Name
   is
      C : constant Attribute_Aliases.Cursor := Aliases.Find (Name);
   begin
      if not Attribute_Aliases.Has_Element (C) then
         return No_Name;
      else
         return Attribute_Aliases.Element (C);
      end if;
   end Alias;

   --------------------
   -- All_Attributes --
   --------------------

   function All_Attributes
     (Pack : Optional_Package_Id) return Containers.Attribute_Id_List
   is
      Result : Containers.Attribute_Id_List;
   begin
      for C in Store.Iterate loop
         declare
            Q_Name : constant Qualified_Name := Attribute_Definitions.Key (C);
            A      : Qualified_Name;
         begin
            if Q_Name.Pack = Pack then
               Result.Insert (Q_Name.Attr);
               A := Alias (Q_Name);

               if A /= No_Name then
                  Result.Insert (A.Attr);
               end if;
            end if;
         end;
      end loop;

      return Result;
   end All_Attributes;

   ------------
   -- Create --
   ------------

   function Create
     (Name : Attribute_Id;
      Pack : Optional_Package_Id := No_Package) return Qualified_Name is
   begin
      return (Pack => Pack, Attr => Name);
   end Create;

   function Create (Index, Value : Value_Type) return Default_Value is
      Result : Default_Value (D_Value);
   begin
      Result.Values.Insert (Index, Value);

      return Result;
   end Create;

   function Create (Ref : Attribute_Id) return Default_Value is
   begin
      return Default_Value'(D_Attribute_Reference, Ref);
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists (Q_Name : Qualified_Name) return Boolean is
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
        (Attribute : Attribute_Id; Definition : Def))
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

   function Get (Q_Name : Qualified_Name) return Def
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
     (Pack : Optional_Package_Id) return Default_Rules
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
         if Pack_Defaults.Key (C) /= No_Package then
            Result.Include (Pack_Defaults.Key (C));
         end if;
      end loop;

      return Result;
   end Get_Packages_With_Default;

   ---------------
   -- Has_Alias --
   ---------------

   function Has_Alias (Name : Qualified_Name) return Boolean
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
     (Create (Name),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Is_Builtin            => True,
      Inherit_From_Extended => Not_Inherited);

   --  project_dir
   Add
     (Create (Project_Dir),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Is_Builtin            => True,
      Inherit_From_Extended => Not_Inherited);

   --  main
   Add
     (Create (Main),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates);

   --  languages
   Add
     (Create (Languages),
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
     (Create (Roots),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates);

   --  externally_built
   Add
     (Create (Externally_Built),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited,
      Default               => Create ("false"),
      Has_Default_In        => Everywhere);

   --  object_dir
   Add
     (Create (Object_Dir),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create ("."),
      Inherit_From_Extended => Not_Inherited);

   --  exec_dir
   Add
     (Create (Exec_Dir),
      Index_Type            => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create (Object_Dir),
      Inherit_From_Extended => Not_Inherited);

   --  source_dirs
   Add
     (Create (Source_Dirs),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Default               => Create ("."),
      Has_Default_In        => No_Aggregates_Abstract,
      Inherit_From_Extended => Not_Inherited);

   --  inherit_source_path
   Add
     (Create (Inherit_Source_Path),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_dirs
   Add
     (Create (Excluded_Source_Dirs),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  ignore_source_sub_dirs
   Add
     (Create (Ignore_Source_Sub_Dirs),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  source_files
   Add
     (Create (Source_Files),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  excluded_source_files, Locally_Removed_Files
   Add
     (Create (Excluded_Source_Files),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);
   Add_Alias (Create (Locally_Removed_Files), Create (Excluded_Source_Files));

   --  source_list_file
   Add
     (Create (Source_List_File),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  excluded_source_list_file
   Add
     (Create (Excluded_Source_List_File),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => No_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  interfaces
   Add
     (Create (Interfaces),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Inherited);

   --  project_files
   Add
     (Create (Project_Files),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  project_path
   Add
     (Create (Project_Path),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Aggregates,
      Inherit_From_Extended => Not_Inherited);

   --  external
   Add
     (Create (External),
      Index_Type           => Env_Var_Name_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => (K_Aggregate => True, others => False));

   --  library_dir
   Add
     (Create (Library_Dir),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library);

   --  library_name
   Add
     (Create (Library_Name),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library);

   --  library_kind
   Add
     (Create (Library_Kind),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited,
      Default               => Create ("static"));

   --  library_version
   Add
     (Create (Library_Version),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited);

   --  library_interface
   Add
     (Create (Library_Interface),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Inherited,
      Is_Set                => True);

   --  library_standalone
   Add
     (Create (Library_Standalone),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => In_Library,
      Default               => (D_Callback, Default_Library_Standalone'Access),
      Inherit_From_Extended => Not_Inherited);

   --  library_encapsulated_options
   Add
     (Create (Library_Encapsulated_Options),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_encapsulated_supported
   Add
     (Create (Library_Encapsulated_Supported),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Configuration,
      Config_Concatenable   => False,
      Inherit_From_Extended => Not_Inherited);

   --  library_auto_init
   Add
     (Create (Library_Auto_Init),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Default               => Create ("false"),
      Inherit_From_Extended => Not_Inherited);

   --  leading_library_options
   Add
     (Create (Leading_Library_Options),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_options
   Add
     (Create (Library_Options),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_rpath_options
   Add
     (Create (Library_Rpath_Options),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  library_src_dir
   Add
     (Create (Library_Src_Dir),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library,
      Default              => Create (Library_Dir),
      Inherit_From_Extended => Not_Inherited);

   --  library_ali_dir
   Add
     (Create (Library_Ali_Dir),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => In_Library,
      Default              => Create (Library_Dir),
      Inherit_From_Extended => Not_Inherited);

   --  library_gcc
   Add
     (Create (Library_Gcc),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_symbol_file
   Add
     (Create (Library_Symbol_File),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited);

   --  library_symbol_policy
   Add
     (Create (Library_Symbol_Policy),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => In_Library,
      Inherit_From_Extended => Not_Inherited);

   --  library_reference_symbol_file
   Add
     (Create (Library_Reference_Symbol_File),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  default_language
   Add
     (Create (Default_Language),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  run_path_option
   Add
     (Create (Run_Path_Option),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  run_path_origin
   Add
     (Create (Run_Path_Origin),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  separate_run_path_options
   Add
     (Create (Separate_Run_Path_Options),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  toolchain_version
   Add
     (Create (Toolchain_Version),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  toolchain_name
   Add
     (Create (Toolchain_Name),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  toolchain_path
   Add
     (Create (Toolchain_Path),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  required_toolchain_version
   Add
     (Create (Required_Toolchain_Version),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  toolchain_description
   Add
     (Create (Toolchain_Description),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  object_generated
   Add
     (Create (Object_Generated),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  objects_linked
   Add
     (Create (Objects_Linked),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  target
   Add
     (Create (Target),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Is_Toolchain_Config   => True,
      Inherit_From_Extended => Inherited,
      Default               => Create ("all"));

   --  runtime
   Add
     (Create (Runtime),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  library_builder
   Add
     (Create (Library_Builder),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_support
   Add
     (Create (Library_Support),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_builder
   Add
     (Create (Archive_Builder),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_builder_append_option
   Add
     (Create (Archive_Builder_Append_Option),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_indexer
   Add
     (Create (Archive_Indexer),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  archive_suffix
   Add
     (Create (Archive_Suffix),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create (".a"),
      Inherit_From_Extended => Not_Inherited);

   --  library_partial_linker
   Add
     (Create (Library_Partial_Linker),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  object_lister
   Add
     (Create (Object_Lister),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  object_lister_matcher
   Add
     (Create (Object_Lister_Matcher),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  shared_library_prefix
   Add
     (Create (Shared_Library_Prefix),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create ("lib"),
      Inherit_From_Extended => Not_Inherited);

   --  shared_library_suffix
   Add
     (Create (Shared_Library_Suffix),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Default               => Create (".so"),
      Inherit_From_Extended => Not_Inherited);

   --  symbolic_link_supported
   Add
     (Create (Symbolic_Link_Supported),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_major_minor_id_supported
   Add
     (Create (Library_Major_Minor_Id_Supported),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_auto_init_supported
   Add
     (Create (Library_Auto_Init_Supported),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  shared_library_minimum_switches
   Add
     (Create (Shared_Library_Minimum_Switches),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  library_version_switches
   Add
     (Create (Library_Version_Switches),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  library_install_name_option
   Add
     (Create (Library_Install_Name_Option),
      Index_Type            => No_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  runtime_dir
   Add
     (Create (Runtime_Dir),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  runtime_library_dir
   Add
     (Create (Runtime_Library_Dir),
      Index_Type            => Language_Index,
      Value                 => Single,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Inherit_From_Extended => Not_Inherited);

   --  runtime_source_dir
   Add
     (Create (Runtime_Source_Dir),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  runtime_source_dirs
   Add
     (Create (Runtime_Source_Dirs),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  naming.spec_suffix & specification_suffix
   Add
     (Create (Spec_Suffix, Pack.Naming),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".ads") + Create ("c", ".h"));
   Add_Alias (Create (Spec_Suffix, Pack.Naming),
              Create (Specification_Suffix, Pack.Naming));

   --  naming.body_suffix & implementation_suffix
   Add
     (Create (Body_Suffix, Pack.Naming),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".adb") + Create ("c", ".c"));
   Add_Alias (Create (Body_Suffix, Pack.Naming),
              Create (Implementation_Suffix, Pack.Naming));

   --  naming.separate_suffix
   Add
     (Create (Separate_Suffix, Pack.Naming),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Empty_Value          => Error,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Body_Suffix));

   --  naming.casing
   Add
     (Create (Casing, Pack.Naming),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("lowercase"));

   --  naming.dot_replacement
   Add
     (Create (Dot_Replacement, Pack.Naming),
      Index_Type           => No_Index,
      Empty_Value          => Error,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("-"));

   --  naming.spec & specification
   Add
     (Create (Spec, Pack.Naming),
      Index_Type           => Unit_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);
   Add_Alias (Create (Spec, Pack.Naming),
              Create (Specification, Pack.Naming));

   --  naming.body & implementation
   Add
     (Create (Body_N, Pack.Naming),
      Index_Type           => Unit_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);
   Add_Alias (Create (Body_N, Pack.Naming),
              Create (Implementation, Pack.Naming));

   --  naming.specification_exceptions
   Add
     (Create (Specification_Exceptions, Pack.Naming),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  naming.implementation_exceptions
   Add
     (Create (Implementation_Exceptions, Pack.Naming),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.default_switches
   --  compiler.switches
   Add
     (Create (Switches, Pack.Compiler),
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);
   Add_Alias (Create (Default_Switches, Pack.Compiler),
              Create (Switches, Pack.Compiler));

   --  compiler.local_configuration_pragmas
   Add
     (Create (Local_Configuration_Pragmas, Pack.Compiler),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_config_file
   Add
     (Create (Local_Config_File, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.driver
   Add
     (Create (Driver, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.language_kind
   Add
     (Create (Language_Kind, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_kind
   Add
     (Create (Dependency_Kind, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.required_switches
   Add
     (Create (Required_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.leading_required_switches
   Add
     (Create (Leading_Required_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.trailing_required_switches
   Add
     (Create (Trailing_Required_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.pic_option
   Add
     (Create (Pic_Option, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.path_syntax
   Add
     (Create (Path_Syntax, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.source_file_switches
   Add
     (Create (Source_File_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.object_file_suffix
   Add
     (Create (Object_File_Suffix, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create (".o"));

   --  compiler.object_file_switches
   Add
     (Create (Object_File_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.multi_unit_switches
   Add
     (Create (Multi_Unit_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.multi_unit_object_separator
   Add
     (Create (Multi_Unit_Object_Separator, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_file_switches
   Add
     (Create (Mapping_File_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.mapping_spec_suffix
   Add
     (Create (Mapping_Spec_Suffix, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_body_suffix
   Add
     (Create (Mapping_Body_Suffix, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_switches
   Add
     (Create (Config_File_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.config_body_file_name
   Add
     (Create (Config_Body_File_Name, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_index
   Add
     (Create (Config_Body_File_Name_Index, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_pattern
   Add
     (Create (Config_Body_File_Name_Pattern, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name
   Add
     (Create (Config_Spec_File_Name, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_index
   Add
     (Create (Config_Spec_File_Name_Index, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_pattern
   Add
     (Create (Config_Spec_File_Name_Pattern, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_unique
   Add
     (Create (Config_File_Unique, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_switches
   Add
     (Create (Dependency_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.dependency_driver
   Add
     (Create (Dependency_Driver, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_switches
   Add
     (Create (Include_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.include_path
   Add
     (Create (Include_Path, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_path_file
   Add
     (Create (Include_Path_File, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_path_switches
   Add
     (Create (Object_Path_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  compiler.max_command_line_length
   Add
     (Create (Max_Command_Line_Length, Pack.Compiler),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_format
   Add
     (Create (Response_File_Format, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_switches
   Add
     (Create (Response_File_Switches, Pack.Compiler),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  builder.switches, builder.default_switches
   Add
     (Create (Switches, Pack.Builder),
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);
   Add_Alias (Create (Default_Switches, Pack.Builder),
              Create (Switches, Pack.Builder));

   --  builder.global_compilation_switches
   Add
     (Create (Global_Compilation_Switches, Pack.Builder),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  builder.executable
   Add
     (Create (Executable, Pack.Builder),
      Index_Type           => File_Index,
      Index_Optional       => True,
      Value                => Single,
      Value_Case_Sensitive => True,
      Empty_Value          => Ignore,
      Is_Allowed_In        => No_Aggregates);

   --  builder.executable_suffix
   Add
     (Create (Executable_Suffix, Pack.Builder),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create (GNAT.OS_Lib.Get_Executable_Suffix.all));

   --  builder.global_configuration_pragmas
   Add
     (Create (Global_Configuration_Pragmas, Pack.Builder),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  builder.global_config_file
   Add
     (Create (Global_Config_File, Pack.Builder),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  gnatls.switches
   Add
     (Create (Switches, Pack.Gnatls),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.switches, binder.default_switches
   Add
     (Create (Switches, Pack.Binder),
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);
   Add_Alias (Create (Default_Switches, Pack.Binder),
              Create (Switches, Pack.Binder));

   --  binder.driver
   Add
     (Create (Driver, Pack.Binder),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.required_switches
   Add
     (Create (Required_Switches, Pack.Binder),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  binder.prefix
   Add
     (Create (Prefix, Pack.Binder),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Default              => Create (""));

   --  binder.objects_path
   Add
     (Create (Objects_Path, Pack.Binder),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  binder.objects_path_file
   Add
     (Create (Objects_Path_File, Pack.Binder),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.required_switches
   Add
     (Create (Required_Switches, Pack.Linker),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.leading_switches
   Add
     (Create (Leading_Switches, Pack.Linker),
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.switches, linker.default_switches
   Add
     (Create (Switches, Pack.Linker),
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);
   Add_Alias (Create (Default_Switches, Pack.Linker),
              Create (Switches, Pack.Linker));

   --  linker.trailing_switches
   Add
     (Create (Trailing_Switches, Pack.Linker),
      Index_Type           => FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.linker_options
   Add
     (Create (Linker_Options, Pack.Linker),
      Index_Type            => No_Index,
      Value                 => List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => Everywhere,
      Config_Concatenable   => True,
      Inherit_From_Extended => Not_Inherited);

   --  linker.map_file_option
   Add
     (Create (Map_File_Option, Pack.Linker),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.driver
   Add
     (Create (Driver, Pack.Linker),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.max_command_line_length
   Add
     (Create (Max_Command_Line_Length, Pack.Linker),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_format
   Add
     (Create (Response_File_Format, Pack.Linker),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_switches
   Add
     (Create (Response_File_Switches, Pack.Linker),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.export_file_format
   Add
     (Create (Export_File_Format, Pack.Linker),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_switch
   Add
     (Create (Export_File_Switch, Pack.Linker),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.switches
   Add
     (Create (Switches, Pack.Clean),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  clean.source_artifact_extensions
   Add
     (Create (Source_Artifact_Extensions, Pack.Clean),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.object_artifact_extensions
   Add
     (Create (Object_Artifact_Extensions, Pack.Clean),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_exec_dir
   Add
     (Create (Artifacts_In_Exec_Dir, Pack.Clean),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_object_dir
   Add
     (Create (Artifacts_In_Object_Dir, Pack.Clean),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  cross_reference.default_switches
   Add
     (Create (Default_Switches, Pack.Cross_Reference),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  cross_reference.switches
   Add
     (Create (Switches, Pack.Cross_Reference),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  finder.default_switches
   Add
     (Create (Default_Switches, Pack.Finder),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  finder.switches
   Add
     (Create (Switches, Pack.Finder),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  pretty_printer.default_switches
   Add
     (Create (Default_Switches, Pack.Pretty_Printer),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  pretty_printer.switches
   Add
     (Create (Switches, Pack.Pretty_Printer),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  gnatstub.default_switches
   Add
     (Create (Default_Switches, Pack.Gnatstub),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  gnatstub.switches
   Add
     (Create (Switches, Pack.Gnatstub),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  check.default_switches
   Add
     (Create (Default_Switches, Pack.Check),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  check.switches
   Add
     (Create (Switches, Pack.Check),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  eliminate.default_switches
   Add
     (Create (Default_Switches, Pack.Eliminate),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  eliminate.switches
   Add
     (Create (Switches, Pack.Eliminate),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  metrics.default_switches
   Add
     (Create (Default_Switches, Pack.Metrics),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True);

   --  metrics.switches
   Add
     (Create (Switches, Pack.Metrics),
      Index_Type           => FileGlob_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  ide.default_switches
   Add
     (Create (Default_Switches, Pack.Ide),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => No_Aggregates);

   --  ide.remote_host
   Add
     (Create (Remote_Host, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.program_host
   Add
     (Create (Program_Host, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.communication_protocol
   Add
     (Create (Communication_Protocol, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.compiler_command
   Add
     (Create (Compiler_Command, Pack.Ide),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.debugger_command
   Add
     (Create (Debugger_Command, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.gnatlist
   Add
     (Create (Gnatlist, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_kind
   Add
     (Create (Vcs_Kind, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_file_check
   Add
     (Create (Vcs_File_Check, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_log_check
   Add
     (Create (Vcs_Log_Check, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  ide.documentation_dir
   Add
     (Create (Documentation_Dir, Pack.Ide),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.prefix
   Add
     (Create (Prefix, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.sources_subdir
   Add
     (Create (Sources_Subdir, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.exec_subdir
   Add
     (Create (Exec_Subdir, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Add
     (Create (Lib_Subdir, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Add
     (Create (ALI_Subdir, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.project_subdir
   Add
     (Create (Project_Subdir, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.active
   Add
     (Create (Active, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.artifacts
   Add
     (Create (Artifacts, Pack.Install),
      Index_Type           => File_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.required_artifacts
   Add
     (Create (Required_Artifacts, Pack.Install),
      Index_Type           => File_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.mode
   Add
     (Create (Mode, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install.install_name
   Add
     (Create (Install_Name, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.root_dir
   Add
     (Create (Root_Dir, Pack.Remote),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.excluded_patterns
   Add
     (Create (Excluded_Patterns, Pack.Remote),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.included_patterns
   Add
     (Create (Included_Patterns, Pack.Remote),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  remote.included_artifact_patterns
   Add
     (Create (Included_Artifact_Patterns, Pack.Remote),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  stack.switches
   Add
     (Create (Switches, Pack.Stack),
      Index_Type           => No_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  origin_project
   Add
     (Create (Origin_Project),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install'side_debug
   Add
     (Create (Side_Debug, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => Everywhere);

   --  include_switches_via_spec
   Add
     (Create (Include_Switches_Via_Spec),
      Index_Type           => Language_Index,
      Value                => List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  only_dirs_with_sources
   Add
     (Create (Only_Dirs_With_Sources),
      Index_Type           => Language_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  warning_message
   Add
     (Create (Warning_Message),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  canonical_target
   Add
     (Create (Canonical_Target),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  create_missing_dirs
   Add
     (Create (Create_Missing_Dirs),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => Everywhere);

   --  install_project
   Add
     (Create (Install_Project, Pack.Install),
      Index_Type           => No_Index,
      Value                => Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => Everywhere);

end GPR2.Project.Registry.Attribute;
