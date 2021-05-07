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

with Ada.Strings.Fixed;
with GPR2.Project.Registry.Pack;

package body GPR2.Project.Registry.Attribute is

   package Pack_Defaults is new Ada.Containers.Indefinite_Ordered_Maps
     (Optional_Name_Type, Default_References.Map,
      "=" => Default_References."=");

   Any_Index : constant Value_Type := (1 => ASCII.NUL);
   --  Internal index declaring that it is fit for any index request

   Attribute_Delimiter : constant String := "'";

   Store    : Attribute_Definitions.Map;
   Defaults : Pack_Defaults.Map;

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

   function Create (Index, Value : Value_Type) return VSR.Map;
   --  Create container for attribute default value

   function Create (Value : Value_Type) return VSR.Map is
     (Create (Any_Index, Value));
   --  Create container for attribute default value

   function "+" (Left, Right : VSR.Map) return VSR.Map;
   --  Concatenate 2 default values for different indexes into one container

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : VSR.Map) return VSR.Map is
      Result : VSR.Map := Left;
   begin
      for C in Right.Iterate loop
         Result.Insert (VSR.Key (C), Right (C));
      end loop;

      return Result;
   end "+";

   ---------
   -- Add --
   ---------

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
      Is_Toolchain_Config  : Boolean            := False;
      Config_Concatenable  : Boolean            := False;
      Index_Type           : Index_Value_Type   := Name_Index)
   is
      procedure Index_Default;
      --  Save definition with default value to Defaults index

      -------------------
      -- Index_Default --
      -------------------

      procedure Index_Default is
         Del_At : constant Natural :=
                    Ada.Strings.Fixed.Index
                      (String (Name), Attribute_Delimiter);
         Pack   : constant Optional_Name_Type :=
                    (if Del_At = 0
                     then No_Name
                     else Name_Type (Name (Name'First .. Del_At - 1)));
         Attr   : constant Name_Type :=
                    Name_Type
                      (if Del_At = 0
                       then Name
                       else Name (Del_At + 1 .. Name'Last));
         CP     : Pack_Defaults.Cursor;
         OK     : Boolean;
      begin
         Defaults.Insert (Pack, Default_References.Empty_Map, CP, OK);
         Defaults (CP).Insert (Attr, Store (Name).Element);
      end Index_Default;

   begin
      Store.Insert
        (Name,
         Def'(Index                => Index,
              Others_Allowed       => Others_Allowed,
              Index_Case_Sensitive => Index_Case_Sensitive,
              Value                => Value,
              Value_Case_Sensitive => Value_Case_Sensitive,
              Empty_Value          => Empty_Value,
              Read_Only            => Read_Only,
              Is_Allowed_In        => Is_Allowed_In,
              Default              => Default,
              Default_Is_Reference => Default_Is_Reference,
              Has_Default_In       => (if Has_Default_In = Nowhere
                                       then Is_Allowed_In
                                       else Has_Default_In),
              Is_Toolchain_Config  => Is_Toolchain_Config,
              Config_Concatenable  => Config_Concatenable,
              Index_Type           => Index_Type));

      if not Default.Is_Empty then
         Index_Default;
      end if;
   end Add;

   ------------
   -- Create --
   ------------

   function Create
     (Name : Name_Type;
      Pack : Optional_Name_Type := No_Name) return Qualified_Name is
   begin
      return Qualified_Name
        (if Pack = No_Name
         then Name
         else Pack & Attribute_Delimiter (1) & Name);
   end Create;

   function Create (Index, Value : Value_Type) return VSR.Map is
      Result : VSR.Map;
   begin
      Result.Insert (Name_Type (Index), Value);

      return Result;
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists (Q_Name : Qualified_Name) return Boolean is
   begin
      return Store.Contains (Q_Name);
   end Exists;

   ----------------------
   -- For_Each_Default --
   ----------------------

   procedure For_Each_Default
     (Rules  : Default_Rules;
      Action : not null access procedure
                 (Attribute : Name_Type; Definition : Def))
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

   function Get (Q_Name : Qualified_Name) return Def is
   begin
      return Store (Q_Name);
   end Get;

   -----------------------
   -- Get_Default_Rules --
   -----------------------

   function Get_Default_Rules
     (Pack : Optional_Name_Type) return Default_Rules
   is
      CR : constant Pack_Defaults.Cursor := Defaults.Find (Pack);
   begin
      if Pack_Defaults.Has_Element (CR) then
         return Defaults (CR).Element;
      else
         return Default_References.Empty_Map'Unrestricted_Access;
      end if;
   end Get_Default_Rules;

begin
   --  name
   Add
     (Create (Name),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => True,
      Is_Allowed_In        => Everywhere);

   --  project_dir
   Add
     (Create (Project_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => True,
      Is_Allowed_In        => Everywhere);

   --  main
   Add
     (Create (Main),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  languages
   Add
     (Create (Languages),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create ("Ada"));

   --  roots
   Add
     (Create (Roots),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Index_Type           => FileGlob_Or_Language_Index);

   --  externally_built
   Add
     (Create (Externally_Built),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  object_dir
   Add
     (Create (Object_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("."));

   --  exec_dir
   Add
     (Create (Exec_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create (Value_Type (Object_Dir)),
      Default_Is_Reference => True);

   --  source_dirs
   Add
     (Create (Source_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create ("."),
      Has_Default_In       => No_Aggregates_Abstract);

   --  inherit_source_path
   Add
     (Create (Inherit_Source_Path),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Index_Type           => Language_Index);

   --  excluded_source_dirs
   Add
     (Create (Excluded_Source_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ignore_source_sub_dirs
   Add
     (Create (Ignore_Source_Sub_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  source_files
   Add
     (Create (Source_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  locally_removed_files
   Add
     (Create (Locally_Removed_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_files
   Add
     (Create (Excluded_Source_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Default              => Create (Value_Type (Locally_Removed_Files)),
      Default_Is_Reference => True);

   --  source_list_file
   Add
     (Create (Source_List_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_list_file
   Add
     (Create (Excluded_Source_List_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  interfaces
   Add
     (Create (Interfaces),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  project_files
   Add
     (Create (Project_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Aggregates);

   --  project_path
   Add
     (Create (Project_Path),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Aggregates);

   --  external
   Add
     (Create (External),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => (K_Aggregate => True, others => False),
      Index_Type           => Name_Index);

   --  library_dir
   Add
     (Create (Library_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_name
   Add
     (Create (Library_Name),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_kind
   Add
     (Create (Library_Kind),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_version
   Add
     (Create (Library_Version),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_interface
   Add
     (Create (Library_Interface),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_standalone
   Add
     (Create (Library_Standalone),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_encapsulated_options
   Add
     (Create (Library_Encapsulated_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  library_encapsulated_supported
   Add
     (Create (Library_Encapsulated_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Configuration,
      Config_Concatenable  => True);

   --  library_auto_init
   Add
     (Create (Library_Auto_Init),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  leading_library_options
   Add
     (Create (Leading_Library_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  library_options
   Add
     (Create (Library_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library,
      Config_Concatenable  => True);

   --  library_rpath_options
   Add
     (Create (Library_Rpath_Options),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  library_src_dir
   Add
     (Create (Library_Src_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library,
      Default              => Create (Value_Type (Library_Dir)),
      Default_Is_Reference => True);

   --  library_ali_dir
   Add
     (Create (Library_Ali_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library,
      Default              => Create (Value_Type (Library_Dir)),
      Default_Is_Reference => True);

   --  library_gcc
   Add
     (Create (Library_Gcc),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_symbol_file
   Add
     (Create (Library_Symbol_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_symbol_policy
   Add
     (Create (Library_Symbol_Policy),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_reference_symbol_file
   Add
     (Create (Library_Reference_Symbol_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  default_language
   Add
     (Create (Default_Language),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  run_path_option
   Add
     (Create (Run_Path_Option),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  run_path_origin
   Add
     (Create (Run_Path_Origin),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  separate_run_path_options
   Add
     (Create (Separate_Run_Path_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  toolchain_version
   Add
     (Create (Toolchain_Version),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True,
      Index_Type           => Language_Index);

   --  toolchain_name
   Add
     (Create (Toolchain_Name),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True,
      Index_Type           => Language_Index);

   --  toolchain_path
   Add
     (Create (Toolchain_Path),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True,
      Index_Type           => Language_Index);

   --  required_toolchain_version
   Add
     (Create (Required_Toolchain_Version),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True,
      Index_Type           => Language_Index);

   --  toolchain_description
   Add
     (Create (Toolchain_Description),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  object_generated
   Add
     (Create (Object_Generated),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  objects_linked
   Add
     (Create (Objects_Linked),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  target
   Add
     (Create (Target),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  runtime
   Add
     (Create (Runtime),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True,
      Index_Type           => Language_Index);

   --  library_builder
   Add
     (Create (Library_Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_support
   Add
     (Create (Library_Support),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_builder
   Add
     (Create (Archive_Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_builder_append_option
   Add
     (Create (Archive_Builder_Append_Option),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_indexer
   Add
     (Create (Archive_Indexer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_suffix
   Add
     (Create (Archive_Suffix),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (".a"));

   --  library_partial_linker
   Add
     (Create (Library_Partial_Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_lister
   Add
     (Create (Object_Lister),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_lister_matcher
   Add
     (Create (Object_Lister_Matcher),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_prefix
   Add
     (Create (Shared_Library_Prefix),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("lib"));

   --  shared_library_suffix
   Add
     (Create (Shared_Library_Suffix),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (".so"));

   --  symbolic_link_supported
   Add
     (Create (Symbolic_Link_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_major_minor_id_supported
   Add
     (Create (Library_Major_Minor_Id_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_auto_init_supported
   Add
     (Create (Library_Auto_Init_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_minimum_switches
   Add
     (Create (Shared_Library_Minimum_Switches),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_version_switches
   Add
     (Create (Library_Version_Switches),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  library_install_name_option
   Add
     (Create (Library_Install_Name_Option),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_dir
   Add
     (Create (Runtime_Dir),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  runtime_library_dir
   Add
     (Create (Runtime_Library_Dir),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  runtime_source_dir
   Add
     (Create (Runtime_Source_Dir),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  runtime_source_dirs
   Add
     (Create (Runtime_Source_Dirs),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  naming.spec_suffix
   Add
     (Create (Spec_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Specification_Suffix)),
      Default_Is_Reference => True,
      Index_Type           => Language_Index);

   --  naming.body_suffix
   Add
     (Create (Body_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Implementation_Suffix)),
      Default_Is_Reference => True,
      Index_Type           => Language_Index);

   --  naming.specification_suffix
   Add
     (Create (Specification_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".ads") + Create ("c", ".h"),
      Index_Type           => Language_Index);

   --  naming.implementation_suffix
   Add
     (Create (Implementation_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".adb") + Create ("c", ".c"),
      Index_Type           => Language_Index);

   --  naming.separate_suffix
   Add
     (Create (Separate_Suffix, Pack.Naming),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", Value_Type (Body_Suffix)),
      Default_Is_Reference => True);

   --  naming.casing
   Add
     (Create (Casing, Pack.Naming),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("lowercase"));

   --  naming.dot_replacement
   Add
     (Create (Dot_Replacement, Pack.Naming),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("-"));

   --  naming.spec
   Add
     (Create (Spec, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Specification)),
      Default_Is_Reference => True,
      Index_Type           => Name_Index);

   --  naming.specification
   Add
     (Create (Specification, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Name_Index);

   --  naming.body
   Add
     (Create (Body_N, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Implementation)),
      Default_Is_Reference => True,
      Index_Type           => Name_Index);

   --  naming.implementation
   Add
     (Create (Implementation, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Name_Index);

   --  naming.specification_exceptions
   Add
     (Create (Specification_Exceptions, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  naming.implementation_exceptions
   Add
     (Create (Implementation_Exceptions, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.default_switches
   Add
     (Create (Default_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.switches
   Add
     (Create (Switches, Pack.Compiler),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  compiler.local_configuration_pragmas
   Add
     (Create (Local_Configuration_Pragmas, Pack.Compiler),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_config_file
   Add
     (Create (Local_Config_File, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.driver
   Add
     (Create (Driver, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.language_kind
   Add
     (Create (Language_Kind, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.dependency_kind
   Add
     (Create (Dependency_Kind, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.required_switches
   Add
     (Create (Required_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.leading_required_switches
   Add
     (Create (Leading_Required_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.trailing_required_switches
   Add
     (Create (Trailing_Required_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.pic_option
   Add
     (Create (Pic_Option, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.path_syntax
   Add
     (Create (Path_Syntax, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.source_file_switches
   Add
     (Create (Source_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.object_file_suffix
   Add
     (Create (Object_File_Suffix, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (".o"),
      Index_Type           => Language_Index);

   --  compiler.object_file_switches
   Add
     (Create (Object_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.multi_unit_switches
   Add
     (Create (Multi_Unit_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.multi_unit_object_separator
   Add
     (Create (Multi_Unit_Object_Separator, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.mapping_file_switches
   Add
     (Create (Mapping_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.mapping_spec_suffix
   Add
     (Create (Mapping_Spec_Suffix, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.mapping_body_suffix
   Add
     (Create (Mapping_Body_Suffix, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_file_switches
   Add
     (Create (Config_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.config_body_file_name
   Add
     (Create (Config_Body_File_Name, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_body_file_name_index
   Add
     (Create (Config_Body_File_Name_Index, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_body_file_name_pattern
   Add
     (Create (Config_Body_File_Name_Pattern, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_spec_file_name
   Add
     (Create (Config_Spec_File_Name, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_spec_file_name_index
   Add
     (Create (Config_Spec_File_Name_Index, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_spec_file_name_pattern
   Add
     (Create (Config_Spec_File_Name_Pattern, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.config_file_unique
   Add
     (Create (Config_File_Unique, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.dependency_switches
   Add
     (Create (Dependency_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.dependency_driver
   Add
     (Create (Dependency_Driver, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.include_switches
   Add
     (Create (Include_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.include_path
   Add
     (Create (Include_Path, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.include_path_file
   Add
     (Create (Include_Path_File, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.object_path_switches
   Add
     (Create (Object_Path_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  compiler.max_command_line_length
   Add
     (Create (Max_Command_Line_Length, Pack.Compiler),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_format
   Add
     (Create (Response_File_Format, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  compiler.response_file_switches
   Add
     (Create (Response_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  builder.default_switches
   Add
     (Create (Default_Switches, Pack.Builder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  builder.switches
   Add
     (Create (Switches, Pack.Builder),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  builder.global_compilation_switches
   Add
     (Create (Global_Compilation_Switches, Pack.Builder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  builder.executable
   Add
     (Create (Executable, Pack.Builder),
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => Single,
      Value_Case_Sensitive => True,
      Empty_Value          => Ignore,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Index_Type           => File_Index);

   --  builder.executable_suffix
   Add
     (Create (Executable_Suffix, Pack.Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.global_configuration_pragmas
   Add
     (Create (Global_Configuration_Pragmas, Pack.Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.global_config_file
   Add
     (Create (Global_Config_File, Pack.Builder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  gnatls.switches
   Add
     (Create (Switches, Pack.Gnatls),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.default_switches
   Add
     (Create (Default_Switches, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  binder.switches
   Add
     (Create (Switches, Pack.Binder),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  binder.driver
   Add
     (Create (Driver, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  binder.required_switches
   Add
     (Create (Required_Switches, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  binder.prefix
   Add
     (Create (Prefix, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  binder.objects_path
   Add
     (Create (Objects_Path, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  binder.objects_path_file
   Add
     (Create (Objects_Path_File, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  linker.required_switches
   Add
     (Create (Required_Switches, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.default_switches
   Add
     (Create (Default_Switches, Pack.Linker),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  linker.leading_switches
   Add
     (Create (Leading_Switches, Pack.Linker),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  linker.switches
   Add
     (Create (Switches, Pack.Linker),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  linker.trailing_switches
   Add
     (Create (Trailing_Switches, Pack.Linker),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => FileGlob_Or_Language_Index);

   --  linker.linker_options
   Add
     (Create (Linker_Options, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.map_file_option
   Add
     (Create (Map_File_Option, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.driver
   Add
     (Create (Driver, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.max_command_line_length
   Add
     (Create (Max_Command_Line_Length, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_format
   Add
     (Create (Response_File_Format, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_switches
   Add
     (Create (Response_File_Switches, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  linker.export_file_format
   Add
     (Create (Export_File_Format, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_switch
   Add
     (Create (Export_File_Switch, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.switches
   Add
     (Create (Switches, Pack.Clean),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  clean.source_artifact_extensions
   Add
     (Create (Source_Artifact_Extensions, Pack.Clean),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  clean.object_artifact_extensions
   Add
     (Create (Object_Artifact_Extensions, Pack.Clean),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => Language_Index);

   --  clean.artifacts_in_exec_dir
   Add
     (Create (Artifacts_In_Exec_Dir, Pack.Clean),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_object_dir
   Add
     (Create (Artifacts_In_Object_Dir, Pack.Clean),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  cross_reference.default_switches
   Add
     (Create (Default_Switches, Pack.Cross_Reference),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  cross_reference.switches
   Add
     (Create (Switches, Pack.Cross_Reference),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  finder.default_switches
   Add
     (Create (Default_Switches, Pack.Finder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  finder.switches
   Add
     (Create (Switches, Pack.Finder),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  pretty_printer.default_switches
   Add
     (Create (Default_Switches, Pack.Pretty_Printer),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  pretty_printer.switches
   Add
     (Create (Switches, Pack.Pretty_Printer),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  gnatstub.default_switches
   Add
     (Create (Default_Switches, Pack.Gnatstub),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  gnatstub.switches
   Add
     (Create (Switches, Pack.Gnatstub),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  check.default_switches
   Add
     (Create (Default_Switches, Pack.Check),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  check.switches
   Add
     (Create (Switches, Pack.Check),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  eliminate.default_switches
   Add
     (Create (Default_Switches, Pack.Eliminate),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  eliminate.switches
   Add
     (Create (Switches, Pack.Eliminate),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  metrics.default_switches
   Add
     (Create (Default_Switches, Pack.Metrics),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates,
      Config_Concatenable  => True,
      Index_Type           => Language_Index);

   --  metrics.switches
   Add
     (Create (Switches, Pack.Metrics),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True,
      Index_Type           => FileGlob_Or_Language_Index);

   --  ide.default_switches
   Add
     (Create (Default_Switches, Pack.Ide),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  ide.remote_host
   Add
     (Create (Remote_Host, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.program_host
   Add
     (Create (Program_Host, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.communication_protocol
   Add
     (Create (Communication_Protocol, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.compiler_command
   Add
     (Create (Compiler_Command, Pack.Ide),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.debugger_command
   Add
     (Create (Debugger_Command, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.gnatlist
   Add
     (Create (Gnatlist, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_kind
   Add
     (Create (Vcs_Kind, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_file_check
   Add
     (Create (Vcs_File_Check, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_log_check
   Add
     (Create (Vcs_Log_Check, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.documentation_dir
   Add
     (Create (Documentation_Dir, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.prefix
   Add
     (Create (Prefix, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.sources_subdir
   Add
     (Create (Sources_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.exec_subdir
   Add
     (Create (Exec_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Add
     (Create (Lib_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Add
     (Create (ALI_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.project_subdir
   Add
     (Create (Project_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.active
   Add
     (Create (Active, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.artifacts
   Add
     (Create (Artifacts, Pack.Install),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => File_Index);

   --  install.required_artifacts
   Add
     (Create (Required_Artifacts, Pack.Install),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Index_Type           => File_Index);

   --  install.mode
   Add
     (Create (Mode, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.install_name
   Add
     (Create (Install_Name, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.root_dir
   Add
     (Create (Root_Dir, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.excluded_patterns
   Add
     (Create (Excluded_Patterns, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.included_patterns
   Add
     (Create (Included_Patterns, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.included_artifact_patterns
   Add
     (Create (Included_Artifact_Patterns, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  stack.switches
   Add
     (Create (Switches, Pack.Stack),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Config_Concatenable  => True);

   --  origin_project
   Add
     (Create (Origin_Project),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  side_debug
   Add
     (Create (Side_Debug, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  include_switches_via_spec
   Add
     (Create (Include_Switches_Via_Spec),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  only_dirs_with_sources
   Add
     (Create (Only_Dirs_With_Sources),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  warning_message
   Add
     (Create (Warning_Message),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  canonical_target
   Add
     (Create (Canonical_Target),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Is_Toolchain_Config  => True);

   --  create_missing_dirs
   Add
     (Create (Create_Missing_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install_project
   Add
     (Create (Install_Project, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

end GPR2.Project.Registry.Attribute;
