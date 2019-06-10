------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with GPR2.Project.Registry.Pack;
with GPR2.Source_Reference.Value;

package body GPR2.Project.Registry.Attribute is

   package Pack_Defaults is new Ada.Containers.Indefinite_Ordered_Maps
     (Optional_Name_Type, Default_References.Map,
      "=" => Default_References."=");

   Store    : Attribute_Definitions.Map;
   Defaults : Pack_Defaults.Map;

   Everywhere       : constant Allowed_In := (others => True);

   Nowhere          : constant Allowed_In := (others => False);

   In_Library       : constant Allowed_In :=
                        (K_Library | K_Aggregate_Library => True,
                         others                          => False);

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

   procedure Store_Insert
     (Name                 : Qualified_Name;
      Index                : Index_Kind;
      Others_Allowed       : Boolean;
      Index_Case_Sensitive : Boolean;
      Value                : Value_Kind;
      Value_Case_Sensitive : Boolean;
      Empty_Value          : Empty_Value_Status := Allow;
      Read_Only            : Boolean;
      Is_Allowed_In        : Allowed_In;
      Default              : VSR.Map    := VSR.Empty_Map;
      Default_Is_Reference : Boolean    := False;
      Has_Default_In       : Allowed_In := Nowhere);
   --  Calls Store.Insert with Key => Name and Value created from other fields

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
         else Name_Type (Pack) & '.' & Name);
   end Create;

   function Create (Index, Value : Value_Type) return VSR.Map is
      Result : VSR.Map;
   begin
      Result.Insert
        (Index,
         Source_Reference.Value.Object
           (Source_Reference.Value.Create (Source_Reference.Builtin, Value)));

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

   ------------------
   -- Store_Insert --
   ------------------

   procedure Store_Insert
     (Name                 : Qualified_Name;
      Index                : Index_Kind;
      Others_Allowed       : Boolean;
      Index_Case_Sensitive : Boolean;
      Value                : Value_Kind;
      Value_Case_Sensitive : Boolean;
      Empty_Value          : Empty_Value_Status := Allow;
      Read_Only            : Boolean;
      Is_Allowed_In        : Allowed_In;
      Default              : VSR.Map    := VSR.Empty_Map;
      Default_Is_Reference : Boolean    := False;
      Has_Default_In       : Allowed_In := Nowhere)
   is
      procedure Index_Default;
      --  Save definnition with default value to Defaults index

      -------------------
      -- Index_Default --
      -------------------

      procedure Index_Default is
         Dot_At : constant Natural :=
                    Ada.Strings.Fixed.Index (String (Name), ".");
         Pack   : constant Optional_Name_Type :=
                    (if Dot_At = 0
                     then No_Name
                     else Name_Type (Name (Name'First .. Dot_At - 1)));
         Attr   : constant Name_Type :=
                    Name_Type
                      (if Dot_At = 0
                       then Name
                       else Name (Dot_At + 1 .. Name'Last));
         CP     : Pack_Defaults.Cursor := Defaults.Find (Pack);
         OK     : Boolean;
      begin
         if not Pack_Defaults.Has_Element (CP) then
            Defaults.Insert (Pack, Default_References.Empty_Map, CP, OK);
            pragma Assert (OK);
         end if;

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
                                       else Has_Default_In)));

      if not Default.Is_Empty then
         Index_Default;
      end if;
   end Store_Insert;

begin
   --  name
   Store_Insert
     (Create (Name),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => True,
      Is_Allowed_In        => Everywhere);

   --  project_dir
   Store_Insert
     (Create (Project_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => True,
      Is_Allowed_In        => Everywhere);

   --  main
   Store_Insert
     (Create (Main),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  languages
   Store_Insert
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
   Store_Insert
     (Create (Roots),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  externally_built
   Store_Insert
     (Create (Externally_Built),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  object_dir
   Store_Insert
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
   Store_Insert
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
   Store_Insert
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
   Store_Insert
     (Create (Inherit_Source_Path),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_dirs
   Store_Insert
     (Create (Excluded_Source_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ignore_source_sub_dirs
   Store_Insert
     (Create (Ignore_Source_Sub_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  source_files
   Store_Insert
     (Create (Source_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  locally_removed_files
   Store_Insert
     (Create (Locally_Removed_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_files
   Store_Insert
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
   Store_Insert
     (Create (Source_List_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_list_file
   Store_Insert
     (Create (Excluded_Source_List_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  interfaces
   Store_Insert
     (Create (Interfaces),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  project_files
   Store_Insert
     (Create (Project_Files),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Aggregates);

   --  project_path
   Store_Insert
     (Create (Project_Path),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Aggregates);

   --  external
   Store_Insert
     (Create (External),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Aggregates);

   --  library_dir
   Store_Insert
     (Create (Library_Dir),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_name
   Store_Insert
     (Create (Library_Name),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_kind
   Store_Insert
     (Create (Library_Kind),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_version
   Store_Insert
     (Create (Library_Version),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_interface
   Store_Insert
     (Create (Library_Interface),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_standalone
   Store_Insert
     (Create (Library_Standalone),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => In_Library,
      Default              => Create ("standard"));

   --  library_encapsulated_options
   Store_Insert
     (Create (Library_Encapsulated_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_encapsulated_supported
   Store_Insert
     (Create (Library_Encapsulated_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Configuration);

   --  library_auto_init
   Store_Insert
     (Create (Library_Auto_Init),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  leading_library_options
   Store_Insert
     (Create (Leading_Library_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_options
   Store_Insert
     (Create (Library_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_rpath_options
   Store_Insert
     (Create (Library_Rpath_Options),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_src_dir
   Store_Insert
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
   Store_Insert
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
   Store_Insert
     (Create (Library_Gcc),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_symbol_file
   Store_Insert
     (Create (Library_Symbol_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_symbol_policy
   Store_Insert
     (Create (Library_Symbol_Policy),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_reference_symbol_file
   Store_Insert
     (Create (Library_Reference_Symbol_File),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  default_language
   Store_Insert
     (Create (Default_Language),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  run_path_option
   Store_Insert
     (Create (Run_Path_Option),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  run_path_origin
   Store_Insert
     (Create (Run_Path_Origin),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  separate_run_path_options
   Store_Insert
     (Create (Separate_Run_Path_Options),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  toolchain_version
   Store_Insert
     (Create (Toolchain_Version),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  toolchain_name
   Store_Insert
     (Create (Toolchain_Name),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  toolchain_description
   Store_Insert
     (Create (Toolchain_Description),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_generated
   Store_Insert
     (Create (Object_Generated),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  objects_linked
   Store_Insert
     (Create (Objects_Linked),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  target
   Store_Insert
     (Create (Target),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime
   Store_Insert
     (Create (Runtime),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_builder
   Store_Insert
     (Create (Library_Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_support
   Store_Insert
     (Create (Library_Support),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_builder
   Store_Insert
     (Create (Archive_Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_builder_append_option
   Store_Insert
     (Create (Archive_Builder_Append_Option),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_indexer
   Store_Insert
     (Create (Archive_Indexer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_suffix
   Store_Insert
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
   Store_Insert
     (Create (Library_Partial_Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_lister
   Store_Insert
     (Create (Object_Lister),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_lister_matcher
   Store_Insert
     (Create (Object_Lister_Matcher),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_prefix
   Store_Insert
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
   Store_Insert
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
   Store_Insert
     (Create (Symbolic_Link_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_major_minor_id_supported
   Store_Insert
     (Create (Library_Major_Minor_Id_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_auto_init_supported
   Store_Insert
     (Create (Library_Auto_Init_Supported),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_minimum_switches
   Store_Insert
     (Create (Shared_Library_Minimum_Switches),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_version_switches
   Store_Insert
     (Create (Library_Version_Switches),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_install_name_option
   Store_Insert
     (Create (Library_Install_Name_Option),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_dir
   Store_Insert
     (Create (Runtime_Dir),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_library_dir
   Store_Insert
     (Create (Runtime_Library_Dir),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_source_dir
   Store_Insert
     (Create (Runtime_Source_Dir),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_source_dirs
   Store_Insert
     (Create (Runtime_Source_Dirs),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.spec_suffix
   Store_Insert
     (Create (Spec_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Specification_Suffix)),
      Default_Is_Reference => True);

   --  naming.body_suffix
   Store_Insert
     (Create (Body_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Implementation_Suffix)),
      Default_Is_Reference => True);

   --  naming.specification_suffix
   Store_Insert
     (Create (Specification_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".ads") + Create ("c", ".h"));

   --  naming.implementation_suffix
   Store_Insert
     (Create (Implementation_Suffix, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create ("ada", ".adb") + Create ("c", ".c"));

   --  naming.separate_suffix
   Store_Insert
     (Create (Separate_Suffix, Pack.Naming),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Body_Suffix)),
      Default_Is_Reference => True);

   --  naming.casing
   Store_Insert
     (Create (Casing, Pack.Naming),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.dot_replacement
   Store_Insert
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
   Store_Insert
     (Create (Spec, Pack.Naming),
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Specification)),
      Default_Is_Reference => True);

   --  naming.specification
   Store_Insert
     (Create (Specification, Pack.Naming),
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.body
   Store_Insert
     (Create (Body_N, Pack.Naming),
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (Value_Type (Implementation)),
      Default_Is_Reference => True);

   --  naming.implementation
   Store_Insert
     (Create (Implementation, Pack.Naming),
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.specification_exceptions
   Store_Insert
     (Create (Specification_Exceptions, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.implementation_exceptions
   Store_Insert
     (Create (Implementation_Exceptions, Pack.Naming),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  compiler.switches
   Store_Insert
     (Create (Switches, Pack.Compiler),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_configuration_pragmas
   Store_Insert
     (Create (Local_Configuration_Pragmas, Pack.Compiler),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_config_file
   Store_Insert
     (Create (Local_Config_File, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.driver
   Store_Insert
     (Create (Driver, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.language_kind
   Store_Insert
     (Create (Language_Kind, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_kind
   Store_Insert
     (Create (Dependency_Kind, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.required_switches
   Store_Insert
     (Create (Required_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.leading_required_switches
   Store_Insert
     (Create (Leading_Required_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.trailing_required_switches
   Store_Insert
     (Create (Trailing_Required_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.pic_option
   Store_Insert
     (Create (Pic_Option, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.path_syntax
   Store_Insert
     (Create (Path_Syntax, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.source_file_switches
   Store_Insert
     (Create (Source_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_file_suffix
   Store_Insert
     (Create (Object_File_Suffix, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere,
      Default              => Create (".o"));

   --  compiler.object_file_switches
   Store_Insert
     (Create (Object_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.multi_unit_switches
   Store_Insert
     (Create (Multi_Unit_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.multi_unit_object_separator
   Store_Insert
     (Create (Multi_Unit_Object_Separator, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_file_switches
   Store_Insert
     (Create (Mapping_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_spec_suffix
   Store_Insert
     (Create (Mapping_Spec_Suffix, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_body_suffix
   Store_Insert
     (Create (Mapping_Body_Suffix, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_switches
   Store_Insert
     (Create (Config_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name
   Store_Insert
     (Create (Config_Body_File_Name, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_index
   Store_Insert
     (Create (Config_Body_File_Name_Index, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_pattern
   Store_Insert
     (Create (Config_Body_File_Name_Pattern, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name
   Store_Insert
     (Create (Config_Spec_File_Name, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_index
   Store_Insert
     (Create (Config_Spec_File_Name_Index, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_pattern
   Store_Insert
     (Create (Config_Spec_File_Name_Pattern, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_unique
   Store_Insert
     (Create (Config_File_Unique, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_switches
   Store_Insert
     (Create (Dependency_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_driver
   Store_Insert
     (Create (Dependency_Driver, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_switches
   Store_Insert
     (Create (Include_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_path
   Store_Insert
     (Create (Include_Path, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_path_file
   Store_Insert
     (Create (Include_Path_File, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_path_switches
   Store_Insert
     (Create (Object_Path_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.max_command_line_length
   Store_Insert
     (Create (Max_Command_Line_Length, Pack.Compiler),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_format
   Store_Insert
     (Create (Response_File_Format, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_switches
   Store_Insert
     (Create (Response_File_Switches, Pack.Compiler),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Builder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.switches
   Store_Insert
     (Create (Switches, Pack.Builder),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.global_compilation_switches
   Store_Insert
     (Create (Global_Compilation_Switches, Pack.Builder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.executable
   Store_Insert
     (Create (Executable, Pack.Builder),
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => Single,
      Value_Case_Sensitive => True,
      Empty_Value          => Ignore,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.executable_suffix
   Store_Insert
     (Create (Executable_Suffix, Pack.Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.global_configuration_pragmas
   Store_Insert
     (Create (Global_Configuration_Pragmas, Pack.Builder),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.global_config_file
   Store_Insert
     (Create (Global_Config_File, Pack.Builder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  gnatls.switches
   Store_Insert
     (Create (Switches, Pack.Gnatls),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  binder.switches
   Store_Insert
     (Create (Switches, Pack.Binder),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.driver
   Store_Insert
     (Create (Driver, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.required_switches
   Store_Insert
     (Create (Required_Switches, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.prefix
   Store_Insert
     (Create (Prefix, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.objects_path
   Store_Insert
     (Create (Objects_Path, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.objects_path_file
   Store_Insert
     (Create (Objects_Path_File, Pack.Binder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.required_switches
   Store_Insert
     (Create (Required_Switches, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Linker),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  linker.leading_switches
   Store_Insert
     (Create (Leading_Switches, Pack.Linker),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.switches
   Store_Insert
     (Create (Switches, Pack.Linker),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.trailing_switches
   Store_Insert
     (Create (Trailing_Switches, Pack.Linker),
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.linker_options
   Store_Insert
     (Create (Linker_Options, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.map_file_option
   Store_Insert
     (Create (Map_File_Option, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.driver
   Store_Insert
     (Create (Driver, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.max_command_line_length
   Store_Insert
     (Create (Max_Command_Line_Length, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_format
   Store_Insert
     (Create (Response_File_Format, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_switches
   Store_Insert
     (Create (Response_File_Switches, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_format
   Store_Insert
     (Create (Export_File_Format, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_switch
   Store_Insert
     (Create (Export_File_Switch, Pack.Linker),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.switches
   Store_Insert
     (Create (Switches, Pack.Clean),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.source_artifact_extensions
   Store_Insert
     (Create (Source_Artifact_Extensions, Pack.Clean),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.object_artifact_extensions
   Store_Insert
     (Create (Object_Artifact_Extensions, Pack.Clean),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_exec_dir
   Store_Insert
     (Create (Artifacts_In_Exec_Dir, Pack.Clean),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_object_dir
   Store_Insert
     (Create (Artifacts_In_Object_Dir, Pack.Clean),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  cross_reference.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Cross_Reference),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  cross_reference.switches
   Store_Insert
     (Create (Switches, Pack.Cross_Reference),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  finder.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Finder),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  finder.switches
   Store_Insert
     (Create (Switches, Pack.Finder),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  pretty_printer.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Pretty_Printer),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  pretty_printer.switches
   Store_Insert
     (Create (Switches, Pack.Pretty_Printer),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  gnatstub.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Gnatstub),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  gnatstub.switches
   Store_Insert
     (Create (Switches, Pack.Gnatstub),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  check.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Check),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  check.switches
   Store_Insert
     (Create (Switches, Pack.Check),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  eliminate.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Eliminate),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  eliminate.switches
   Store_Insert
     (Create (Switches, Pack.Eliminate),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  metrics.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Metrics),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  metrics.switches
   Store_Insert
     (Create (Switches, Pack.Metrics),
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => File_Names_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.default_switches
   Store_Insert
     (Create (Default_Switches, Pack.Ide),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  ide.remote_host
   Store_Insert
     (Create (Remote_Host, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.program_host
   Store_Insert
     (Create (Program_Host, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.communication_protocol
   Store_Insert
     (Create (Communication_Protocol, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.compiler_command
   Store_Insert
     (Create (Compiler_Command, Pack.Ide),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.debugger_command
   Store_Insert
     (Create (Debugger_Command, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.gnatlist
   Store_Insert
     (Create (Gnatlist, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_kind
   Store_Insert
     (Create (Vcs_Kind, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_file_check
   Store_Insert
     (Create (Vcs_File_Check, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_log_check
   Store_Insert
     (Create (Vcs_Log_Check, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.documentation_dir
   Store_Insert
     (Create (Documentation_Dir, Pack.Ide),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.prefix
   Store_Insert
     (Create (Prefix, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.sources_subdir
   Store_Insert
     (Create (Sources_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.exec_subdir
   Store_Insert
     (Create (Exec_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Store_Insert
     (Create (Lib_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.project_subdir
   Store_Insert
     (Create (Project_Subdir, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.active
   Store_Insert
     (Create (Active, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.artifacts
   Store_Insert
     (Create (Artifacts, Pack.Install),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.required_artifacts
   Store_Insert
     (Create (Required_Artifacts, Pack.Install),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.mode
   Store_Insert
     (Create (Mode, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.install_name
   Store_Insert
     (Create (Install_Name, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.root_dir
   Store_Insert
     (Create (Root_Dir, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.excluded_patterns
   Store_Insert
     (Create (Excluded_Patterns, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.included_patterns
   Store_Insert
     (Create (Included_Patterns, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.included_artifact_patterns
   Store_Insert
     (Create (Included_Artifact_Patterns, Pack.Remote),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  stack.switches
   Store_Insert
     (Create (Switches, Pack.Stack),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.output_directory
   Store_Insert
     (Create (Output_Directory, Pack.Codepeer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.database_directory
   Store_Insert
     (Create (Database_Directory, Pack.Codepeer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.message_patterns
   Store_Insert
     (Create (Message_Patterns, Pack.Codepeer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.additional_patterns
   Store_Insert
     (Create (Additional_Patterns, Pack.Codepeer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.switches
   Store_Insert
     (Create (Switches, Pack.Codepeer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.excluded_source_files
   Store_Insert
     (Create (Excluded_Source_Files, Pack.Codepeer),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  origin_project
   Store_Insert
     (Create (Origin_Project),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  side_debug
   Store_Insert
     (Create (Side_Debug, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  include_switches_via_spec
   Store_Insert
     (Create (Include_Switches_Via_Spec),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  only_dirs_with_sources
   Store_Insert
     (Create (Only_Dirs_With_Sources),
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  warning_message
   Store_Insert
     (Create (Warning_Message),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  canonical_target
   Store_Insert
     (Create (Canonical_Target),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  create_missing_dirs
   Store_Insert
     (Create (Create_Missing_Dirs),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install_project
   Store_Insert
     (Create (Install_Project, Pack.Install),
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

end GPR2.Project.Registry.Attribute;
