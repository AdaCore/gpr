------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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

with Ada.Containers.Indefinite_Ordered_Maps; use Ada;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Environment_Variables;

package body GPR2.Project.Registry.Attribute is

   If_OS_Filename_Case_Sensitive : constant Boolean :=
                                     Environment_Variables.Exists ("OS");
   --  True on Windows

   function Less_Case_Insensitive
     (Left, Right : Qualified_Name) return Boolean is
     (Ada.Strings.Less_Case_Insensitive (String (Left), String (Right)));

   package Attribute_Definitions is new Ada.Containers.Indefinite_Ordered_Maps
     (Qualified_Name, Def, Less_Case_Insensitive);

   Store : Attribute_Definitions.Map;

   procedure Store_Insert
     (Name                 : Qualified_Name;
      Index                : Index_Kind;
      Others_Allowed       : Boolean;
      Index_Case_Sensitive : Boolean;
      Value                : Value_Kind;
      Value_Case_Sensitive : Boolean;
      Read_Only            : Boolean;
      Is_Allowed_In        : Allowed_In) with Inline;
   --  Calls Store.Insert with Key => Name and Value created from other fields

   --  Constants for some common attribute definitions

   Everywhere       : constant Allowed_In := (others => True);

   In_Library       : constant Allowed_In :=
                        (K_Library | K_Aggregate_Library => True,
                         others                          => False);

   No_Aggregates    : constant Allowed_In :=
                        (K_Aggregate | K_Aggregate_Library => False,
                         others                            => True);

   In_Configuration : constant Allowed_In :=
                        (K_Configuration => True, others => False);

   ------------
   -- Create --
   ------------

   function Create
     (Name : Name_Type;
      Pack : Optional_Name_Type := "") return Qualified_Name is
   begin
      return Qualified_Name
        (if Pack = No_Name
         then Name
         else Name_Type (Pack) & '.' & Name);
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists (Q_Name : Qualified_Name) return Boolean is
   begin
      return Store.Contains (Q_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Q_Name : Qualified_Name) return Def is
   begin
      return Store (Q_Name);
   end Get;

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
      Read_Only            : Boolean;
      Is_Allowed_In        : Allowed_In) is
   begin
      Store.Insert
        (Name,
         Def'(Index                => Index,
              Others_Allowed       => Others_Allowed,
              Index_Case_Sensitive => Index_Case_Sensitive,
              Value                => Value,
              Value_Case_Sensitive => Value_Case_Sensitive,
              Read_Only            => Read_Only,
              Is_Allowed_In        => Is_Allowed_In));
   end Store_Insert;

begin
   --  name
   Store_Insert
     ("name",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => True,
      Is_Allowed_In        => Everywhere);

   --  project_dir
   Store_Insert
     ("project_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => True,
      Is_Allowed_In        => Everywhere);

   --  main
   Store_Insert
     ("main",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  languages
   Store_Insert
     ("languages",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  roots
   Store_Insert
     ("roots",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  externally_built
   Store_Insert
     ("externally_built",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  object_dir
   Store_Insert
     ("object_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  exec_dir
   Store_Insert
     ("exec_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  source_dirs
   Store_Insert
     ("source_dirs",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  inherit_source_path
   Store_Insert
     ("inherit_source_path",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_dirs
   Store_Insert
     ("excluded_source_dirs",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ignore_source_sub_dirs
   Store_Insert
     ("ignore_source_sub_dirs",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  source_files
   Store_Insert
     ("source_files",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  locally_removed_files
   Store_Insert
     ("locally_removed_files",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_files
   Store_Insert
     ("excluded_source_files",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  source_list_file
   Store_Insert
     ("source_list_file",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  excluded_source_list_file
   Store_Insert
     ("excluded_source_list_file",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  interfaces
   Store_Insert
     ("interfaces",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  project_files
   Store_Insert
     ("project_files",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => (K_Aggregate | K_Aggregate_Library => True,
                               others => False));

   --  project_path
   Store_Insert
     ("project_path",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => (K_Aggregate | K_Aggregate_Library => True,
                               others => False));

   --  external
   Store_Insert
     ("external",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => (K_Aggregate | K_Aggregate_Library => True,
                               others => False));

   --  library_dir
   Store_Insert
     ("library_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_name
   Store_Insert
     ("library_name",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_kind
   Store_Insert
     ("library_kind",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_version
   Store_Insert
     ("library_version",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_interface
   Store_Insert
     ("library_interface",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_standalone
   Store_Insert
     ("library_standalone",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_encapsulated_options
   Store_Insert
     ("library_encapsulated_options",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_encapsulated_supported
   Store_Insert
     ("library_encapsulated_supported",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Configuration);

   --  library_auto_init
   Store_Insert
     ("library_auto_init",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  leading_library_options
   Store_Insert
     ("leading_library_options",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_options
   Store_Insert
     ("library_options",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_rpath_options
   Store_Insert
     ("library_rpath_options",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_src_dir
   Store_Insert
     ("library_src_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_ali_dir
   Store_Insert
     ("library_ali_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_gcc
   Store_Insert
     ("library_gcc",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_symbol_file
   Store_Insert
     ("library_symbol_file",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_symbol_policy
   Store_Insert
     ("library_symbol_policy",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => In_Library);

   --  library_reference_symbol_file
   Store_Insert
     ("library_reference_symbol_file",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  default_language
   Store_Insert
     ("default_language",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  run_path_option
   Store_Insert
     ("run_path_option",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  run_path_origin
   Store_Insert
     ("run_path_origin",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  separate_run_path_options
   Store_Insert
     ("separate_run_path_options",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  toolchain_version
   Store_Insert
     ("toolchain_version",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  toolchain_description
   Store_Insert
     ("toolchain_description",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_generated
   Store_Insert
     ("object_generated",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  objects_linked
   Store_Insert
     ("objects_linked",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  target
   Store_Insert
     ("target",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime
   Store_Insert
     ("runtime",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_builder
   Store_Insert
     ("library_builder",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_support
   Store_Insert
     ("library_support",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_builder
   Store_Insert
     ("archive_builder",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_builder_append_option
   Store_Insert
     ("archive_builder_append_option",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_indexer
   Store_Insert
     ("archive_indexer",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  archive_suffix
   Store_Insert
     ("archive_suffix",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_partial_linker
   Store_Insert
     ("library_partial_linker",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_lister
   Store_Insert
     ("object_lister",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  object_lister_matcher
   Store_Insert
     ("object_lister_matcher",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_prefix
   Store_Insert
     ("shared_library_prefix",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_suffix
   Store_Insert
     ("shared_library_suffix",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  symbolic_link_supported
   Store_Insert
     ("symbolic_link_supported",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_major_minor_id_supported
   Store_Insert
     ("library_major_minor_id_supported",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_auto_init_supported
   Store_Insert
     ("library_auto_init_supported",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  shared_library_minimum_switches
   Store_Insert
     ("shared_library_minimum_switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_version_switches
   Store_Insert
     ("library_version_switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  library_install_name_option
   Store_Insert
     ("library_install_name_option",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_dir
   Store_Insert
     ("runtime_dir",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_library_dir
   Store_Insert
     ("runtime_library_dir",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_source_dir
   Store_Insert
     ("runtime_source_dir",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  runtime_source_dirs
   Store_Insert
     ("runtime_source_dirs",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.spec_suffix
   Store_Insert
     ("naming.spec_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.body_suffix
   Store_Insert
     ("naming.body_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.specification_suffix
   Store_Insert
     ("naming.specification_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.implementation_suffix
   Store_Insert
     ("naming.implementation_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.separate_suffix
   Store_Insert
     ("naming.separate_suffix",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.casing
   Store_Insert
     ("naming.casing",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => False,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.dot_replacement
   Store_Insert
     ("naming.dot_replacement",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.spec
   Store_Insert
     ("naming.spec",
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.specification
   Store_Insert
     ("naming.specification",
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.body
   Store_Insert
     ("naming.body",
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.implementation
   Store_Insert
     ("naming.implementation",
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.specification_exceptions
   Store_Insert
     ("naming.specification_exceptions",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  naming.implementation_exceptions
   Store_Insert
     ("naming.implementation_exceptions",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.default_switches
   Store_Insert
     ("compiler.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  compiler.switches
   Store_Insert
     ("compiler.switches",
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_configuration_pragmas
   Store_Insert
     ("compiler.local_configuration_pragmas",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.local_config_file
   Store_Insert
     ("compiler.local_config_file",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.driver
   Store_Insert
     ("compiler.driver",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.language_kind
   Store_Insert
     ("compiler.language_kind",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_kind
   Store_Insert
     ("compiler.dependency_kind",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.required_switches
   Store_Insert
     ("compiler.required_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.leading_required_switches
   Store_Insert
     ("compiler.leading_required_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.trailing_required_switches
   Store_Insert
     ("compiler.trailing_required_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.pic_option
   Store_Insert
     ("compiler.pic_option",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.path_syntax
   Store_Insert
     ("compiler.path_syntax",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.source_file_switches
   Store_Insert
     ("compiler.source_file_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_file_suffix
   Store_Insert
     ("compiler.object_file_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_file_switches
   Store_Insert
     ("compiler.object_file_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.multi_unit_switches
   Store_Insert
     ("compiler.multi_unit_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.multi_unit_object_separator
   Store_Insert
     ("compiler.multi_unit_object_separator",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_file_switches
   Store_Insert
     ("compiler.mapping_file_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_spec_suffix
   Store_Insert
     ("compiler.mapping_spec_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.mapping_body_suffix
   Store_Insert
     ("compiler.mapping_body_suffix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_switches
   Store_Insert
     ("compiler.config_file_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name
   Store_Insert
     ("compiler.config_body_file_name",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_index
   Store_Insert
     ("compiler.config_body_file_name_index",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_body_file_name_pattern
   Store_Insert
     ("compiler.config_body_file_name_pattern",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name
   Store_Insert
     ("compiler.config_spec_file_name",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_index
   Store_Insert
     ("compiler.config_spec_file_name_index",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_spec_file_name_pattern
   Store_Insert
     ("compiler.config_spec_file_name_pattern",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.config_file_unique
   Store_Insert
     ("compiler.config_file_unique",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_switches
   Store_Insert
     ("compiler.dependency_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.dependency_driver
   Store_Insert
     ("compiler.dependency_driver",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_switches
   Store_Insert
     ("compiler.include_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_path
   Store_Insert
     ("compiler.include_path",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.include_path_file
   Store_Insert
     ("compiler.include_path_file",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.object_path_switches
   Store_Insert
     ("compiler.object_path_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.max_command_line_length
   Store_Insert
     ("compiler.max_command_line_length",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_format
   Store_Insert
     ("compiler.response_file_format",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  compiler.response_file_switches
   Store_Insert
     ("compiler.response_file_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.default_switches
   Store_Insert
     ("builder.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.switches
   Store_Insert
     ("builder.switches",
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.global_compilation_switches
   Store_Insert
     ("builder.global_compilation_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.executable
   Store_Insert
     ("builder.executable",
      Index                => Optional,
      Others_Allowed       => False,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.executable_suffix
   Store_Insert
     ("builder.executable_suffix",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  builder.global_configuration_pragmas
   Store_Insert
     ("builder.global_configuration_pragmas",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  builder.global_config_file
   Store_Insert
     ("builder.global_config_file",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  gnatls.switches
   Store_Insert
     ("gnatls.switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.default_switches
   Store_Insert
     ("binder.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  binder.switches
   Store_Insert
     ("binder.switches",
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.driver
   Store_Insert
     ("binder.driver",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.required_switches
   Store_Insert
     ("binder.required_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.prefix
   Store_Insert
     ("binder.prefix",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.objects_path
   Store_Insert
     ("binder.objects_path",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  binder.objects_path_file
   Store_Insert
     ("binder.objects_path_file",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.required_switches
   Store_Insert
     ("linker.required_switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.default_switches
   Store_Insert
     ("linker.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  linker.leading_switches
   Store_Insert
     ("linker.leading_switches",
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.switches
   Store_Insert
     ("linker.switches",
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.trailing_switches
   Store_Insert
     ("linker.trailing_switches",
      Index                => Optional,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.linker_options
   Store_Insert
     ("linker.linker_options",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.map_file_option
   Store_Insert
     ("linker.map_file_option",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.driver
   Store_Insert
     ("linker.driver",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.max_command_line_length
   Store_Insert
     ("linker.max_command_line_length",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_format
   Store_Insert
     ("linker.response_file_format",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.response_file_switches
   Store_Insert
     ("linker.response_file_switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_format
   Store_Insert
     ("linker.export_file_format",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  linker.export_file_switch
   Store_Insert
     ("linker.export_file_switch",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.switches
   Store_Insert
     ("clean.switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.source_artifact_extensions
   Store_Insert
     ("clean.source_artifact_extensions",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.object_artifact_extensions
   Store_Insert
     ("clean.object_artifact_extensions",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_exec_dir
   Store_Insert
     ("clean.artifacts_in_exec_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  clean.artifacts_in_object_dir
   Store_Insert
     ("clean.artifacts_in_object_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  cross_reference.default_switches
   Store_Insert
     ("cross_reference.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  cross_reference.switches
   Store_Insert
     ("cross_reference.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  finder.default_switches
   Store_Insert
     ("finder.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  finder.switches
   Store_Insert
     ("finder.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  pretty_printer.default_switches
   Store_Insert
     ("pretty_printer.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  pretty_printer.switches
   Store_Insert
     ("pretty_printer.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  gnatstub.default_switches
   Store_Insert
     ("gnatstub.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  gnatstub.switches
   Store_Insert
     ("gnatstub.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  check.default_switches
   Store_Insert
     ("check.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  check.switches
   Store_Insert
     ("check.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  eliminate.default_switches
   Store_Insert
     ("eliminate.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  eliminate.switches
   Store_Insert
     ("eliminate.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  metrics.default_switches
   Store_Insert
     ("metrics.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  metrics.switches
   Store_Insert
     ("metrics.switches",
      Index                => Yes,
      Others_Allowed       => True,
      Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.default_switches
   Store_Insert
     ("ide.default_switches",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => No_Aggregates);

   --  ide.remote_host
   Store_Insert
     ("ide.remote_host",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.program_host
   Store_Insert
     ("ide.program_host",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.communication_protocol
   Store_Insert
     ("ide.communication_protocol",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.compiler_command
   Store_Insert
     ("ide.compiler_command",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.debugger_command
   Store_Insert
     ("ide.debugger_command",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.gnatlist
   Store_Insert
     ("ide.gnatlist",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_kind
   Store_Insert
     ("ide.vcs_kind",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_file_check
   Store_Insert
     ("ide.vcs_file_check",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.vcs_log_check
   Store_Insert
     ("ide.vcs_log_check",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  ide.documentation_dir
   Store_Insert
     ("ide.documentation_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.prefix
   Store_Insert
     ("install.prefix",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.sources_subdir
   Store_Insert
     ("install.sources_subdir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.exec_subdir
   Store_Insert
     ("install.exec_subdir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.lib_subdir
   Store_Insert
     ("install.lib_subdir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.project_subdir
   Store_Insert
     ("install.project_subdir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.active
   Store_Insert
     ("install.active",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.artifacts
   Store_Insert
     ("install.artifacts",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.required_artifacts
   Store_Insert
     ("install.required_artifacts",
      Index                => Yes,
      Others_Allowed       => False,
      Index_Case_Sensitive => True,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.mode
   Store_Insert
     ("install.mode",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  install.install_name
   Store_Insert
     ("install.install_name",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.root_dir
   Store_Insert
     ("remote.root_dir",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.excluded_patterns
   Store_Insert
     ("remote.excluded_patterns",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.included_patterns
   Store_Insert
     ("remote.included_patterns",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  remote.included_artifact_patterns
   Store_Insert
     ("remote.included_artifact_patterns",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  stack.switches
   Store_Insert
     ("stack.switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.output_directory
   Store_Insert
     ("codepeer.output_directory",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.database_directory
   Store_Insert
     ("codepeer.database_directory",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.message_patterns
   Store_Insert
     ("codepeer.message_patterns",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.additional_patterns
   Store_Insert
     ("codepeer.additional_patterns",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.switches
   Store_Insert
     ("codepeer.switches",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  codepeer.excluded_source_files
   Store_Insert
     ("codepeer.excluded_source_files",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => List,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

   --  origin_project
   Store_Insert
     ("origin_project",
      Index                => No,
      Others_Allowed       => False,
      Index_Case_Sensitive => False,
      Value                => Single,
      Value_Case_Sensitive => True,
      Read_Only            => False,
      Is_Allowed_In        => Everywhere);

end GPR2.Project.Registry.Attribute;
