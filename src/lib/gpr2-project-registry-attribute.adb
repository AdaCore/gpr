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

begin
   --  name
   Store.Insert
     ("name",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => True,
           Is_Allowed_In        => Everywhere));

   --  project_dir
   Store.Insert
     ("project_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => True,
           Is_Allowed_In        => Everywhere));

   --  main
   Store.Insert
     ("main",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  languages
   Store.Insert
     ("languages",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => False,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  roots
   Store.Insert
     ("roots",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  externally_built
   Store.Insert
     ("externally_built",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => False,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  object_dir
   Store.Insert
     ("object_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  exec_dir
   Store.Insert
     ("exec_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  source_dirs
   Store.Insert
     ("source_dirs",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  inherit_source_path
   Store.Insert
     ("inherit_source_path",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  excluded_source_dirs
   Store.Insert
     ("excluded_source_dirs",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ignore_source_sub_dirs
   Store.Insert
     ("ignore_source_sub_dirs",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  source_files
   Store.Insert
     ("source_files",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  locally_removed_files
   Store.Insert
     ("locally_removed_files",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  excluded_source_files
   Store.Insert
     ("excluded_source_files",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  source_list_file
   Store.Insert
     ("source_list_file",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  excluded_source_list_file
   Store.Insert
     ("excluded_source_list_file",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  interfaces
   Store.Insert
     ("interfaces",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  project_files
   Store.Insert
     ("project_files",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => (K_Aggregate | K_Aggregate_Library => True,
                                    others => False)));

   --  project_path
   Store.Insert
     ("project_path",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => (K_Aggregate | K_Aggregate_Library => True,
                                    others => False)));

   --  external
   Store.Insert
     ("external",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => True,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => (K_Aggregate | K_Aggregate_Library => True,
                                    others => False)));

   --  library_dir
   Store.Insert
     ("library_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_name
   Store.Insert
     ("library_name",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_kind
   Store.Insert
     ("library_kind",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => False,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_version
   Store.Insert
     ("library_version",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_interface
   Store.Insert
     ("library_interface",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_standalone
   Store.Insert
     ("library_standalone",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => False,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_encapsulated_options
   Store.Insert
     ("library_encapsulated_options",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_encapsulated_supported
   Store.Insert
     ("library_encapsulated_supported",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Configuration));

   --  library_auto_init
   Store.Insert
     ("library_auto_init",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  leading_library_options
   Store.Insert
     ("leading_library_options",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_options
   Store.Insert
     ("library_options",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_rpath_options
   Store.Insert
     ("library_rpath_options",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_src_dir
   Store.Insert
     ("library_src_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_ali_dir
   Store.Insert
     ("library_ali_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_gcc
   Store.Insert
     ("library_gcc",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_symbol_file
   Store.Insert
     ("library_symbol_file",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_symbol_policy
   Store.Insert
     ("library_symbol_policy",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => In_Library));

   --  library_reference_symbol_file
   Store.Insert
     ("library_reference_symbol_file",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  default_language
   Store.Insert
     ("default_language",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  run_path_option
   Store.Insert
     ("run_path_option",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  run_path_origin
   Store.Insert
     ("run_path_origin",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  separate_run_path_options
   Store.Insert
     ("separate_run_path_options",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  toolchain_version
   Store.Insert
     ("toolchain_version",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  toolchain_description
   Store.Insert
     ("toolchain_description",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  object_generated
   Store.Insert
     ("object_generated",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  objects_linked
   Store.Insert
     ("objects_linked",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  target
   Store.Insert
     ("target",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  runtime
   Store.Insert
     ("runtime",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_builder
   Store.Insert
     ("library_builder",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_support
   Store.Insert
     ("library_support",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  archive_builder
   Store.Insert
     ("archive_builder",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  archive_builder_append_option
   Store.Insert
     ("archive_builder_append_option",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  archive_indexer
   Store.Insert
     ("archive_indexer",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  archive_suffix
   Store.Insert
     ("archive_suffix",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_partial_linker
   Store.Insert
     ("library_partial_linker",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  object_lister
   Store.Insert
     ("object_lister",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  object_lister_matcher
   Store.Insert
     ("object_lister_matcher",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  shared_library_prefix
   Store.Insert
     ("shared_library_prefix",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  shared_library_suffix
   Store.Insert
     ("shared_library_suffix",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  symbolic_link_supported
   Store.Insert
     ("symbolic_link_supported",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_major_minor_id_supported
   Store.Insert
     ("library_major_minor_id_supported",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_auto_init_supported
   Store.Insert
     ("library_auto_init_supported",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  shared_library_minimum_switches
   Store.Insert
     ("shared_library_minimum_switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_version_switches
   Store.Insert
     ("library_version_switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  library_install_name_option
   Store.Insert
     ("library_install_name_option",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  runtime_dir
   Store.Insert
     ("runtime_dir",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  runtime_library_dir
   Store.Insert
     ("runtime_library_dir",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  runtime_source_dir
   Store.Insert
     ("runtime_source_dir",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  runtime_source_dirs
   Store.Insert
     ("runtime_source_dirs",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.spec_suffix
   Store.Insert
     ("naming.spec_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.body_suffix
   Store.Insert
     ("naming.body_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.specification_suffix
   Store.Insert
     ("naming.specification_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.implementation_suffix
   Store.Insert
     ("naming.implementation_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.separate_suffix
   Store.Insert
     ("naming.separate_suffix",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.casing
   Store.Insert
     ("naming.casing",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => False,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.dot_replacement
   Store.Insert
     ("naming.dot_replacement",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.spec
   Store.Insert
     ("naming.spec",
      Def'(Index                => Optional,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.specification
   Store.Insert
     ("naming.specification",
      Def'(Index                => Optional,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.body
   Store.Insert
     ("naming.body",
      Def'(Index                => Optional,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.implementation
   Store.Insert
     ("naming.implementation",
      Def'(Index                => Optional,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.specification_exceptions
   Store.Insert
     ("naming.specification_exceptions",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  naming.implementation_exceptions
   Store.Insert
     ("naming.implementation_exceptions",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.default_switches
   Store.Insert
     ("compiler.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  compiler.switches
   Store.Insert
     ("compiler.switches",
      Def'(Index                => Optional,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.local_configuration_pragmas
   Store.Insert
     ("compiler.local_configuration_pragmas",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.local_config_file
   Store.Insert
     ("compiler.local_config_file",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.driver
   Store.Insert
     ("compiler.driver",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.language_kind
   Store.Insert
     ("compiler.language_kind",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.dependency_kind
   Store.Insert
     ("compiler.dependency_kind",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.required_switches
   Store.Insert
     ("compiler.required_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.leading_required_switches
   Store.Insert
     ("compiler.leading_required_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.trailing_required_switches
   Store.Insert
     ("compiler.trailing_required_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.pic_option
   Store.Insert
     ("compiler.pic_option",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.path_syntax
   Store.Insert
     ("compiler.path_syntax",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.source_file_switches
   Store.Insert
     ("compiler.source_file_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.object_file_suffix
   Store.Insert
     ("compiler.object_file_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.object_file_switches
   Store.Insert
     ("compiler.object_file_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.multi_unit_switches
   Store.Insert
     ("compiler.multi_unit_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.multi_unit_object_separator
   Store.Insert
     ("compiler.multi_unit_object_separator",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.mapping_file_switches
   Store.Insert
     ("compiler.mapping_file_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.mapping_spec_suffix
   Store.Insert
     ("compiler.mapping_spec_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.mapping_body_suffix
   Store.Insert
     ("compiler.mapping_body_suffix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_file_switches
   Store.Insert
     ("compiler.config_file_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_body_file_name
   Store.Insert
     ("compiler.config_body_file_name",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_body_file_name_index
   Store.Insert
     ("compiler.config_body_file_name_index",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_body_file_name_pattern
   Store.Insert
     ("compiler.config_body_file_name_pattern",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_spec_file_name
   Store.Insert
     ("compiler.config_spec_file_name",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_spec_file_name_index
   Store.Insert
     ("compiler.config_spec_file_name_index",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_spec_file_name_pattern
   Store.Insert
     ("compiler.config_spec_file_name_pattern",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.config_file_unique
   Store.Insert
     ("compiler.config_file_unique",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.dependency_switches
   Store.Insert
     ("compiler.dependency_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.dependency_driver
   Store.Insert
     ("compiler.dependency_driver",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.include_switches
   Store.Insert
     ("compiler.include_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.include_path
   Store.Insert
     ("compiler.include_path",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.include_path_file
   Store.Insert
     ("compiler.include_path_file",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.object_path_switches
   Store.Insert
     ("compiler.object_path_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.max_command_line_length
   Store.Insert
     ("compiler.max_command_line_length",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.response_file_format
   Store.Insert
     ("compiler.response_file_format",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  compiler.response_file_switches
   Store.Insert
     ("compiler.response_file_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  builder.default_switches
   Store.Insert
     ("builder.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  builder.switches
   Store.Insert
     ("builder.switches",
      Def'(Index                => Optional,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  builder.global_compilation_switches
   Store.Insert
     ("builder.global_compilation_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  builder.executable
   Store.Insert
     ("builder.executable",
      Def'(Index                => Optional,
           Others_Allowed       => False,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  builder.executable_suffix
   Store.Insert
     ("builder.executable_suffix",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  builder.global_configuration_pragmas
   Store.Insert
     ("builder.global_configuration_pragmas",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  builder.global_config_file
   Store.Insert
     ("builder.global_config_file",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  gnatls.switches
   Store.Insert
     ("gnatls.switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  binder.default_switches
   Store.Insert
     ("binder.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  binder.switches
   Store.Insert
     ("binder.switches",
      Def'(Index                => Optional,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  binder.driver
   Store.Insert
     ("binder.driver",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  binder.required_switches
   Store.Insert
     ("binder.required_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  binder.prefix
   Store.Insert
     ("binder.prefix",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  binder.objects_path
   Store.Insert
     ("binder.objects_path",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  binder.objects_path_file
   Store.Insert
     ("binder.objects_path_file",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.required_switches
   Store.Insert
     ("linker.required_switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.default_switches
   Store.Insert
     ("linker.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  linker.leading_switches
   Store.Insert
     ("linker.leading_switches",
      Def'(Index                => Optional,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.switches
   Store.Insert
     ("linker.switches",
      Def'(Index                => Optional,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.trailing_switches
   Store.Insert
     ("linker.trailing_switches",
      Def'(Index                => Optional,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.linker_options
   Store.Insert
     ("linker.linker_options",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.map_file_option
   Store.Insert
     ("linker.map_file_option",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.driver
   Store.Insert
     ("linker.driver",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.max_command_line_length
   Store.Insert
     ("linker.max_command_line_length",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.response_file_format
   Store.Insert
     ("linker.response_file_format",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.response_file_switches
   Store.Insert
     ("linker.response_file_switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.export_file_format
   Store.Insert
     ("linker.export_file_format",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  linker.export_file_switch
   Store.Insert
     ("linker.export_file_switch",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  clean.switches
   Store.Insert
     ("clean.switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  clean.source_artifact_extensions
   Store.Insert
     ("clean.source_artifact_extensions",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  clean.object_artifact_extensions
   Store.Insert
     ("clean.object_artifact_extensions",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  clean.artifacts_in_exec_dir
   Store.Insert
     ("clean.artifacts_in_exec_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  clean.artifacts_in_object_dir
   Store.Insert
     ("clean.artifacts_in_object_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  cross_reference.default_switches
   Store.Insert
     ("cross_reference.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  cross_reference.switches
   Store.Insert
     ("cross_reference.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  finder.default_switches
   Store.Insert
     ("finder.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  finder.switches
   Store.Insert
     ("finder.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  pretty_printer.default_switches
   Store.Insert
     ("pretty_printer.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  pretty_printer.switches
   Store.Insert
     ("pretty_printer.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  gnatstub.default_switches
   Store.Insert
     ("gnatstub.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  gnatstub.switches
   Store.Insert
     ("gnatstub.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  check.default_switches
   Store.Insert
     ("check.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  check.switches
   Store.Insert
     ("check.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  eliminate.default_switches
   Store.Insert
     ("eliminate.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  eliminate.switches
   Store.Insert
     ("eliminate.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  metrics.default_switches
   Store.Insert
     ("metrics.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  metrics.switches
   Store.Insert
     ("metrics.switches",
      Def'(Index                => Yes,
           Others_Allowed       => True,
           Index_Case_Sensitive => If_OS_Filename_Case_Sensitive,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.default_switches
   Store.Insert
     ("ide.default_switches",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => No_Aggregates));

   --  ide.remote_host
   Store.Insert
     ("ide.remote_host",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.program_host
   Store.Insert
     ("ide.program_host",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.communication_protocol
   Store.Insert
     ("ide.communication_protocol",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.compiler_command
   Store.Insert
     ("ide.compiler_command",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.debugger_command
   Store.Insert
     ("ide.debugger_command",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.gnatlist
   Store.Insert
     ("ide.gnatlist",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.vcs_kind
   Store.Insert
     ("ide.vcs_kind",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.vcs_file_check
   Store.Insert
     ("ide.vcs_file_check",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.vcs_log_check
   Store.Insert
     ("ide.vcs_log_check",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  ide.documentation_dir
   Store.Insert
     ("ide.documentation_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.prefix
   Store.Insert
     ("install.prefix",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.sources_subdir
   Store.Insert
     ("install.sources_subdir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.exec_subdir
   Store.Insert
     ("install.exec_subdir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.lib_subdir
   Store.Insert
     ("install.lib_subdir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.project_subdir
   Store.Insert
     ("install.project_subdir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.active
   Store.Insert
     ("install.active",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.artifacts
   Store.Insert
     ("install.artifacts",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => True,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.required_artifacts
   Store.Insert
     ("install.required_artifacts",
      Def'(Index                => Yes,
           Others_Allowed       => False,
           Index_Case_Sensitive => True,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.mode
   Store.Insert
     ("install.mode",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  install.install_name
   Store.Insert
     ("install.install_name",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  remote.root_dir
   Store.Insert
     ("remote.root_dir",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  remote.excluded_patterns
   Store.Insert
     ("remote.excluded_patterns",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  remote.included_patterns
   Store.Insert
     ("remote.included_patterns",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  remote.included_artifact_patterns
   Store.Insert
     ("remote.included_artifact_patterns",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  stack.switches
   Store.Insert
     ("stack.switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  codepeer.output_directory
   Store.Insert
     ("codepeer.output_directory",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  codepeer.database_directory
   Store.Insert
     ("codepeer.database_directory",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  codepeer.message_patterns
   Store.Insert
     ("codepeer.message_patterns",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  codepeer.additional_patterns
   Store.Insert
     ("codepeer.additional_patterns",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  codepeer.switches
   Store.Insert
     ("codepeer.switches",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  codepeer.excluded_source_files
   Store.Insert
     ("codepeer.excluded_source_files",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => List,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

   --  origin_project
   Store.Insert
     ("origin_project",
      Def'(Index                => No,
           Others_Allowed       => False,
           Index_Case_Sensitive => False,
           Value                => Single,
           Value_Case_Sensitive => True,
           Read_Only            => False,
           Is_Allowed_In        => Everywhere));

end GPR2.Project.Registry.Attribute;
