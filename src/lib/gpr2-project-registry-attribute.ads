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
      Pack : Optional_Name_Type := "") return Qualified_Name;
   --  Returns a fully qualified name for the given attribute and package names

   type Allowed_In is array (Project_Kind) of Boolean with Pack;

   type Def is record
      Index                : Index_Kind;
      Others_Allowed       : Boolean;
      Index_Case_Sensitive : Boolean;
      Value                : Value_Kind;
      Value_Case_Sensitive : Boolean;
      Value_Empty          : Empty_Value_Status;
      Read_Only            : Boolean;
      Is_Allowed_In        : Allowed_In;
   end record
     with Dynamic_Predicate =>
       --  Either Index is allowed or the other parts are default
       (Def.Index in Index_Allowed
        or else (not Def.Others_Allowed
                 and then not Def.Index_Case_Sensitive))
     and then
       --  Must be usable somewhere
       Def.Is_Allowed_In /= (Project_Kind => False);

   function Exists (Q_Name : Qualified_Name) return Boolean;
   --  The qualified name comprise the package name and attribute name, both
   --  parts are separated by a dot which is mandatory even if the package
   --  name is empty (for a top level attribute).

   function Get (Q_Name : Qualified_Name) return Def
     with Pre => Exists (Q_Name);
   --  Returns the definition data for the given attribute fully qualified name

   --  Some common attribute names

   ALI_Subdir                  : constant Name_Type := "ali_subdir";
   Active                      : constant Name_Type := "active";
   Archive_Suffix              : constant Name_Type := "archive_suffix";
   Artifacts                   : constant Name_Type := "artifacts";
   Body_N                      : constant Name_Type := "body";
   Body_Suffix                 : constant Name_Type := "body_suffix";
   Dot_Replacement             : constant Name_Type := "dot_replacement";
   Driver                      : constant Name_Type := "driver";
   Excluded_Patterns           : constant Name_Type := "excluded_patterns";
   Excluded_Source_Files       : constant Name_Type := "excluded_source_files";
   Excluded_Source_List_File   : constant Name_Type :=
                                   "excluded_source_list_file";
   Exec_Dir                    : constant Name_Type := "exec_dir";
   Exec_Subdir                 : constant Name_Type := "exec_subdir";
   Executable                  : constant Name_Type := "executable";
   Executable_Suffix           : constant Name_Type := "executable_suffix";
   External                    : constant Name_Type := "external";
   Externally_Built            : constant Name_Type := "externally_built";
   Implementation              : constant Name_Type := "implementation";
   Implementation_Exceptions   : constant Name_Type :=
                                   "implementation_exceptions";
   Implementation_Suffix       : constant Name_Type := "implementation_suffix";
   Included_Artifacts_Patterns : constant Name_Type :=
                                   "included_artifacts_patterns";
   Included_Patterns           : constant Name_Type := "included_patterns";
   Install_Name                : constant Name_Type := "install_name";
   Install_Project             : constant Name_Type := "install_project";
   Interfaces                  : constant Name_Type := "interfaces";
   Languages                   : constant Name_Type := "languages";
   Lib_Subdir                  : constant Name_Type := "lib_subdir";
   Library_Dir                 : constant Name_Type := "library_dir";
   Library_Interface           : constant Name_Type := "library_interface";
   Library_Kind                : constant Name_Type := "library_kind";
   Library_Name                : constant Name_Type := "library_name";
   Library_Options             : constant Name_Type := "library_options";
   Library_Standalone          : constant Name_Type := "library_standalone";
   Library_Version             : constant Name_Type := "library_version";
   Link_Lib_Subdir             : constant Name_Type := "link_lib_subdir";
   Linker_Options              : constant Name_Type := "linker_options";
   Main                        : constant Name_Type := "main";
   Mode                        : constant Name_Type := "mode";
   Name                        : constant Name_Type := "name";
   Object_Dir                  : constant Name_Type := "object_dir";
   Object_File_Suffix          : constant Name_Type := "object_file_suffix";
   Prefix                      : constant Name_Type := "prefix";
   Project_Dir                 : constant Name_Type := "project_dir";
   Project_Files               : constant Name_Type := "project_files";
   Project_Subdir              : constant Name_Type := "project_subdir";
   Required_Artifacts          : constant Name_Type := "required_artifacts";
   Root_Dir                    : constant Name_Type := "root_dir";
   Runtime                     : constant Name_Type := "runtime";
   Separate_Suffix             : constant Name_Type := "separate_suffix";
   Shared_Lib_Prefix           : constant Name_Type := "shared_lib_prefix";
   Shared_Lib_Suffix           : constant Name_Type := "shared_lib_suffix";
   Side_Debug                  : constant Name_Type := "side_debug";
   Source_Dirs                 : constant Name_Type := "source_dirs";
   Source_Files                : constant Name_Type := "source_files";
   Source_List_File            : constant Name_Type := "source_list_file";
   Sources_Subdir              : constant Name_Type := "sources_subdir";
   Spec                        : constant Name_Type := "spec";
   Spec_Suffix                 : constant Name_Type := "spec_suffix";
   Specification               : constant Name_Type := "specification";
   Specification_Exceptions    : constant Name_Type :=
                                   "specification_exceptions";
   Specification_Suffix        : constant Name_Type := "specification_suffix";
   Standalone_Library          : constant Name_Type := "standalone_library";
   Target                      : constant Name_Type := "target";

private

   type Qualified_Name is new Name_Type;

end GPR2.Project.Registry.Attribute;
