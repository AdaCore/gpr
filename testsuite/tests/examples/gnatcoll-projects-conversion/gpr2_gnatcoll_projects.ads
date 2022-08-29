--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package can be used to facilitate GNATCOLL.Projects to GPR2 conversion

--  It provides primitives to emulate:
--    GNATCOLL.Projects.Artifacts_Dir
--    GNATCOLL.Projects.Attribute_Value
--    GNATCOLL.Projects.Create
--    GNATCOLL.Projects.Get_Runtime
--    GNATCOLL.Projects.Get_Target
--    GNATCOLL.Projects.Name
--    GNATCOLL.Projects.Object_Dir
--    GNATCOLL.Projects.Register_New_Attribute

--  The following file path types conversions are provided
--    GNATCOLL.VFS.Filesystem_String / GPR2.Path_Name.Object
--    GNATCOLL.VFS.Virtual_File / GPR2.Path_Name.Object

--  Output_Messages procedure can be used to print info/warnings/error messages

with GNAT.Strings;
with GNATCOLL.VFS;

with GPR2; use GPR2;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2_GNATCOLL_Projects is

   function Artifacts_Dir
     (Project : GPR2.Project.View.Object)
      return GNATCOLL.VFS.Virtual_File;
   --  GNATCOLL.Projects.Artifacts_Dir (Project : Project_Type) conversion
   --  WARNING: this function is handling subdirs.

   function Object_Dir
     (Project : GPR2.Project.View.Object)
      return GNATCOLL.VFS.Virtual_File
   is
     (if Project.Is_Defined
      then GPR2.Path_Name.Virtual_File (Project.Object_Directory)
      else GNATCOLL.VFS.No_File);
   --  GNATCOLL.Projects.Object_Dir (Project : Project_Type) conversion
   --  WARNING: this function is handling subdirs.

   function Name
     (Project : GPR2.Project.View.Object) return String
   is
     (if Project.Is_Defined then String (Project.Name) else "default");
   --  GNATCOLL.Projects.Name (Project : Project_Type) conversion

   function Get_Runtime
     (Project : GPR2.Project.View.Object) return String
   is
     (String (Project.Tree.Runtime (Ada_Language)));
   --  GNATCOLL.Projects.Get_Runtime (Project : Project_Type) conversion

   function Create
     (Self            : GPR2.Project.Tree.Object;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : GPR2.Project.View.Object'Class :=
                          GPR2.Project.View.Undefined;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True)
      return GNATCOLL.VFS.Virtual_File;
   --  GNATCOLL.Projects.Create function conversion

   function Register_New_Attribute
     (Name                 : Q_Attribute_Id;
      Is_List              : Boolean := False;
      Indexed              : Boolean := False;
      Case_Sensitive_Index : Boolean := False) return String;
   --  GNATCOLL.Projects.Register_New_Attribute conversion

   function Attribute_Value
     (Project      : GPR2.Project.View.Object;
      Name         : Q_Attribute_Id;
      Index        : String := "";
      Default      : String := "";
      Use_Extended : Boolean := False) return String;
   --  GNATCOLL.Projects.Attribute_Value conversion (string attribute)

   function Attribute_Value
     (Project      : GPR2.Project.View.Object;
      Name         : Q_Attribute_Id;
      Index        : String := "";
      Use_Extended : Boolean := False)
      return GNAT.Strings.String_List_Access;
   --  GNATCOLL.Projects.Attribute_Value conversion (string list attribute)

   procedure Output_Messages
     (Log                 : GPR2.Log.Object;
      Output_Warnings     : Boolean := True;
      Output_Informations : Boolean := False);
   --  print Log content

   function Get_Target
     (Tree : GPR2.Project.Tree.Object) return String
   is
     (if Tree.Add_Tool_Prefix ("x") = "x"
      then ""
      else String (Tree.Target));
   --  GNATCOLL.Projects.Get_Target conversion

end GPR2_GNATCOLL_Projects;
