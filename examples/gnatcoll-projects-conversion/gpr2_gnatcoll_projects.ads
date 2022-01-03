------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with GNAT.Strings;
with GNATCOLL.VFS;

with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2_GNATCOLL_Projects is

   use GPR2;

   function To_Filesystem_String
     (Path : GPR2.Path_Name.Object) return GNATCOLL.VFS.Filesystem_String is
     (if Path.Is_Defined
      then GNATCOLL.VFS.Filesystem_String (Path.Value)
      else ""
     );

   function To_Virtual_File
     (Path : GPR2.Path_Name.Object) return GNATCOLL.VFS.Virtual_File is
     (if Path.Is_Defined
      then GNATCOLL.VFS.Create (To_Filesystem_String (Path))
      else GNATCOLL.VFS.No_File);

   function To_Pathname (Filename : GNATCOLL.VFS.Filesystem_String)
                         return GPR2.Path_Name.Object is
     (GPR2.Path_Name.Create
        (GPR2.Name_Type (Filename), GPR2.Name_Type (Filename)));

   function To_Pathname (File : GNATCOLL.VFS.Virtual_File)
                         return GPR2.Path_Name.Object is
     (GPR2.Path_Name.Create
        (GPR2.Name_Type (File.Display_Full_Name),
         GPR2.Name_Type (File.Display_Full_Name)));

   function Artifacts_Dir (Project : GPR2.Project.View.Object)
                           return GNATCOLL.VFS.Virtual_File;
   --  GNATCOLL.Projects.Artifacts_Dir (Project : Project_Type) conversion
   --  WARNING: this function is handling subdirs.

   function Object_Dir (Project : GPR2.Project.View.Object)
                        return GNATCOLL.VFS.Virtual_File is
     (if Project.Is_Defined
      then To_Virtual_File (Project.Object_Directory)
      else GNATCOLL.VFS.No_File);
   --  GNATCOLL.Projects.Object_Dir (Project : Project_Type) conversion
   --  WARNING: this function is handling subdirs.

   function Name (Project : GPR2.Project.View.Object) return String is
     (if Project.Is_Defined then String (Project.Name) else "default");
   --  GNATCOLL.Projects.Name (Project : Project_Type) conversion

   function Get_Runtime (Project : GPR2.Project.View.Object) return String is
     (String (Project.Tree.Runtime ("Ada")));
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
     (Name                 : String;
      Pkg                  : String;
      Is_List              : Boolean := False;
      Indexed              : Boolean := False;
      Case_Sensitive_Index : Boolean := False) return String;
   --  GNATCOLL.Projects.Register_New_Attribute conversion

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Package_Name   : String;
      Attribute_Name : String;
      Index          : String := "";
      Default        : String := "";
      Use_Extended   : Boolean := False) return String;
   --  GNATCOLL.Projects.Attribute_Value conversion (string attribute)

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Package_Name   : String;
      Attribute_Name : String;
      Index          : String := "";
      Use_Extended   : Boolean := False)
      return GNAT.Strings.String_List_Access;
   --  GNATCOLL.Projects.Attribute_Value conversion (string list attribute)

   procedure Output_Messages (Log                 : GPR2.Log.Object;
                              Output_Warnings     : Boolean := True;
                              Output_Informations : Boolean := False);
   --  print Log content

   function Get_Target (Tree : GPR2.Project.Tree.Object) return String is
     (if Tree.Add_Tool_Prefix ("x") = "x" then "" else String (Tree.Target));
   --  GNATCOLL.Projects.Get_Target conversion

end GPR2_GNATCOLL_Projects;
