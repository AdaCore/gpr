------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

package body GPR2_GNATCOLL_Projects is

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Optional_Attribute_Id;
      Package_Name   : Optional_Package_Id := No_Package;
      Index          : GPR2.Project.Attribute_Index.Object;
      Use_Extended   : Boolean := False)
      return GPR2.Project.Attribute.Object;
   --  Internal Attribute_Value returning GPR2.Project.Attribute.Object.
   --  Single, List kinds and return type formating will be done by callers

   -------------------
   -- Artifacts_Dir --
   -------------------

   function Artifacts_Dir (Project : GPR2.Project.View.Object)
                           return GNATCOLL.VFS.Virtual_File is
   begin
      if Project.Is_Defined then
         if Project.Kind not in K_Configuration | K_Abstract
           and then Project.Object_Directory.Is_Defined
         then
            return GPR2.Path_Name.GNATCOLL.To_Virtual_File
              (Project.Object_Directory);
         elsif Project.Tree.Subdirs /= GPR2.No_Filename then
            return GPR2.Path_Name.GNATCOLL.To_Virtual_File
              (Project.Dir_Name.Compose (Project.Tree.Subdirs));
         else
            return GPR2.Path_Name.GNATCOLL.To_Virtual_File (Project.Dir_Name);
         end if;
      else
         return GNATCOLL.VFS.No_File;
      end if;

   end Artifacts_Dir;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Optional_Attribute_Id;
      Package_Name   : Optional_Package_Id := No_Package;
      Index          : GPR2.Project.Attribute_Index.Object;
      Use_Extended   : Boolean := False)
      return GPR2.Project.Attribute.Object
   is

      function Get_Attribute
        (View  : GPR2.Project.View.Object;
         Pack  : GPR2.Package_Id;
         Name  : GPR2.Attribute_Id;
         Index : GPR2.Project.Attribute_Index.Object :=
           GPR2.Project.Attribute_Index.Undefined)
         return GPR2.Project.Attribute.Object;
      --  Get package's attribute value

      function Get_Attribute
        (View  : GPR2.Project.View.Object;
         Name  : GPR2.Attribute_Id;
         Index : GPR2.Project.Attribute_Index.Object :=
           GPR2.Project.Attribute_Index.Undefined)
         return GPR2.Project.Attribute.Object;
      --  Get view's attribute value

      -------------------
      -- Get_Attribute --
      -------------------

      function Get_Attribute
        (View  : GPR2.Project.View.Object;
         Pack  : GPR2.Package_Id;
         Name  : GPR2.Attribute_Id;
         Index : GPR2.Project.Attribute_Index.Object :=
           GPR2.Project.Attribute_Index.Undefined)
         return GPR2.Project.Attribute.Object
      is
         Attribute : GPR2.Project.Attribute.Object;
      begin
         if View.Check_Attribute
           (Pack   => Pack,
            Name   => Name,
            Index  => Index,
            Result => Attribute)
         then
            return Attribute;
         else
            return GPR2.Project.Attribute.Undefined;
         end if;
      end Get_Attribute;

      -------------------
      -- Get_Attribute --
      -------------------

      function Get_Attribute
        (View  : GPR2.Project.View.Object;
         Name  : GPR2.Attribute_Id;
         Index : GPR2.Project.Attribute_Index.Object :=
           GPR2.Project.Attribute_Index.Undefined)
         return GPR2.Project.Attribute.Object
      is
         Attribute : GPR2.Project.Attribute.Object;
      begin
         if View.Check_Attribute
           (Name   => Name,
            Index  => Index,
            Result => Attribute)
         then
            return Attribute;
         else
            return GPR2.Project.Attribute.Undefined;
         end if;
      end Get_Attribute;

      Attribute : GPR2.Project.Attribute.Object;
      --  Get_Attribute function return value

   begin
      if Project.Is_Defined then
         if Package_Name /= No_Package then

            --  Looking for a project's package's attribute

            if Project.Has_Package
              (Name => Package_Name, Check_Extended => True)
            then
               Attribute := Get_Attribute
                 (View  => Project,
                  Pack  => Package_Name,
                  Name  => Attribute_Name,
                  Index => Index);
            end if;

            --  If not yet found look if required in extended list.

            if not Attribute.Is_Defined
              and then Use_Extended
              and then Project.Is_Extending
            then
               declare
                  Extended_Root : GPR2.Project.View.Object :=
                                    Project.Extended_Root;
               begin
                  while Extended_Root.Is_Defined loop
                     if Extended_Root.Has_Package
                       (Name           => Package_Name,
                        Check_Extended => True)
                     then
                        Attribute := Get_Attribute
                          (View  => Extended_Root,
                           Pack  => Package_Name,
                           Name  => Attribute_Name,
                           Index => Index);
                     end if;
                     if not Attribute.Is_Defined
                       and then Extended_Root.Is_Extending
                     then
                        --  Try extended of extended.

                        Extended_Root := Extended_Root.Extended_Root;
                     else
                        --  Exit the loop and look for configuration attribute

                        Extended_Root := GPR2.Project.View.Undefined;
                     end if;
                  end loop;
               end;
            end if;

            --  If not yet found look for attribute in configuration view.

            if not Attribute.Is_Defined
              and then Project.Tree.Has_Configuration
              and then Project.Tree.Configuration.Corresponding_View.
                Has_Package (Name => Package_Name, Check_Extended => True)
            then
               Attribute := Get_Attribute
                 (View  => Project.Tree.Configuration.Corresponding_View,
                  Pack  => Package_Name,
                  Name  => Attribute_Name,
                  Index => Index);
            end if;
         else
            --  Look for attribute in view

            Attribute := Get_Attribute
              (View  => Project,
               Name  => Attribute_Name,
               Index => Index);

            --  If not yet found look if required in extended list.

            if not Attribute.Is_Defined
              and then Use_Extended
              and then Project.Is_Extending
            then
               declare
                  Extended_Root : GPR2.Project.View.Object :=
                                    Project.Extended_Root;
               begin
                  while Extended_Root.Is_Defined loop
                     Attribute := Get_Attribute
                       (View  => Extended_Root,
                        Name  => Attribute_Name,
                        Index => Index);
                     if not Attribute.Is_Defined
                       and then Extended_Root.Is_Extending
                     then
                        --  Try extended of extended.

                        Extended_Root := Extended_Root.Extended_Root;
                     else
                        --  Exit the loop and look for configuration attribute

                        Extended_Root := GPR2.Project.View.Undefined;
                     end if;
                  end loop;
               end;
            end if;

            --  If not yet found look for attribute in configuration view.

            if not Attribute.Is_Defined
              and then Project.Tree.Has_Configuration
            then
               Attribute := Get_Attribute
                 (View  => Project.Tree.Configuration.Corresponding_View,
                 Name  => Attribute_Name,
                 Index => Index);
            end if;
         end if;
      end if;
      return Attribute;
   end Attribute_Value;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Optional_Attribute_Id;
      Package_Name   : Optional_Package_Id := No_Package;
      Index          : String := "";
      Default        : String := "";
      Use_Extended   : Boolean := False) return String is

   begin
      if Attribute_Name /= No_Attribute then
         declare
            Attribute : constant GPR2.Project.Attribute.Object :=
                          Attribute_Value
                            (Project        => Project,
                             Package_Name   => Package_Name,
                             Attribute_Name => Attribute_Name,
                             Index          =>
                               (if Index = ""
                                then GPR2.Project.Attribute_Index.Undefined
                                else GPR2.Project.Attribute_Index.Create
                                  (Index)),
                             Use_Extended   => Use_Extended);

            use GPR2.Project.Registry.Attribute;
         begin
            if Attribute.Is_Defined
              and then Attribute.Kind = GPR2.Project.Registry.Attribute.Single
            then
               return Attribute.Value.Text;
            else
               return Default;
            end if;
         end;
      else
         return Default;
      end if;
   end Attribute_Value;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Optional_Attribute_Id;
      Package_Name   : Optional_Package_Id := No_Package;
      Index          : String := "";
      Use_Extended   : Boolean := False)
      return GNAT.Strings.String_List_Access is

      function List (Attribute : GPR2.Project.Attribute.Object)
                     return GNAT.Strings.String_List_Access;
      --  convert attribute values to string list

      ----------
      -- List --
      ----------

      function List (Attribute : GPR2.Project.Attribute.Object)
                     return GNAT.Strings.String_List_Access is
      begin
         if Attribute.Is_Defined then
            declare
               List : constant GNAT.Strings.String_List_Access :=
                        new GNAT.Strings.String_List
                          (1 .. Integer (Attribute.Count_Values));
               I    : Integer := 1;
            begin
               for Value of Attribute.Values loop
                  List (I) := new String'(Value.Text);
                  I := I + 1;
               end loop;
               return List;
            end;
         else
            return null;
         end if;
      end List;

   begin
      if Attribute_Name /= No_Attribute then
         return List (Attribute_Value
                      (Project        => Project,
                       Package_Name   => Package_Name,
                       Attribute_Name => Attribute_Name,
                       Index          =>
                         (if Index = ""
                          then GPR2.Project.Attribute_Index.Undefined
                          else GPR2.Project.Attribute_Index.Create (Index)),
                       Use_Extended   => Use_Extended));
      else
         return null;
      end if;
   end Attribute_Value;

   ------------
   -- Create --
   ------------

   function Create
     (Self            : GPR2.Project.Tree.Object;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : GPR2.Project.View.Object'Class :=
        GPR2.Project.View.Undefined;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True)
      return GNATCOLL.VFS.Virtual_File is
      use GNATCOLL.VFS;
   begin
      if GNAT.OS_Lib.Is_Absolute_Path (+Name) then
         return GNATCOLL.VFS.Create (Full_Filename => Name);
      else
         declare
            Full_Path : constant GPR2.Path_Name.Object := Self.Get_File
              (Base_Name        => GPR2.Path_Name.Create
                 (GPR2.Filename_Type (Name),
                  GPR2.Filename_Type (Name)).Simple_Name,
               View             => GPR2.Project.View.Object (Project),
               Use_Source_Path  => Use_Source_Path,
               Use_Object_Path  => Use_Object_Path);
         begin
            if not Full_Path.Is_Defined then
               return GNATCOLL.VFS.Create (Full_Filename => Name);
            else
               return GNATCOLL.VFS.Create
                 (Full_Filename => +String (Full_Path.Value));
            end if;
         end;
      end if;
   end Create;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages (Log                 : GPR2.Log.Object;
                              Output_Warnings     : Boolean := True;
                              Output_Informations : Boolean := False)
   is
      use GPR2.Log;
      Displayed : GPR2.Containers.Value_Set;
   begin
      for C in Log.Iterate
        (Information => Output_Informations,
         Warning     => Output_Warnings,
         Error       => True,
         Read        => True,
         Unread      => True)
      loop
         declare
            use GPR2.Message;

            Msg      : constant GPR2.Log.Constant_Reference_Type :=
                         Log.Constant_Reference (C);
            Text     : constant String := Msg.Format;
            Dummy    : GPR2.Containers.Value_Type_Set.Cursor;
            Inserted : Boolean;

            use Ada.Text_IO;
         begin
            Displayed.Insert (Text, Dummy, Inserted);

            if Inserted then
               Put_Line
                 (File_Access'
                    (case Msg.Level is
                        when Information     => Current_Output,
                        when Warning | Error => Current_Error).all,
                  Text);
            end if;
         end;
      end loop;
   end Output_Messages;

   ----------------------------
   -- Register_New_Attribute --
   ----------------------------

   function Register_New_Attribute
     (Name                 : Optional_Attribute_Id;
      Pkg                  : Optional_Package_Id := No_Package;
      Is_List              : Boolean := False;
      Indexed              : Boolean := False;
      Case_Sensitive_Index : Boolean := False) return String
   is
   begin
      if Name = No_Attribute then
         return "cannot register attribute without name for package "
           & Image (Pkg);
      end if;

      if Pkg /= No_Package and then not GPR2.Project.Registry.Pack.Exists (Pkg)
      then
         GPR2.Project.Registry.Pack.Add
           (Pkg, GPR2.Project.Registry.Pack.Everywhere);
      end if;

      GPR2.Project.Registry.Attribute.Add
        (Name                 => GPR2.Project.Registry.Attribute.Create
           (Name => Name,
            Pack => Pkg),
         Index                => (if Indexed
                                  then GPR2.Project.Registry.Attribute.Yes
                                  else GPR2.Project.Registry.Attribute.No),
         Others_Allowed       => False,
         Index_Case_Sensitive => Case_Sensitive_Index,
         Value                =>
           (if Is_List
            then GPR2.Project.Registry.Attribute.List
            else GPR2.Project.Registry.Attribute.Single),
         Value_Case_Sensitive => False,
         Is_Allowed_In        =>
           GPR2.Project.Registry.Attribute.Everywhere);

      return "";
   exception
      when others =>
         return "cannot register new attribute " & Image (Name)
           & " for package " & Image (Pkg);
   end Register_New_Attribute;

end GPR2_GNATCOLL_Projects;
