------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.OS.Constants;

with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

package body GPR2_GNATCOLL_Projects is

   use type GNATCOLL.OS.OS_Type;

   Is_Windows_Host : constant Boolean :=
                       GNATCOLL.OS.Constants.OS = GNATCOLL.OS.Windows
                         with Warnings => Off;

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
            return To_Virtual_File (Project.Object_Directory);
         elsif Project.Tree.Subdirs /= GPR2.No_Name then
            return To_Virtual_File
              (Project.Dir_Name.Compose (Project.Tree.Subdirs));
         else
            return To_Virtual_File (Project.Dir_Name);
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
      Package_Name   : String;
      Attribute_Name : String;
      Index          : String := "";
      Default        : String := "";
      Use_Extended   : Boolean := False) return String is

      use GPR2.Project.View;

      function Internal
        (Project           : GPR2.Project.View.Object;
         Package_Name      : String;
         Attribute_Name    : String;
         Index             : String := "";
         Default           : String := "";
         Use_Extended      : Boolean := False;
         Use_Configuration : Boolean := True) return String;
      --  Attribute_Value implementation if attribute not found in project
      --  and Use_Configuration is True, return value found in configuration.

      --------------
      -- Internal --
      --------------

      function Internal
        (Project           : GPR2.Project.View.Object;
         Package_Name      : String;
         Attribute_Name    : String;
         Index             : String := "";
         Default           : String := "";
         Use_Extended      : Boolean := False;
         Use_Configuration : Boolean := True) return String is

         Attribute_Index : constant GPR2.Project.Attribute_Index.Object :=
                             (if Index = ""
                              then GPR2.Project.Attribute_Index.Undefined
                              else GPR2.Project.Attribute_Index.Create
                                (Index));
      begin
         if Package_Name'Length > 0 then
            return Project.Pack
              (GPR2.Name_Type (Package_Name)).Attribute
                (GPR2.Name_Type (Attribute_Name), Attribute_Index).Value.Text;
         else
            return Project.Attribute
              (GPR2.Name_Type (Attribute_Name), Attribute_Index).Value.Text;
         end if;

      exception

         when E : others =>
            if Use_Extended and then Project.Is_Extending then
               return Internal
                 (Project           => Project.Extended,
                  Package_Name      => Package_Name,
                  Attribute_Name    => Attribute_Name,
                  Index             => Index,
                  Default           => Default,
                  Use_Extended      => False,
                  Use_Configuration => Use_Configuration);
            elsif Use_Configuration
              and then Project.Tree.Has_Configuration
            then
               return Internal
                 (Project           =>
                    Project.Tree.Configuration.Corresponding_View,
                  Package_Name      => Package_Name,
                  Attribute_Name    => Attribute_Name,
                  Index             => Index,
                  Default           => Default,
                  Use_Extended      => False,
                  Use_Configuration => False);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Exceptions.Exception_Information (E));
               return Default;
            end if;
      end Internal;

   begin
      return Internal
        (Project           => Project,
         Package_Name      => Package_Name,
         Attribute_Name    => Attribute_Name,
         Index             => Index,
         Default           => Default,
         Use_Extended      => Use_Extended,
         Use_Configuration => True);
   end Attribute_Value;

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Package_Name   : String;
      Attribute_Name : String;
      Index          : String := "";
      Use_Extended   : Boolean := False)
      return GNAT.Strings.String_List_Access is

      function Internal
        (Project           : GPR2.Project.View.Object;
         Package_Name      : String;
         Attribute_Name    : String;
         Index             : String := "";
         Use_Extended      : Boolean := False;
         Use_Configuration : Boolean := True)
         return GNAT.Strings.String_List_Access;
      --  Attribute_Value implementation if attribute not found in project
      --  and Use_Configuration is True, return value found in configuration.

      --------------
      -- Internal --
      --------------

      function Internal
        (Project           : GPR2.Project.View.Object;
         Package_Name      : String;
         Attribute_Name    : String;
         Index             : String := "";
         Use_Extended      : Boolean := False;
         Use_Configuration : Boolean := True)
         return GNAT.Strings.String_List_Access is

         function List (Attribute : GPR2.Project.Attribute.Object)
                        return GNAT.Strings.String_List_Access;
         --  convert attribute to string list

         ----------
         -- List --
         ----------

         function List (Attribute : GPR2.Project.Attribute.Object)
                        return GNAT.Strings.String_List_Access is
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
         end List;

         Attribute_Index : constant GPR2.Project.Attribute_Index.Object :=
                             (if Index = ""
                              then GPR2.Project.Attribute_Index.Undefined
                              else GPR2.Project.Attribute_Index.Create
                                (Index));

      begin
         if Package_Name'Length > 0 then
            return List (Project.Pack
                         (GPR2.Name_Type (Package_Name)).Attribute
                         (GPR2.Name_Type (Attribute_Name), Attribute_Index));
         else
            return List (Project.Attribute
                         (GPR2.Name_Type (Attribute_Name), Attribute_Index));
         end if;

      exception
         when E : others =>
            if Use_Extended and then Project.Is_Extending then
               return Internal
                 (Project           => Project.Extended,
                  Package_Name      => Package_Name,
                  Attribute_Name    => Attribute_Name,
                  Index             => Index,
                  Use_Extended      => False,
                  Use_Configuration => Use_Configuration);
            elsif Use_Configuration
              and then Project.Tree.Has_Configuration
            then
               return Internal
                 (Project           =>
                    Project.Tree.Configuration.Corresponding_View,
                  Package_Name      => Package_Name,
                  Attribute_Name    => Attribute_Name,
                  Index             => Index,
                  Use_Extended      => False,
                  Use_Configuration => False);
            else
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

               declare
                  List : constant GNAT.Strings.String_List_Access :=
                           new GNAT.Strings.String_List (1 .. 1);
               begin
                  List (1) := new String'("");
                  return List;
               end;
            end if;

      end Internal;

   begin
      return Internal (Project           => Project,
                       Package_Name      => Package_Name,
                       Attribute_Name    => Attribute_Name,
                       Index             => Index,
                       Use_Extended      => Use_Extended,
                       Use_Configuration => True);
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
                 (GPR2.Optional_Name_Type (Name),
                  GPR2.Optional_Name_Type (Name)).Simple_Name,
               View             => GPR2.Project.View.Object (Project),
               Use_Source_Path  => Use_Source_Path,
               Use_Object_Path  => Use_Object_Path);
         begin
            if not Full_Path.Is_Defined then
               return GNATCOLL.VFS.Create (Full_Filename => Name);
            else
               return GNATCOLL.VFS.Create
                 (Full_Filename => +String (Full_Path.Name));
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
     (Name                 : String;
      Pkg                  : String;
      Is_List              : Boolean := False;
      Indexed              : Boolean := False;
      Case_Sensitive_Index : Boolean := False) return String is
   begin
      if Pkg /= "" and then not GPR2.Project.Registry.Pack.Exists
        (GPR2.Name_Type (Pkg))
      then
         GPR2.Project.Registry.Pack.Add
           (GPR2.Name_Type (Pkg), GPR2.Project.Registry.Pack.Everywhere);
      end if;
      GPR2.Project.Registry.Attribute.Add
        (Name                 => GPR2.Project.Registry.Attribute.Create
           (GPR2.Optional_Name_Type (Name),
            GPR2.Optional_Name_Type (Pkg)),
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
         Read_Only            => False,
         Is_Allowed_In        =>
           GPR2.Project.Registry.Attribute.Everywhere);

      return "";
   exception
      when others =>
         return "cannot register new attribute " & Name & " for package "
           & Pkg;
   end Register_New_Attribute;

end GPR2_GNATCOLL_Projects;
