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

with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Variable;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   Project_Name : constant GPR2.Filename_Type := "prj";
   use GPR2;

   procedure Print_Message is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Message;

   procedure Print_Variable (Variable : GPR2.Project.Variable.Object) is
      use GPR2.Project.Registry.Attribute;
   begin
      Ada.Text_IO.Put(String (Variable.Name.Text) & ":" & Variable.Kind'Img & "=");
      if Variable.Kind = GPR2.Project.Registry.Attribute.Single then
         Ada.Text_IO.Put_Line (Variable.Value.Text);
      else
         for V of Variable.Values loop
            Ada.Text_IO.Put (V.Text & ";");
         end loop;
         Ada.Text_IO.Put_Line ("");
      end if;
   end Print_Variable;

begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (Project_Name),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   Print_Variable (Tree.Root_Project.Variable (Name => "A"));
   Print_Variable (Tree.Root_Project.Variable (Name => "B"));
   Print_Variable (Tree.Root_Project.Variable (Name => "C"));
   Print_Variable (Tree.Root_Project.Variable (Name => "D"));
   Print_Variable (Tree.Root_Project.Variable (Name => "E"));
   Print_Variable (Tree.Root_Project.Variable (Name => "F"));
   Print_Variable (Tree.Root_Project.Variable (Name => "G"));
   Print_Variable (Tree.Root_Project.Variable (Name => "H"));
   Print_Variable (Tree.Root_Project.Variable (Name => "I"));
   Print_Variable (Tree.Root_Project.Variable (Name => "J"));
   Print_Variable (Tree.Root_Project.Variable (Name => "K"));
   Print_Variable (Tree.Root_Project.Variable (Name => "L"));
   Print_Variable (Tree.Root_Project.Variable (Name => "M"));
   Print_Variable (Tree.Root_Project.Variable (Name => "N"));
   Print_Variable (Tree.Root_Project.Variable (Name => "O"));
exception
   when Project_Error =>
      Print_Message;
end Main;
