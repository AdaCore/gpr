--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   procedure Print_Variable
     (Variable : GPR2.Project.Variable.Object;
      Pack     : GPR2.Package_Id := GPR2.Project_Level_Scope) is
      use GPR2.Project.Registry.Attribute;
      use type GPR2.Package_Id;
   begin
      if Pack /= GPR2.Project_Level_Scope then
         Ada.Text_IO.Put (GPR2.Image (Pack) & '.');
      end if;

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

   procedure Test (Project : GPR2.Filename_Type) is
   begin
      Ada.Text_IO.Put_Line (String (Project) & ".gpr:");
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Print_Message;

      for V of Tree.Root_Project.Variables loop
         Print_Variable (V);
      end loop;

      for Pack of Tree.Root_Project.Packages loop
         for V of Tree.Root_Project.Variables (Pack) loop
            Print_Variable (V, Pack);
         end loop;
      end loop;

   exception
      when Project_Error =>
         Print_Message;
   end Test;

begin
   Test ("prj");
   Test ("prj1");
   Test ("prj2");
   Test ("prj3");
   Test ("prj4");
   Test ("prj5");
   Test ("prj6");
exception
   when Project_Error =>
      Print_Message;
end Main;
