--
--  Copyright (C) 2020-2022, AdaCore
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

   procedure Test (Prj : GPR2.Filename_Type) is
   begin
      Ada.Text_IO.Put_Line (String (Prj) & ".gpr:");
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Prj),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      for V of Tree.Root_Project.Variables loop
         Print_Variable (V);
      end loop;
   exception
      when Project_Error =>
         Print_Message;
   end Test;

begin
   Test ("prj");
   Test ("prj2");
   Test ("prj3");
   Test ("prj4");
end Main;
