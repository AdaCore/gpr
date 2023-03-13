--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;


procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, False, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension ("double_quote"),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   Ada.Text_IO.Put_Line
     ("Variable = " & Tree.Root_Project.Variable ("var").Value.Text);
   Ada.Text_IO.Put_Line (Tree.Root_Project.Variable ("var").Image);
   Ada.Text_IO.Put_Line
     ("Attribute = " &
        Tree.Root_Project.Attribute
        (Name  => GPR2.Project.Registry.Attribute.Compiler.Default_Switches,
         Index =>
           GPR2.Project.Attribute_Index.Create
             (GPR2.C_Language)).Values.First_Element.Text);
   Ada.Text_IO.Put_Line
     (Tree.Root_Project.Attribute
        (Name  => GPR2.Project.Registry.Attribute.Compiler.Default_Switches,
         Index =>
           GPR2.Project.Attribute_Index.Create (GPR2.C_Language)).Image);

exception
   when Project_Error =>
      Print_Messages;
end Main;
