--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Environment_Variables;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   Filename     : constant GPR2.Path_Name.Object :=
                    GPR2.Path_Name.Create_File
                      (GPR2.Project.Ensure_Extension ("aggr.gpr"),
                       GPR2.Path_Name.No_Resolution);
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate (Information => False)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   procedure Test (Name : String) is
   begin
      Ada.Text_IO.Put_Line (Name);
      Tree.Unload;
      Tree.Load_Autoconf (Filename => Filename, Context => Context);
      Print_Messages;
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

begin
   Test ("Test1");
   Context.Include ("TEST", "2");
   Test ("Test2");
   Context.Include ("TEST", "3");
   Test ("Test3");
   Context.Include ("TEST", "4");
   Test ("Test4");
   Context.Include ("TEST", "5");
   Ada.Environment_Variables.Set ("VAR", "5");
   Test ("Test5");
   Context.Include ("TEST", "6");
   Ada.Environment_Variables.Set ("VAR", "BAD");
   Context.Include ("VAR", "6");
   Test ("Test6");

end Main;
