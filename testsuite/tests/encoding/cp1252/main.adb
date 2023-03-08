--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
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

   function Test
     (Project_Name : GPR2.Filename_Type)
      return String is
    begin
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      return Tree.Root_Project.Variable ("Var").Value.Text;
   exception
      when Project_Error =>
         Print_Messages;
         return "";
   end Test;
begin
   declare
      UTF8 : String := Test ("utf8.gpr");
      CP1252 : String := Test ("cp1252.gpr");
   begin
      --  check that Could not decode source as "UTF-8" reported when
      --  file is not using UTF-8 & CP-1252 encoding.

      Put (Test ("cp1252error.gpr"));

      if UTF8 = CP1252 then
         Put_Line ("OK");
      else
         Put_Line ("utf8.gpr returned");
         Put_Line (UTF8);
         Put_Line ("cp1252.gpr returned");
         Put_Line (CP1252);
      end if;
   end;
end Main;
