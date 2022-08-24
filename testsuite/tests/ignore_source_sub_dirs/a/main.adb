--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;

with Test;

procedure Main is
   Tree       : GPR2.Project.Tree.Object;
   Context    : GPR2.Context.Object;
   Main_Found : Integer := 0;
   Test_Found : Integer := 0;

   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension ("test.gpr"),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   for Prj of Tree loop
      for S of Prj.Sources loop
         if S.Path_Name.Simple_Name = "main.adb" then
            Main_Found := Main_Found + 1;
         elsif S.Path_Name.Simple_Name = "test.ads" then
            Test_Found := Test_Found + 1;
         elsif S.Path_Name.Simple_Name = "ignored.ads" then
            Ada.Text_IO.Put_Line
              ("unexpected " & String (S.Path_Name.Value) & " found");
         end if;
      end loop;
   end loop;
   if Main_Found /= 1 then
      Ada.Text_IO.Put_Line ("main.adb found" & Main_Found'Img & " times");
   end if;
   if Test_Found /= 1 then
      Ada.Text_IO.Put_Line ("test.ads found" & Test_Found'Img & " times");
   end if;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Print_Messages;
end Main;
