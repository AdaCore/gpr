--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Test;

procedure Main is
   Tree       : GPR2.Project.Tree.Object;
   Context    : GPR2.Context.Object;
   Main_Found : Integer := 0;
   Test_Found : Integer := 0;
   Log        : GPR2.Log.Object;

   use GPR2;

begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension ("test.gpr"),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   Tree.Update_Sources (Messages => Log);
   Log.Output_Messages;

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
      Tree.Log_Messages.Output_Messages (Information => False);
end Main;
