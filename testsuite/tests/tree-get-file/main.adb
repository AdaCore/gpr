--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   Project_Name : constant GPR2.Filename_Type := "test";
   use GPR2;
begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (Project_Name),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);

   Tree.Update_Sources (With_Runtime => True);

   if not Tree.Log_Messages.Has_Error then
      declare
         use Ada.Text_IO;

         OK : Boolean := True;

         procedure Check (S : GPR2.Simple_Name; Should_Be : Boolean := True) is
            File : constant GPR2.Path_Name.Object := Tree.Get_File (S);
         begin
            if (File.Is_Defined and then File.Exists
                and then File.Simple_Name = S) /= Should_Be
            then
               Put_Line ("Wrong for " & String (S));
               OK := False;
            end if;
         end Check;

      begin
         Check ("gpr2.gpr");
         Check ("main.adb");
         Check ("main.o");
         Check ("gpr2-project-tree.ads");
         Check ("a-textio.ads");
         Check ("none.ads", Should_Be => False);

         if OK then
            Put_Line ("OK");
         end if;
      end;
   end if;
end Main;
