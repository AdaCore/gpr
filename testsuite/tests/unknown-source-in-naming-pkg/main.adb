--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

   procedure Test (Project_Name : GPR2.Filename_Type) is
      Log      : GPR2.Log.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Tree.Update_Sources (Messages => Log);
      Log.Output_Messages (Information => False);
   exception
      when Project_Error =>
         Log.Output_Messages (Information => False);
   end Test;

begin
   Test ("files/ticket.gpr");
   Test ("files/ticket2.gpr");
   Test ("files/prj.gpr");
end Main;
