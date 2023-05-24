--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Build.Source;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;
   Log : GPR2.Log.Object;

   procedure Display_Source (Name : Simple_Name);

   procedure Display_Source (Name : Simple_Name) is
      Src : GPR2.Build.Source.Object;
   begin
      if Prj.Root_Project.Has_Source (Name) then
         Src := Prj.Root_Project.Source (Name);
         Text_IO.Put_Line (String (Name) & ": " & Src.Kind'Image);
      else
         Text_IO.Put_Line ("no such source: " & String (Name));
      end if;
   end Display_Source;

begin
   Project.Tree.Load (Prj, Create ("./data/prj.gpr"), Ctx);

   Prj.Update_Sources (Messages => Log);
   Display_Source ("pkg.a");
   Display_Source ("pkg_b.a");
   Display_Source ("pkg-execute_s_b.a");

exception
   when Project_Error =>
      Text_IO.Put_Line ("Cannot load tree:");
      Prj.Log_Messages.Output_Messages
        (Information => False, Warning => False);
end Main;
