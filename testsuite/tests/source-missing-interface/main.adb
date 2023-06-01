--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
      Log  : GPR2.Log.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);
      Prj.Update_Sources (Messages => Log);
      if Log.Has_Element then
         Text_IO.Put_Line ("Messages found:");
         Log.Output_Messages;
      end if;
   end Check;

begin
   Check ("demo.gpr");
end Main;
