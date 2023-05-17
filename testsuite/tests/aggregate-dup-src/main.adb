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
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;
   Log : GPR2.Log.Object;

begin
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   Prj.Update_Sources (Messages => Log);
   Log.Output_Messages (Information => False);

exception
   when Project_Error =>

      Text_IO.Put_Line ("Messages found:");
      Prj.Log_Messages.Output_Messages (Information => False);
end Main;
