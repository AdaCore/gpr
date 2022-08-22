--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

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

begin
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

exception
   when GPR2.Project_Error =>
      for C in Prj.Log_Messages.Iterate
        (False, False, True, True, True)
      loop
         declare
            M : constant Message.Object := Log.Element (C);
         begin
            Text_IO.Put_Line (M.Message);
         end;
      end loop;
end Main;
