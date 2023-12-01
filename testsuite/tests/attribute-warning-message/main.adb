--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Log;
with Ada.Text_IO;

procedure Main is

   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load_Autoconf (Prj, Create ("demo.gpr"), Ctx);

   if Prj.Has_Messages then
      for C in Prj.Log_Messages.Iterate
         (False, True, True, False, True, True)
      loop
         Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
      end loop;
   end if;

end Main;
