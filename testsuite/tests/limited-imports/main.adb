--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;
begin
   Project.Tree.Load (Prj, Create ("a.gpr"), Ctx);
   Text_IO.Put_Line ("All good, no message.");

exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               Mes : constant String := M.Format;
               F   : constant Natural :=
                       Strings.Fixed.Index (Mes, "imports ");
               L   : constant Natural :=
                       Strings.Fixed.Index (Mes, "/cyclic-projects");
            begin
               if F /= 0 and then L /= 0 then
                  Text_IO.Put_Line (Mes (1 .. F + 7) & Mes (L .. Mes'Last));
               else
                  Text_IO.Put_Line (Mes);
               end if;
            end;
         end loop;
      end if;
end Main;
