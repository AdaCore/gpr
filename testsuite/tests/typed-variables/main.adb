--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Load (Filename : String);

   ----------
   -- Load --
   ----------

   procedure Load (Filename : String) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
   begin
      Project.Tree.Load (Prj, Create (Filename_Type (Filename)), Ctx);
      Text_IO.Put_Line ("All good, no message.");

   exception
      when GPR2.Project_Error =>
         if Prj.Has_Messages then
            Text_IO.Put_Line ("Messages found:");

            for C in Prj.Log_Messages.Iterate
              (False, False, True, True, True)
            loop
               declare
                  M   : constant Message.Object := Log.Element (C);
                  Mes : constant String := M.Format;
                  L   : constant Natural :=
                    Strings.Fixed.Index (Mes, "/typed-variables");
               begin
                  if L /= 0 then
                     Text_IO.Put_Line (Mes (L .. Mes'Last));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;
         end if;
   end Load;

begin
   Load ("demo.gpr");
   Load ("demo2.gpr");
   Load ("demo3.gpr");
end Main;
