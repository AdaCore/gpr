--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source;

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
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      for Source of Prj.Root_Project.Sources loop
         null;
      end loop;
   exception
      when GPR2.Project_Error =>
         if Prj.Has_Messages then
            Text_IO.Put_Line ("Messages found:");

            for C in Prj.Log_Messages.Iterate
              (False, False, True, False, True, True)
            loop
               declare
                  M   : constant Message.Object := Log.Element (C);
                  Mes : constant String := M.Format;
                  L   : constant Natural :=
                          Strings.Fixed.Index
                            (Mes, "/source-missing-interface");
               begin
                  if L /= 0 then
                     Text_IO.Put_Line (Mes (L .. Mes'Last));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;
         end if;
   end Check;

begin
   Check ("demo.gpr");
end Main;
