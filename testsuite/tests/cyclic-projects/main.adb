--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Load (Filename : Filename_Type);

   ----------
   -- Load --
   ----------

   procedure Load (Filename : Filename_Type) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
   begin
      Project.Tree.Load (Prj, Create (Filename), Ctx);

   exception
      when GPR2.Project_Error =>
         if Prj.Has_Messages then
            Text_IO.Put_Line ("Messages found for " & String (Filename));

            for C in Prj.Log_Messages.Iterate
              (False, False, True, True, True)
            loop
               declare
                  Mes : constant String := Log.Element (C).Format;
                  F   : constant Natural :=
                          Strings.Fixed.Index (Mes, "imports ");
                  L   : constant Natural :=
                          Strings.Fixed.Index (Mes, "cyclic-projects");
               begin
                  if F /= 0 and then L /= 0 then
                     Text_IO.Put_Line
                       (Mes (1 .. F + 7) & Mes (L - 1 .. Mes'Last));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;
         end if;
   end Load;

begin
   Load ("a.gpr");     --  a -> b -> d -> a
   Load ("agg.gpr");   --  no circularity
   Load ("agg2.gpr");  --  agg2 -> f -> agg2
   Load ("multi.gpr"); --  multiple circularities
end Main;
