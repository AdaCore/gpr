------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Tree;
with GPR2.Project.Typ;

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

      for T of Prj.Root_Project.Types loop
         Text_IO.Put ("Type : " & String (T.Name.Text) & " -");

         for V of T.Values loop
            Text_IO.Put (' ' & V.Text);
         end loop;

         Text_IO.New_Line;
      end loop;

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
                    Strings.Fixed.Index (Mes, "/types");
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
   Load ("demo4-child.gpr");
end Main;
