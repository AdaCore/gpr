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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Projects : constant array (1 .. 3) of String (1 .. 1) := ("a", "b", "c");

begin
   for P of Projects loop
      declare
         Prj : Project.Tree.Object;
         Ctx : Context.Object;

      begin
         Project.Tree.Load (Prj, Create (Filename_Type (P)), Ctx);
         Text_IO.Put_Line ("All good, no message.");

      exception
         when GPR2.Project_Error =>
            if Prj.Has_Messages then
               Text_IO.Put_Line ("Messages found:");

               for C in Prj.Log_Messages.Iterate
                 (False, False, True, True, True)
               loop
                  declare
                     Mes : constant String := Log.Element (C).Format;
                     F   : constant Natural :=
                             Strings.Fixed.Index (Mes, "imports ");
                     L   : constant Natural :=
                             Strings.Fixed.Index (Mes, "/limited-refs");
                  begin
                     if F /= 0 and then L /= 0 then
                        Text_IO.Put_Line
                          (Mes (1 .. F + 7) & Mes (L .. Mes'Last));
                     else
                        Text_IO.Put_Line (Mes);
                     end if;
                  end;
               end loop;
            end if;
      end;
   end loop;
end Main;
