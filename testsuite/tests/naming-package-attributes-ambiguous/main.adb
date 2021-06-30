------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GNAT.OS_Lib;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Source;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

   procedure Display_Source (Name : Simple_Name);

   procedure Display_Source (Name : Simple_Name) is
      Src : GPR2.Source.Object;
   begin
      if Prj.Root_Project.Has_Source (Name) then
         Src := Prj.Root_Project.Source (Name).Source;
         Text_IO.Put_Line (String (Name) & ": " & Src.Kind'Image);
      else
         Text_IO.Put_Line ("no such source: " & String (Name));
      end if;
   end Display_Source;

begin
   Project.Tree.Load (Prj, Create ("./data/prj.gpr"), Ctx);

   Prj.Update_Sources;
   Display_Source ("pkg.a");
   Display_Source ("pkg_b.a");
   Display_Source ("pkg-execute_s_b.a");

exception
   when Project_Error =>

      Text_IO.Put_Line ("Messages found:");

      for C in Prj.Log_Messages.Iterate (False, True, True, True, True) loop
         declare
            use Ada.Strings;
            use Ada.Strings.Fixed;
            DS  : Character renames GNAT.OS_Lib.Directory_Separator;
            M   : constant Message.Object := Log.Element (C);
            Mes : constant String := M.Format;
            L   : constant Natural :=
                    Fixed.Index (Mes, DS & "aggregate-dup-src" & DS);
         begin
            if L /= 0 then
               Text_IO.Put_Line
                 (Replace_Slice
                    (Mes,
                     Fixed.Index (Mes (1 .. L), """", Going => Backward) + 1,
                     L - 1,
                     "<path>"));
            else
               Text_IO.Put_Line (Mes);
            end if;
         end;
      end loop;
end Main;
