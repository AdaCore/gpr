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

with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Path_Name;
with GPR2.Context;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;

with P1;

procedure Check_Mem is
   use Ada.Command_Line;
   use GPR2;

   Count : constant Natural :=
             (if Argument_Count > 0 then Integer'Value (Argument (1)) else 1);
begin
   P1; -- Call generated dummy sources

   for J in 1 .. Count loop
      declare
         T : Project.Tree.Object;
      begin
         T.Load_Autoconf
           (Path_Name.Create_File ("check_mem.gpr"), Context.Empty);

         for J of T.Root_Project.Sources loop
            exit;
         end loop;

         if Argument_Count > 1 then
            for V of T loop
               Ada.Text_IO.Put_Line (V.Path_Name.Value);
               for S of V.Sources loop
                  Ada.Text_IO.Put_Line (ASCII.HT & S.Path_Name.Value);
               end loop;
            end loop;
         end if;
      end;
   end loop;
end Check_Mem;
