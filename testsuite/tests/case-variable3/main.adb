------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Project.Tree;
with GPR2.Context;

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
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               Mes : constant String := M.Format;
               L   : constant Natural :=
                       Strings.Fixed.Index (Mes, "/variables3");
            begin
               if L /= 0 then
                  Text_IO.Put_Line (Mes (L .. Mes'Last));
               else
                  Text_IO.Put_Line (Mes);
               end if;
            end;
         end loop;
      end if;
end Main;
