------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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
with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Project.Tree;

procedure GPRdump is
   use Ada;
   use Ada.Exceptions;

begin
   if Command_Line.Argument_Count = 0 then
      Text_IO.Put_Line ("usage: gprdump <project>");
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   else
      declare
         Pathname : constant GPR2.Path_Name_Type :=
                      GPR2.Project.Create
                        (GPR2.Optional_Name_Type
                           (Command_Line.Argument (1)));
         Project  : GPR2.Project.Tree.Object;
         Context  : GPR2.Context.Object;
      begin
         Project.Load (Pathname, Context);
      exception
         when others =>
            for M of Project.Log_Messages.all loop
               Text_IO.Put_Line (M.Format);
            end loop;
      end;
   end if;

exception
   when E : others =>
      Text_IO.Put_Line ("cannot parse project: " & Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRdump;
