------------------------------------------------------------------------------
--                                                                          --
--                        GPR2 PROJECT MANAGER                              --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Containers;

--  Executable that takes as input a command line of one or more commands,
--  each command being separated by the '&&' characters.
--  All commands are executed in order and only when the previous command
--  has finished.
--
--  If one command fails, the subsequent commands are not executed.

function Processes_Wrapper.Main return Integer is
   package CLI renames Ada.Command_Line;

   function "=" (Left, Right : Argument_List) return Boolean;
   --  Returns True if all the arguments of both argument lists are equal

   function "=" (Left, Right : Argument_List) return Boolean is
      use Arg_Lists;
      use type Ada.Containers.Count_Type;
      Left_Cursor  : Cursor := Left.First;
      Right_Cursor : Cursor := Right.First;
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      while Left_Cursor /= No_Element loop
         if Element (Left_Cursor) /= Element (Right_Cursor) then
            return False;
         end if;

         Next (Left_Cursor);
         Next (Right_Cursor);
      end loop;

      return True;
   end "=";

   package Command_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Argument_List, "=" => "=");

   Commands : Command_Vectors.Vector;
begin
   declare
      Command : Argument_List;
   begin
      for I in 1 .. CLI.Argument_Count loop
         if CLI.Argument (I) = "&&" then
            if Command.Is_Empty then
               Ada.Text_IO.Put_Line ("Error, no command provided before '&&'");
               return 1;
            else
               Commands.Append (Command);
               Command.Clear;
            end if;
         else
            Command.Append (CLI.Argument (I));

            if I = CLI.Argument_Count then
               Commands.Append (Command);
            end if;
         end if;
      end loop;
   end;

   for Command of Commands loop
      declare
         Proc_Handle : constant Process_Handle := Start (Command);
         Ret         : constant Integer        := Wait (Proc_Handle);
      begin
         if Ret /= 0 then

            --  Do not execute remaining commands if the current one failed

            return Ret;
         end if;
      end;
   end loop;

   return 0;
exception
   when Ex : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Ex));
      return 1;
end Processes_Wrapper.Main;
