------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with GPR2.Interrupt_Handler;
with GPR2.Project.Tree;

with GPRls.Options;
with GPRls.Process;

with GPRtools.Program_Termination;
with GPRtools.Sigint;
with GPRtools.Util;

function GPRls.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use Ada.Exceptions;
   use GPR2;
   use GPRtools.Program_Termination;

   Opt  : Options.Object;
   Tree : Project.Tree.Object;

begin
   --  Install the Ctrl-C handler

   GPR2.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprls");

   --  Parse arguments and load the project tree

   Opt.Tree := Tree.Reference;
   if not Opt.Build_From_Command_Line then
      Handle_Program_Termination
        (Opt     => Opt,
         Message => "");
   end if;

   --  Run the gprls main procedure

   GPRls.Process (Opt);

   return To_Exit_Status (E_Success);

exception
   when Project_Error | Processing_Error =>
      Handle_Program_Termination
        (Opt                   => Opt,
         Display_Tree_Messages => True,
         Force_Exit            => False,
         Message               => '"' & String (Opt.Filename.Name)
         & """ processing failed");
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Opt        => Opt,
         Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRls.Main;
