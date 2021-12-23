------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with GNAT.Command_Line;

with GPR2.Interrupt_Handler;
with GPR2.Project.Tree;

with GPRls.Options;
with GPRls.Process;

with GPRtools.Sigint;
with GPRtools.Util;

procedure GPRls.Main is

   use Ada;
   use Ada.Exceptions;

   use GPR2;

   Opt  : Options.Object;
   Tree : Project.Tree.Object;

begin
   --  Install the Ctrl-C handler

   GPR2.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprls");

   --  Parse arguments

   Opt.Tree := Tree.Reference;
   Opt.Build_From_Command_Line;

   --  Run the gprls main procedure

   GPRls.Process (Opt);

exception
   when GNAT.Command_Line.Exit_From_Command_Line
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter
      =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Options.Usage_Error =>
      Text_IO.Put_Line ("gprls: " & Exception_Message (E));
      GNAT.Command_Line.Try_Help;

   when E : others =>
      Text_IO.Put_Line ("error: " & Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end GPRls.Main;
