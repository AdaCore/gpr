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

with Ada.Exceptions;

with GPRname.Common;
with GPRname.Options;
with GPRname.Process;

with GPR2.Interrupt_Handler;

with GPRtools.Sigint;
with GPRtools.Util;

procedure GPRname.Main is

   Opt : GPRname.Options.Object;

begin
   --  Install the Ctrl-C handler

   GPR2.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   GPRtools.Util.Set_Program_Name ("gprname");

   --  Parse arguments

   Opt.Build_From_Command_Line;

   --  Run the gprname main procedure

   GPRname.Process (Opt);

exception

   when E : GPRname.Common.GPRname_Exception =>
      GPRtools.Util.Fail_Program (Ada.Exceptions.Exception_Message (E));

   when E : others =>
      GPRtools.Util.Fail_Program
        ("error: " & Ada.Exceptions.Exception_Information (E));
end GPRname.Main;
