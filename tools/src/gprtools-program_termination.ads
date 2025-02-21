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

package GPRtools.Program_Termination is

   use Ada;

   E_Program_Termination : exception;

   type Exit_Code_Type is
     (E_Success,    -- No warnings or errors
      E_Warnings,   -- Compiler warnings generated
      E_No_Code,    -- No code generated
      E_General,    -- General missusage error
      E_Errors,     -- Compiler error messages generated
      E_Project,    -- Issued when the project could not load
      E_Fatal,      -- Fatal (serious) error, e.g. source file not found
      E_Abort);     -- Internally detected compiler error

   type Exit_Cause_Type is
     (E_Generic,    -- Generic error, will prefix the message with "error:"
      E_Tool);      -- Tool error, will prefix the message with the tool name

   procedure Handle_Program_Termination
     (Display_Command_Line_Help : Boolean := False;
      Force_Exit                : Boolean := True;
      Exit_Code                 : Exit_Code_Type  := E_Fatal;
      Exit_Cause                : Exit_Cause_Type := E_Tool;
      Message                   : String := "");
   --  This procedure handles how should a program be terminated, it can
   --  display command line help, set exit code and force the exit of the
   --  program.
   --  The exact process will be determined by the parameters.
   --     - Display_Command_Line_Help : Whether or not it should print the
   --                                   command line help.
   --     - Force_Exit : Whether or not it should force program exit.
   --                    - True  : E_Program_Termination is raised
   --                    - False : The program will proceed but Set_Exit_Status
   --                              will be called with Exit_Code.
   --     - Exit_Code : Which exit code should be emitted.
   --     - Exit_Cause : Prefixes the Message with :
   --                    - E_Generic : "error:"
   --                    - E_Tool    : "<toolname>:"
   --                    for E_Tool to properly work, the tools needs to
   --                    register its name with GPRtools.Util.Set_Program_Name
   --     - Message : Error message to be displayed.

   function To_Exit_Status
     (Code : Exit_Code_Type) return Command_Line.Exit_Status;
   --  Translate Code as Exit_Status

end GPRtools.Program_Termination;
