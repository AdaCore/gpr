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

--  Common utilities for all gpr tools

with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with GPRtools.Options;

package GPRtools.Util is

   type Exit_Code_Type is
     (E_Success,    -- No warnings or errors
      E_Warnings,   -- Compiler warnings generated
      E_No_Code,    -- No code generated
      E_No_Compile, -- Compilation not needed (smart recompilation)
      E_Errors,     -- Compiler error messages generated
      E_Fatal,      -- Fatal (serious) error, e.g. source file not found
      E_Abort);     -- Internally detected compiler error

   procedure Set_Program_Name (Name : String);

   procedure Output_Messages (Options : GPRtools.Options.Object'Class);
   --  Output errors and if Verbose is True other messages from log

   -------------------------
   -- Program termination --
   -------------------------

   procedure Fail_Program (Message : String);
   --  Terminate program with a message and a fatal status code

   procedure Project_Processing_Failed
     (Options : GPRtools.Options.Object'Class);
   --  Output or not project processing error messages depend on Verbose
   --  parameters. Output error message '"proj.gpr" processing failed' at the
   --  end if not Quiet.

   function Look_For_Default_Project
     (Quiet : Boolean; Implicit_Only : Boolean) return GPR2.Path_Name.Object;
   --  Calls GPR2.Project.Look_For_Default_Project and print returned project
   --  if not Quiet.

   procedure Finish_Program
     (Exit_Code : Exit_Code_Type := E_Success;
      Message   : String := "");
   --  Terminate program, with or without a message, setting the status code
   --  according to Exit_Code.

end GPRtools.Util;
