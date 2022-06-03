------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

with Ada.Command_Line;

with GPR2.Log;
with GPR2.Path_Name;

with GPRtools.Options;

package GPRtools.Util is

   use GPR2;

   type Exit_Code_Type is
     (E_Success,    -- No warnings or errors
      E_Warnings,   -- Compiler warnings generated
      E_No_Code,    -- No code generated
      E_No_Compile, -- Compilation not needed (smart recompilation)
      E_Errors,     -- Compiler error messages generated
      E_Fatal,      -- Fatal (serious) error, e.g. source file not found
      E_Abort);     -- Internally detected compiler error

   procedure Set_Program_Name (Name : String);

   procedure Output_Messages
     (Options : GPRtools.Options.Base_Options'Class;
      Log     : GPR2.Log.Object := GPR2.Log.Undefined);
   --  Output errors and if Verbose is True other messages from Log.
   --  Options Tree's log is used when Log is undefined.

   function Is_Ada_Predefined_Unit (Unit : Name_Type) return Boolean;
   --  Return True if Unit is an Ada runtime unit

   function Executable_Prefix_Path return String;
   --  Return the absolute path parent directory of the directory where the
   --  current executable resides, if its directory is named "bin", otherwise
   --  return an empty string. When a directory is returned, it is guaranteed
   --  to end with a directory separator.

   -------------------------
   -- Program termination --
   -------------------------

   procedure Fail_Program (Message : String);
   --  Terminate program with a message and a fatal status code

   procedure Project_Processing_Failed
     (Options : GPRtools.Options.Base_Options'Class);
   --  Output or not project processing error messages depend on Verbose
   --  parameters. Output error message '"proj.gpr" processing failed' at the
   --  end if not Quiet.

   function Check_For_Default_Project return GPR2.Path_Name.Object;
   --  Look for default project in the current directory,
   --  return Undefined if no or several projects are in the current
   --  directory.

   procedure Finish_Program
     (Exit_Code : Exit_Code_Type := E_Success;
      Message   : String := "");
   --  Terminate program, with or without a message, setting the status code
   --  according to Exit_Code.

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   pragma No_Return (Exit_Program);
   --  A call to Exit_Program terminates execution with the given status.
   --  A status of zero indicates normal completion, a non-zero status
   --  indicates abnormal termination.

   function Exit_Code
     (Code : Exit_Code_Type) return Ada.Command_Line.Exit_Status;
   --  Translate Code as Exit_Status

   function Partial_Name
     (Lib_Name      : Simple_Name;
      Number        : Natural;
      Object_Suffix : Simple_Name) return Simple_Name;
   --  Returns the name of an object file created by the partial linker

end GPRtools.Util;
