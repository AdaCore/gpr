------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with GPR2.Log;

package body GPRtools.Util is

   Keep_Program_Name : Unbounded_String;

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   pragma No_Return (Exit_Program);
   --  A call to Exit_Program terminates execution with the given status.
   --  A status of zero indicates normal completion, a non-zero status
   --  indicates abnormal termination.

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Exit_Code : Exit_Code_Type) is
   begin
      --  The program will exit with the following status:

      --    0 if the object file has been generated (with or without warnings)
      --    1 if recompilation was not needed (smart recompilation)
      --    4 for a fatal error
      --    5 if there were errors
      --    6 if no code has been generated (spec)

      --  Note that exit code 3 is not used and must not be used as this is
      --  the code returned by a program aborted via C abort() routine on
      --  Windows. GCC checks for that case and thinks that the child process
      --  has been aborted. This code (exit code 3) used to be the code used
      --  for E_No_Code, but E_No_Code was changed to 6 for this reason.

      case Exit_Code is
         when E_Success    => OS_Exit (0);
         when E_Warnings   => OS_Exit (0);
         when E_No_Compile => OS_Exit (1);
         when E_Fatal      => OS_Exit (4);
         when E_Errors     => OS_Exit (5);
         when E_No_Code    => OS_Exit (6);
         when E_Abort      => OS_Abort;
      end case;
   end Exit_Program;

   ------------------
   -- Fail_Program --
   ------------------

   procedure Fail_Program (Message : String) is
   begin
      Finish_Program (E_Fatal, Message);
   end Fail_Program;

   --------------------
   -- Finish_Program --
   --------------------

   procedure Finish_Program
     (Exit_Code : Exit_Code_Type := E_Success;
      Message   : String := "") is
   begin
      if Message'Length > 0 then
         Put_Line
           ((if Exit_Code = E_Success
             then Standard_Output
             else Standard_Error),
            To_String (Keep_Program_Name) & ": " & Message);
      end if;

      Exit_Program (Exit_Code);
   end Finish_Program;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages
     (Tree    : GPR2.Project.Tree.Object;
      Verbose : Boolean;
      Output  : Ada.Text_IO.File_Type) is
   begin
      for C in Tree.Log_Messages.Iterate
        (Information => Verbose, Warning => Verbose,
         Error => True, Read => True, Unread => True)
      loop
         Put_Line (Output, GPR2.Log.Element (C).Format);
      end loop;
   end Output_Messages;

   -------------------------------
   -- Project_Processing_Failed --
   -------------------------------

   procedure Project_Processing_Failed
     (Tree : GPR2.Project.Tree.Object; Verbose : Boolean) is
   begin
      Output_Messages (Tree, Verbose, Standard_Error);
      Fail_Program
        ('"' & String (Tree.Root_Project.Path_Name.Simple_Name)
         & """ processing failed");
   end Project_Processing_Failed;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name (Name : String) is
   begin
      Keep_Program_Name := To_Unbounded_String (Name);
   end Set_Program_Name;

end GPRtools.Util;
