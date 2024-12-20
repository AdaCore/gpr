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

with Ada.Text_IO;

with GPRtools.Command_Line;
with GPRtools.Util;

package body GPRtools.Program_Termination is

   --------------------------------
   -- Handle_Program_Termination --
   --------------------------------

   procedure Handle_Program_Termination
     (Display_Command_Line_Help : Boolean := False;
      Force_Exit                : Boolean := True;
      Exit_Code                 : Exit_Code_Type  := E_Fatal;
      Exit_Cause                : Exit_Cause_Type := E_Tool;
      Message                   : String := "") is
   begin
      if Message'Length > 0 then
         declare
            Output_Kind : constant Text_IO.File_Type :=
                            (if Exit_Code = E_Success
                             then Text_IO.Standard_Output
                             else Text_IO.Standard_Error);

            Complete_Message : constant String :=
                                 (if Exit_Cause = E_Tool
                                  then GPRtools.Util.Get_Program_Name
                                  else "error") & ": " & Message;
         begin
            Text_IO.Put_Line (Output_Kind, Complete_Message);
         end;
      end if;

      if Display_Command_Line_Help then
         GPRtools.Command_Line.Try_Help;
      end if;

      Ada.Command_Line.Set_Exit_Status (To_Exit_Status (Exit_Code));

      if Force_Exit then
         raise E_Program_Termination;
      end if;
   end Handle_Program_Termination;

   --------------------
   -- To_Exit_Status --
   --------------------

   function To_Exit_Status
     (Code : Exit_Code_Type) return Ada.Command_Line.Exit_Status is
   begin
      case Code is
         when E_Success    => return 0;
         when E_Warnings   => return 0;
         when E_General    => return 1;
         when E_Errors     => return 4;
         when E_Fatal      => return 5;
         when E_No_Code    => return 6;
         when E_Abort      => return 16#FF#;
      end case;
   end To_Exit_Status;

end GPRtools.Program_Termination;
