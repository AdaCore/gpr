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

with GNAT.OS_Lib;

private with Ada.Strings.Unbounded;
private with GPR2.Path_Name.Set;

package GPRinstall is

   DS : constant Character := GNAT.OS_Lib.Directory_Separator;

private

   Sig_Line : constant String := "S ";
   --  The prefix of the line containing the original project's signature

   Search_Paths : GPR2.Path_Name.Set.Object;

   procedure Delete_Registered_Directory (Root_Dir : String);
   --  Delete all registered directories

   procedure Register_Directory (Dir : String);
   --  Register directories inside a map to be deleted by calling
   --  Delete_Registered_Directory.

   GPRinstall_Error : exception;
   --  General GPRinstall exception that is supposed to have associated error
   --  message attached.

   GPRinstall_Error_No_Message : exception;
   --  Special exception for cases that require multiple line diagnostics
   --  that are supposed to be printed before raising the exception.

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;
   function "-" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

end GPRinstall;
