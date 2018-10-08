------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

private with GPR2.Path_Name.Set;

package GPRinstall is

   DS : constant Character := GNAT.OS_Lib.Directory_Separator;

private

   Sig_Line : constant String := "S ";
   --  The prefix of the line containing the original project's signature

   Search_Paths : GPR2.Path_Name.Set.Object;

   procedure Delete_Empty_Directory (Prefix, Dir_Name : String);
   --  Delete Dir_Name if empty, if removed try with parent directory but not
   --  above the given prefix.

end GPRinstall;
