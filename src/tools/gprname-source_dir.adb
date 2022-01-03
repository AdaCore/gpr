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

with Ada.Strings.Fixed;

with GNAT.OS_Lib;

package body GPRname.Source_Dir is

   ------------
   -- Create --
   ------------

   function Create
     (Name      : Filename_Type;
      Directory : Filename_Optional := No_Filename) return Object
   is
      use Ada.Strings;

      Is_Recursive : constant Boolean :=
                       (Name'Length >= 2
                        and then Fixed.Tail (String (Name), 2) = "**");
      PN           : Path_Name.Object;

      Dir : constant Filename_Type :=
            ((if Directory = No_Filename then ""
             else Directory & GNAT.OS_Lib.Directory_Separator) & Name);

   begin
      if Is_Recursive then
         PN := Path_Name.Create_Directory
           (Filename_Type (Fixed.Head (String (Dir), Dir'Length - 2)));
      else
         PN := Path_Name.Create_Directory (Dir);
      end if;

      if not PN.Exists then
         raise GPRname_Exception with "invalid source directory: " & PN.Value;
      end if;

      return (PN, Is_Recursive, +String (Name));
   end Create;

end GPRname.Source_Dir;
