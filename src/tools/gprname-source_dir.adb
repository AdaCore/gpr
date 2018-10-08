------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

package body GPRname.Source_Dir is

   ------------
   -- Create --
   ------------

   function Create
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
   is
      use Ada.Strings;

      Is_Recursive : constant Boolean :=
                       (Name'Length >= 2
                        and then Fixed.Tail (String (Name), 2) = "**");
      PN           : Path_Name.Object;

   begin
      if Is_Recursive then
         PN := Path_Name.Create_Directory
           (Name_Type (Fixed.Head (String (Name), Name'Length - 2)),
            Directory);
      else
         PN := Path_Name.Create_Directory (Name, Directory);
      end if;

      if not PN.Exists then
         raise GPRname_Exception with "invalid source directory: " & PN.Value;
      end if;

      return (PN, Is_Recursive, +String (Name));
   end Create;

end GPRname.Source_Dir;
