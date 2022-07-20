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

with Ada.Directories;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

package body GPRname.Source_Dir is

   ------------
   -- Create --
   ------------

   function Create
     (Name      : Filename_Type;
      Directory : Filename_Optional := No_Filename)
      return Dir_And_Optional_File
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

      if Is_Recursive
      then
         PN := Path_Name.Create_Directory
           (Filename_Type (Fixed.Head
            (String (Dir), Dir'Length - 2)));

         if not PN.Exists then
            raise GPRname_Exception with "invalid source directory: "
              & PN.Value;
         end if;

         return Dir_And_Optional_File'
           (Has_File => False,
            Dir      => (PN, Is_Recursive, +String (Name)));
      else
         if Ada.Directories.Exists (Name => String (Dir))
         then
            if GNAT.OS_Lib.Is_Directory (Name => String (Dir))
            then
               PN := Path_Name.Create_Directory (Dir);

               if not PN.Exists then
                  raise GPRname_Exception with "invalid source directory: "
                    & PN.Value;
               end if;

               return Dir_And_Optional_File'
                 (Has_File => False,
                  Dir      => (PN, Is_Recursive, +String (Name)));
            else
               declare
                  Realdir  : constant Filename_Type :=
                    Filename_Type (Ada.Directories.Containing_Directory
                                   (String (Dir)));
                  Realname : constant Filename_Type :=
                    Filename_Type (Ada.Directories.Containing_Directory
                                   (String (Name)));
               begin
                  PN := Path_Name.Create_Directory (Realdir);

                  if not PN.Exists then
                     raise GPRname_Exception with "invalid source directory: "
                       & PN.Value;
                  end if;

                  return Dir_And_Optional_File'
                    (Has_File => True,
                     Dir      => (PN, Is_Recursive, +String (Realname)),
                     File     => GPR2.Path_Name.Create_File
                       (Name => Name)
                    );
               end;
            end if;
         else
            raise GPRname_Exception with "invalid source: " & String (Dir);
         end if;
      end if;

   end Create;

end GPRname.Source_Dir;
