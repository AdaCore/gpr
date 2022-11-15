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
with Ada.Text_IO;

package body GPRinstall is

   use Ada;

   ----------------------------
   -- Delete_Empty_Directory --
   ----------------------------

   procedure Delete_Empty_Directory (Prefix, Dir_Name : String) is
      use Ada.Directories;

      Prefix_Dir_Len : constant Natural := Prefix'Length - 1;
      Search         : Search_Type;
      Element        : Directory_Entry_Type;
      To_Delete      : Boolean := True;
   begin
      --  Do not try to remove a directory past the project dir

      if Dir_Name'Length >= Prefix_Dir_Len then
         --  Check whether the directory is empty or not

         if Exists (Dir_Name) then
            Start_Search (Search, Dir_Name, Pattern => "");

            Check_Entry : while More_Entries (Search) loop
               Get_Next_Entry (Search, Element);

               if Directories.Simple_Name (Element) not in "." | ".." then
                  To_Delete := False;
                  exit Check_Entry;
               end if;
            end loop Check_Entry;

            End_Search (Search);

         else
            To_Delete := False;
         end if;

         --  If empty delete it

         if To_Delete then
            begin
               Delete_Directory (Dir_Name);
            exception
                  --  This can happen if there is still some sym links into
                  --  the directory.
               when Text_IO.Use_Error =>
                  null;
            end;
         end if;

         --  And then try recursively with parent directory

         Delete_Empty_Directory (Prefix, Containing_Directory (Dir_Name));
      end if;
   end Delete_Empty_Directory;

end GPRinstall;
