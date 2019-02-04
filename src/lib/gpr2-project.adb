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

with Ada.Directories;

with GNAT.OS_Lib;

package body GPR2.Project is

   use Ada;

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Name_Type;
      Paths : Path_Name.Set.Object := Path_Name.Set.Set.Empty_List)
      return GPR2.Path_Name.Object
   is
      use GNAT;

      DS       : constant Character := OS_Lib.Directory_Separator;

      GPR_Name : constant Name_Type :=
                   (if Directories.Extension (String (Name)) in "gpr" | "cgpr"
                    then Name
                    else Name & ".gpr");

   begin
      --  If the file exists or an absolute path has been specificed or there
      --  is no ADA_PROJECT_PATH, just create the Path_Name_Type using the
      --  given Name.

      if OS_Lib.Is_Absolute_Path (String (GPR_Name)) then
         return Path_Name.Create
           (GPR_Name,
            Name_Type (OS_Lib.Normalize_Pathname (String (GPR_Name))));

      else
         --  If we have an empty Paths set, this is the root project and it is
         --  expected to look into the current working directorty in this case.

         if Paths.Is_Empty then
            if Directories.Exists
                (Directories.Current_Directory & DS & String (GPR_Name))
            then
               return Path_Name.Create
                 (GPR_Name,
                  Name_Type (OS_Lib.Normalize_Pathname
                    (Directories.Current_Directory & DS & String (GPR_Name))));
            end if;

         else
            for P of Paths loop
               declare
                  F_Name : constant String :=
                             String (Path_Name.Dir_Name (P))
                             & String (GPR_Name);
               begin
                  if Directories.Exists (F_Name) then
                     return Path_Name.Create
                       (GPR_Name,
                        Name_Type (OS_Lib.Normalize_Pathname (F_Name)));
                  end if;
               end;
            end loop;
         end if;
      end if;

      return Path_Name.Create_File
        (GPR_Name, Name_Type (Directories.Current_Directory));
   end Create;

   ------------------------------
   -- Look_For_Default_Project --
   ------------------------------

   function Look_For_Default_Project return Path_Name.Object is
      use Directories;
      Default_Name : constant String := "default.gpr";
      Search       : Search_Type;
      Item         : Directory_Entry_Type;
   begin
      if Exists (Default_Name)
        and then Kind (Default_Name) = Ordinary_File
      then
         return Path_Name.Create_File (Name_Type (Default_Name));
      end if;

      Start_Search
        (Search, ".", "*.gpr", (Ordinary_File => True, others => False));

      if More_Entries (Search) then
         Get_Next_Entry (Search, Item);

         if More_Entries (Search) then
            --  Only one project in current directory can be default one
            return Path_Name.Undefined;
         else
            return Path_Name.Create_File (Name_Type (Full_Name (Item)));
         end if;

      else
         return Path_Name.Undefined;
      end if;
   end Look_For_Default_Project;

   ------------------
   -- Search_Paths --
   ------------------

   function Search_Paths
     (Root_Project      : Path_Name.Object;
      Tree_Search_Paths : Path_Name.Set.Object) return Path_Name.Set.Object is
   begin
      return Result : Path_Name.Set.Object := Tree_Search_Paths do
         Result.Prepend
           (Path_Name.Create_Directory (Name_Type (Root_Project.Dir_Name)));
      end return;
   end Search_Paths;

end GPR2.Project;
