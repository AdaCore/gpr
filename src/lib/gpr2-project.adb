------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body GPR2.Project is

   use Ada;

   GPRls : constant GNAT.OS_Lib.String_Access :=
             GNAT.OS_Lib.Locate_Exec_On_Path ("gprls");

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Name_Type;
      Paths : Containers.Name_List := Containers.Name_Type_List.Empty_Vector)
      return Path_Name_Type
   is
      use GNAT;

      function "+"
        (Str : String) return Unbounded_String renames To_Unbounded_String;

      function Create (Name, Path_Name : String) return Path_Name_Type
        is (Path_Name_Type'
              (As_Is     => +Name,
               Value     => +Path_Name,
               Base_Name => +Directories.Base_Name (Path_Name),
               Dir_Name  => +Directories.Containing_Directory (Path_Name)));

      DS       : constant Character := OS_Lib.Directory_Separator;

      GPR_Name : constant String :=
                   (if Directories.Extension (String (Name)) in "gpr" | "cgpr"
                    then String (Name)
                    else String (Name) & ".gpr");

   begin
      --  If the file exists or an absolute path has been specificed or there
      --  is no ADA_PROJECT_PATH, just create the Path_Name_Type using the
      --  given Name.

      if OS_Lib.Is_Absolute_Path (GPR_Name) then
         return Create (GPR_Name, OS_Lib.Normalize_Pathname (GPR_Name));

      else
         --  If we have an empty Paths set, this is the root project and it is
         --  expected to look into the current working directorty in this case.

         if Paths.Is_Empty then
            if Directories.Exists
                (Directories.Current_Directory & DS & String (Name))
            then
               return Create
                 (GPR_Name,
                  OS_Lib.Normalize_Pathname
                    (Directories.Current_Directory & DS & String (Name)));
            end if;

         else
            for P of Paths loop
               if Directories.Exists (String (P) & DS & GPR_Name) then
                  return Create
                    (GPR_Name,
                     OS_Lib.Normalize_Pathname (String (P) & DS & GPR_Name));
               end if;
            end loop;
         end if;
      end if;

      return Create (GPR_Name, GPR_Name);
   end Create;

   -----------
   -- Paths --
   -----------

   function Paths (Parent : Path_Name_Type) return Containers.Name_List is
      use type GNAT.OS_Lib.String_Access;

      Result : Containers.Name_List;
   begin
      --  First check in parent project directory

      if Parent /= No_Path_Name then
         Result.Append (Name_Type (Dir_Name (Parent)));
      end if;

      --  Then -aP switches if any
      --  ??? not yet supported

      --  Then in GPR_PROJECT_PATH_FILE, one path per line

      if Environment_Variables.Exists ("GPR_PROJECT_PATH_FILE") then
         declare
            Filename : constant String :=
                         Environment_Variables.Value ("GPR_PROJECT_PATH_FILE");
            Buffer   : String (1 .. 1024);
            Last     : Natural;
            File     : Text_IO.File_Type;
         begin
            if Directories.Exists (Filename) then
               Text_IO.Open (File, Text_IO.In_File, Filename);

               while not Text_IO.End_Of_File (File) loop
                  Text_IO.Get_Line (File, Buffer, Last);
                  Result.Append (Name_Type (Buffer (1 .. Last)));
               end loop;

               Text_IO.Close (File);
            end if;
         end;
      end if;

      --  Then in GPR_PROJECT_PATH and ADA_PROJECT_PATH

      if Environment_Variables.Exists ("GPR_PROJECT_PATH") then
         Result.Append
           (Name_Type (Environment_Variables.Value ("GPR_PROJECT_PATH")));
      end if;

      if Environment_Variables.Exists ("ADA_PROJECT_PATH") then
         Result.Append
           (Name_Type (Environment_Variables.Value ("ADA_PROJECT_PATH")));
      end if;

      --  Then target specific directory if specified
      --  ??? not yet supported

      if GPRls /= null then
         declare
            Prefix : constant String :=
                       Directories.Containing_Directory
                         (Directories.Containing_Directory (GPRls.all));
         begin
            --  <prefix>/share/gpr

            Result.Append
              (Name_Type
                 (Directories.Compose
                    (Directories.Compose (Prefix, "share"), "gpr")));

            --  <prefix>/lib/gnat

            Result.Append
              (Name_Type
                 (Directories.Compose
                   (Directories.Compose (Prefix, "lib"), "gnat")));
         end;
      end if;

      return Result;
   end Paths;

end GPR2.Project;
