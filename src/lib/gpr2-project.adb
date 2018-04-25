------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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

with GPR2.Containers;

with GNAT.OS_Lib;
with GNAT.String_Split;

package body GPR2.Project is

   use Ada;

   GPRls : constant GNAT.OS_Lib.String_Access :=
             GNAT.OS_Lib.Locate_Exec_On_Path ("gprls");

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
                (Directories.Current_Directory & DS & String (Name))
            then
               return Path_Name.Create
                 (GPR_Name,
                  Name_Type (OS_Lib.Normalize_Pathname
                    (Directories.Current_Directory & DS & String (Name))));
            end if;

         else
            for P of Paths loop
               if Directories.Exists
                 (String (Path_Name.Dir_Name (P)) & DS & String (GPR_Name))
               then
                  return Path_Name.Create
                    (GPR_Name,
                     Name_Type (OS_Lib.Normalize_Pathname
                       (String (Path_Name.Dir_Name (P))
                        & DS & String (GPR_Name))));
               end if;
            end loop;
         end if;
      end if;

      return Path_Name.Create (GPR_Name, GPR_Name);
   end Create;

   -----------
   -- Paths --
   -----------

   function Paths
     (Parent : Path_Name.Object) return Path_Name.Set.Object
   is

      use type Containers.Count_Type;
      use type GNAT.OS_Lib.String_Access;
      use type Path_Name.Object;

      procedure Append
        (Result : in out Path_Name.Set.Object; Value : String)
        with Post => (if Value'Length = 0
                      then Result'Old.Length = Result.Length
                      else Result'Old.Length + 1 = Result.Length);

      procedure Add_List
        (Result : in out Path_Name.Set.Object;
         Values : String)
        with Post => Result'Old.Length <= Result.Length;
      --  Add list Values (which has OS dependant path separator) into Result

      --------------
      -- Add_List --
      --------------

      procedure Add_List
        (Result : in out Path_Name.Set.Object;
         Values : String)
      is
         use GNAT;

         V  : String_Split.Slice_Set;
      begin
         String_Split.Create (V, Values, String'(1 => OS_Lib.Path_Separator));

         for K in 1 .. String_Split.Slice_Count (V) loop
            Append (Result, String_Split.Slice (V, K));
         end loop;
      end Add_List;

      ------------
      -- Append --
      ------------

      procedure Append
        (Result : in out Path_Name.Set.Object;
         Value  : String) is
      begin
         if Value /= "" then
            Result.Append (Path_Name.Create_Directory (Name_Type (Value)));
         end if;
      end Append;

      Result : Path_Name.Set.Object;

   begin
      --  First check in parent project directory

      if Parent /= Path_Name.Undefined then
         Result.Append (Parent);
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
                  Append (Result, Buffer (1 .. Last));
               end loop;

               Text_IO.Close (File);
            end if;
         end;
      end if;

      --  Then in GPR_PROJECT_PATH and ADA_PROJECT_PATH

      if Environment_Variables.Exists ("GPR_PROJECT_PATH") then
         Add_List (Result, Environment_Variables.Value ("GPR_PROJECT_PATH"));
      end if;

      if Environment_Variables.Exists ("ADA_PROJECT_PATH") then
         Add_List (Result, Environment_Variables.Value ("ADA_PROJECT_PATH"));
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

            Append
              (Result,
               Directories.Compose
                (Directories.Compose (Prefix, "share"), "gpr"));

            --  <prefix>/lib/gnat

            Append
              (Result,
               Directories.Compose
                (Directories.Compose (Prefix, "lib"), "gnat"));
         end;
      end if;

      return Result;
   end Paths;

end GPR2.Project;
