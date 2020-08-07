------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;

with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.String_Split;

package body GPR2.Project is

   use GNAT;

   ---------------------------------
   -- Append_Default_Search_Paths --
   ---------------------------------

   procedure Append_Default_Search_Paths
     (Paths : in out Path_Name.Set.Object)
   is
      Prefix : constant String := Get_Tools_Directory;

      procedure Append (Value : String)
        with Post => (if Value'Length = 0
                      then Paths'Old.Length = Paths.Length
                          else Paths'Old.Length <= Paths.Length);
      --  Append Value into Paths is it is not empty and does not exists in
      --  Paths.

      procedure Add_List (Values : String)
        with Post => Paths'Old.Length <= Paths.Length;
      --  Add list Values (which has OS dependant path separator) into
      --  Paths.

      --------------
      -- Add_List --
      --------------

      procedure Add_List (Values : String) is
         V : String_Split.Slice_Set;
      begin
         String_Split.Create
           (V, Values, String'(1 => OS_Lib.Path_Separator));

         for K in 1 .. String_Split.Slice_Count (V) loop
            Append (String_Split.Slice (V, K));
         end loop;
      end Add_List;

      ------------
      -- Append --
      ------------

      procedure Append (Value : String) is
      begin
         if Value /= "" then
            declare
               Path : constant Path_Name.Object :=
                        Path_Name.Create_Directory (Name_Type (Value));
            begin
               if not Paths.Contains (Path) then
                  Paths.Append (Path);
               end if;
            end;
         end if;
      end Append;

   begin
      --  Then in GPR_PROJECT_PATH_FILE, one path per line

      if Environment_Variables.Exists ("GPR_PROJECT_PATH_FILE") then
         declare
            Filename : constant String :=
                         Environment_Variables.Value
                           ("GPR_PROJECT_PATH_FILE");
            File     : Text_IO.File_Type;
         begin
            if Directories.Exists (Filename) then
               Text_IO.Open (File, Text_IO.In_File, Filename);

               while not Text_IO.End_Of_File (File) loop
                  Append (Text_IO.Get_Line (File));
               end loop;

               Text_IO.Close (File);
            end if;
         end;
      end if;

      --  Then in GPR_PROJECT_PATH and ADA_PROJECT_PATH

      if Environment_Variables.Exists ("GPR_PROJECT_PATH") then
         Add_List (Environment_Variables.Value ("GPR_PROJECT_PATH"));
      end if;

      if Environment_Variables.Exists ("ADA_PROJECT_PATH") then
         Add_List (Environment_Variables.Value ("ADA_PROJECT_PATH"));
      end if;

      if Prefix /= "" then
         --  <prefix>/share/gpr

         Append
           (Directories.Compose
              (Directories.Compose (Prefix, "share"), "gpr"));

         --  <prefix>/lib/gnat

         Append
           (Directories.Compose
              (Directories.Compose (Prefix, "lib"), "gnat"));
      end if;
   end Append_Default_Search_Paths;

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Name_Type;
      Paths : Path_Name.Set.Object := Path_Name.Set.Empty_Set)
      return GPR2.Path_Name.Object
   is
      DS       : constant Character := OS_Lib.Directory_Separator;
      GPR_Name : constant Name_Type := Ensure_Extension (Name);

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
                             Path_Name.Dir_Name (P) & String (GPR_Name);
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

      return Path_Name.Create_File (GPR_Name, Path_Name.Resolve_On_Current);
   end Create;

   --------------------------
   -- Default_Search_Paths --
   --------------------------

   function Default_Search_Paths
     (Current_Directory : Boolean) return Path_Name.Set.Object
   is
      Result : Path_Name.Set.Object;
   begin
      if Current_Directory then
         Result.Append
           (Path_Name.Create_Directory
              (Name_Type (Directories.Current_Directory)));
      end if;

      Append_Default_Search_Paths (Result);

      return Result;
   end Default_Search_Paths;

   ----------------------
   -- Ensure_Extension --
   ----------------------

   function Ensure_Extension (Name : Name_Type) return Name_Type is
      use Ada.Characters.Handling;
   begin
      if To_Lower (Directories.Extension (String (Name))) in
           String (Project_File_Extension_No_Dot)
           | String (Config_File_Extension_No_Dot)
      then
         return Name;
      else
         --  The default is the .gpr extension, i.e. configuration project file
         --  name have to be provided with extension.

         return Name & Project_File_Extension;
      end if;
   end Ensure_Extension;

   ------------------------------
   -- Look_For_Default_Project --
   ------------------------------

   function Look_For_Default_Project
     (Implicit_Only : Boolean) return Path_Name.Object
   is
      use Directories;
      Default_Name : constant String := "default.gpr";
      Search       : Search_Type;
      Item         : Directory_Entry_Type;
      Implicit     : Path_Name.Object;

      function Executable_Prefix_Path return String;

      ----------------------------
      -- Executable_Prefix_Path --
      ----------------------------

      function Executable_Prefix_Path return String is
         use GNAT.OS_Lib;

         --  Exec_Name : constant String := Ada.Command_Line.Command_Name;
         Exec_Name : constant String := "gnat";

         function Get_Install_Dir (S : String) return String;
         --  S is the executable name preceded by the absolute or relative
         --  path, e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory
         --  where "bin" lies (in the example "C:\usr").
         --  If the executable is not in a "bin" directory, returns "".

         ---------------------
         -- Get_Install_Dir --
         ---------------------

         function Get_Install_Dir (S : String) return String is
            use GNAT.Case_Util;

            Exec      : String  :=
                          Normalize_Pathname (S, Resolve_Links => True);
            Path_Last : Integer := 0;

         begin
            for J in reverse Exec'Range loop
               if Exec (J) = Directory_Separator then
                  Path_Last := J - 1;
                  exit;
               end if;
            end loop;

            if Path_Last >= Exec'First + 2 then
               To_Lower (Exec (Path_Last - 2 .. Path_Last));
            end if;

            if Path_Last < Exec'First + 2
              or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
              or else (Path_Last - 3 >= Exec'First
                       and then Exec (Path_Last - 3) /= Directory_Separator)
            then
               return "";
            end if;

            return (Exec (Exec'First .. Path_Last - 4)) & Directory_Separator;
         end Get_Install_Dir;

      begin
         --  First determine if a path prefix was placed in front of the
         --  executable name.

         for J in reverse Exec_Name'Range loop
            if Exec_Name (J) = Directory_Separator then
               return Get_Install_Dir (Exec_Name);
            end if;
         end loop;

         --  If we get here, the user has typed the executable name with no
         --  directory prefix.

         declare
            Path : GNAT.OS_Lib.String_Access :=
                     Locate_Exec_On_Path (Exec_Name);
         begin
            if Path = null then
               return "";
            else
               declare
                  Dir : constant String := Get_Install_Dir (Path.all);
               begin
                  Free (Path);
                  return Dir;
               end;
            end if;
         end;
      end Executable_Prefix_Path;

   begin
      if not Implicit_Only then
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

         end if;
      end if;

      Implicit := Path_Name.Create_File
        (Name_Type (Executable_Prefix_Path & "/share/gpr/_default.gpr"));

      if Implicit.Exists then
         return Implicit;
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
