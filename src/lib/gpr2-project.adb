--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.String_Split;

package body GPR2.Project is

   use GNAT;

   ---------------------------------
   -- Append_Default_Search_Paths --
   ---------------------------------

   procedure Append_Default_Search_Paths
     (Paths       : in out Path_Name.Set.Object;
      Environment : GPR2.Environment.Object :=
                       GPR2.Environment.Process_Environment)
   is

      procedure Append (Value : String)
        with Post => (if Value'Length = 0
                      then Paths'Old.Length = Paths.Length
                          else Paths'Old.Length <= Paths.Length);
      --  Append Value into Paths is it is not empty and does not exists in
      --  Paths.

      procedure Add_List (Values : String)
        with Post => Paths'Old.Length <= Paths.Length;
      --  Add list Values (which has OS-dependent path separator) into
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
                        Path_Name.Create_Directory (Filename_Type (Value));
            begin
               if not Paths.Contains (Path) then
                  Paths.Append (Path);
               end if;
            end;
         end if;
      end Append;

   begin
      --  Then in GPR_PROJECT_PATH_FILE, one path per line

      if Environment.Exists ("GPR_PROJECT_PATH_FILE") then
         declare
            Filename : constant String :=
                         Environment.Value
                           ("GPR_PROJECT_PATH_FILE");
            File     : Text_IO.File_Type;
         begin
            if Filename'Length > 0 and then Directories.Exists (Filename) then
               Text_IO.Open (File, Text_IO.In_File, Filename);

               while not Text_IO.End_Of_File (File) loop
                  Append (Text_IO.Get_Line (File));
               end loop;

               Text_IO.Close (File);
            end if;
         end;
      end if;

      --  Then in GPR_PROJECT_PATH and ADA_PROJECT_PATH

      if Environment.Exists ("GPR_PROJECT_PATH") then
         Add_List (Environment.Value ("GPR_PROJECT_PATH"));
      end if;

      if Environment.Exists ("ADA_PROJECT_PATH") then
         Add_List (Environment.Value ("ADA_PROJECT_PATH"));
      end if;
   end Append_Default_Search_Paths;

   ------------
   -- Create --
   ------------

   function Create
     (Name          : Filename_Type;
      Resolve_Links : Boolean              := False;
      Paths         : Path_Name.Set.Object := Path_Name.Set.Empty_Set)
      return GPR2.Path_Name.Object
   is
      DS       : constant Character := OS_Lib.Directory_Separator;
      GPR_Name : constant Filename_Type := Ensure_Extension (Name);

   begin
      --  If the file exists or an absolute path has been specified or there
      --  is no ADA_PROJECT_PATH, just create the Path_Name_Type using the
      --  given Name.

      if OS_Lib.Is_Absolute_Path (String (GPR_Name)) then
         return Path_Name.Create_File
           (GPR_Name,
           Resolve_Links => Resolve_Links);

      else
         --  If we have an empty Paths set, this is the root project and it is
         --  expected to look into the current working directory in this case.

         if Paths.Is_Empty then
            if Directories.Exists
                 (Directories.Current_Directory & DS & String (GPR_Name))
            then
               return Path_Name.Create_File
                 (GPR_Name,
                  Resolve_Links => Resolve_Links);
            end if;

         else
            for P of Paths loop
               declare
                  F_Name : constant Filename_Type :=
                             Path_Name.Dir_Name (P) & GPR_Name;
               begin
                  if Directories.Exists (String (F_Name)) then
                     return Path_Name.Create_File
                       (GPR_Name,
                        Directory => Path_Name.Dir_Name (P),
                        Resolve_Links => Resolve_Links);
                  end if;
               exception
                  when Ada.IO_Exceptions.Name_Error =>
                     null;
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
     (Current_Directory : Boolean;
      Environment       : GPR2.Environment.Object :=
                            GPR2.Environment.Process_Environment)
      return Path_Name.Set.Object
   is
      Result : Path_Name.Set.Object;
   begin
      if Current_Directory then
         Result.Append
           (Path_Name.Create_Directory
              (Filename_Type (Directories.Current_Directory)));
      end if;

      Append_Default_Search_Paths (Result, Environment);

      return Result;
   end Default_Search_Paths;

   ----------------------
   -- Ensure_Extension --
   ----------------------

   function Ensure_Extension
     (Name        : Filename_Type;
      Config_File : Boolean := False) return Filename_Type
   is
      use Ada.Characters.Handling;
   begin
      if To_Lower (Directories.Extension (String (Name))) in
           String (Project_File_Extension_No_Dot)
           | String (Config_File_Extension_No_Dot)
      then
         return Name;
      elsif Config_File then
         return Name & Config_File_Extension;
      else
         --  The default is the .gpr extension

         return Name & Project_File_Extension;
      end if;
   exception
      when others =>
         if Config_File then
            return Name & Config_File_Extension;
         else
            --  The default is the .gpr extension
            return Name & Project_File_Extension;
         end if;
   end Ensure_Extension;

   ------------------
   -- Search_Paths --
   ------------------

   function Search_Paths
     (Root_Project      : Path_Name.Object;
      Tree_Search_Paths : Path_Name.Set.Object) return Path_Name.Set.Object is
   begin
      return Result : Path_Name.Set.Object := Tree_Search_Paths do
         Result.Prepend
           (Path_Name.Create_Directory
              (Filename_Type (Root_Project.Dir_Name)));
      end return;
   end Search_Paths;

end GPR2.Project;
