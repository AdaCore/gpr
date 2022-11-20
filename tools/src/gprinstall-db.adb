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
with Ada.Integer_Text_IO;
with Ada.Text_IO;

with GNAT.MD5;

package body GPRinstall.DB is

   ----------
   -- List --
   ----------

   procedure List (Options : GPRinstall.Options.Object) is

      use Ada;
      use Ada.Directories;
      use Ada.Integer_Text_IO;
      use Ada.Text_IO;

      use GNAT;
      use GNAT.MD5;

      type Stats is record
         N_Files           : Natural := 0;
         N_Files_Not_Found : Natural := 0;
         Bytes             : Directories.File_Size := 0;
      end record;

      function Get_Stat (Manifest : String) return Stats;
      --  Compute the stats for the given manifest file

      procedure Process (D_Entry : Directory_Entry_Type);
      --  Process a directory entry, this is a specific manifest file

      --------------
      -- Get_Stat --
      --------------

      function Get_Stat (Manifest : String) return Stats is

         Dir    : constant String := Containing_Directory (Manifest) & DS;
         File   : File_Type;
         Line   : String (1 .. 2048);
         Last   : Natural;
         Result : Stats;

         subtype MD5_Range is Positive range Message_Digest'Range;
         subtype Name_Range
           is Positive range MD5_Range'Last + 2 .. Line'Last;

      begin
         Open (File, In_File, Manifest);

         while not End_Of_File (File) loop
            Get_Line (File, Line, Last);

            if Line (1 .. 2) /= Sig_Line then
               declare
                  Filename : constant String :=
                               Dir & Line (Name_Range'First .. Last);
               begin
                  if Exists (Filename) then
                     Result.N_Files := Result.N_Files + 1;
                     Result.Bytes := Result.Bytes + Size (Filename);
                  else
                     Result.N_Files_Not_Found := Result.N_Files_Not_Found + 1;
                  end if;
               end;
            end if;
         end loop;

         Close (File);

         return Result;
      end Get_Stat;

      -------------
      -- Process --
      -------------

      procedure Process (D_Entry : Directory_Entry_Type) is

         package File_Size_IO is
           new Text_IO.Integer_IO (Directories.File_Size);
         use File_Size_IO;

         S    : Stats;
         Unit : String (1 .. 2) := "b ";
         Size : Directories.File_Size;
      begin
         Put ("   " & Directories.Simple_Name (D_Entry));
         Set_Col (25);

         if Options.Output_Stats then
            --  Get stats

            S := Get_Stat (Full_Name (D_Entry));

            --  Number of files

            Put (S.N_Files, Width => 5);
            if S.N_Files > 1 then
               Put (" files, ");
            else
               Put (" file, ");
            end if;

            --  Sizes

            Size := S.Bytes;

            if Size > 1024 then
               Size := Size / 1024;
               Unit := "Kb";
            end if;

            if Size > 1024 then
               Size := Size / 1024;
               Unit := "Mb";
            end if;

            if Size > 1024 then
               Size := Size / 1024;
               Unit := "Gb";
            end if;

            Put (Size, Width => 5);
            Put (' ' & Unit);

            --  Files not found if any

            if S.N_Files_Not_Found > 0 then
               Put (" (");
               Put (S.N_Files_Not_Found, Width => 0);
               Put (" files missing)");
            end if;
         end if;

         New_Line;
      end Process;

      Dir : constant String := Options.Project_Dir & "manifests";

   begin
      New_Line;

      if Exists (Dir) then
         Put_Line ("List of installed packages");
         New_Line;

         Search
           (Dir, "*",
            (Ordinary_File => True, others => False),
            Process'Access);
      else
         Put_Line ("No package installed");
         New_Line;
      end if;
   end List;

end GPRinstall.DB;
