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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO;

with GNAT.MD5; use GNAT.MD5;
with GNAT.OS_Lib;

with GPR2.Path_Name;
with GPRtools;

package body GPRinstall.Uninstall is

   use Ada;
   use Ada.Directories;
   use GPR2;

   package File_Set is new Containers.Indefinite_Ordered_Sets (String);

   -------------
   -- Process --
   -------------

   procedure Process
     (Install_Name : String;
      Options      : in out GPRinstall.Options.Object)
   is
      use GNAT;
      use GPRtools;

      procedure Delete_File (Position : File_Set.Cursor);
      --  Delete file pointed to by Position, do nothing if the file is not
      --  found.

      procedure Do_Delete (Filename : String);
      --  Delete file or display a message if in dry-run mode

      procedure Delete_Empty_Directory (Dir_Name : String);
      --  Delete Dir_Name if empty, if removed try with parent directory

      ----------------------------
      -- Delete_Empty_Directory --
      ----------------------------

      procedure Delete_Empty_Directory (Dir_Name : String) is
      begin
         Delete_Empty_Directory (-Options.Global_Prefix_Dir.V, Dir_Name);
      end Delete_Empty_Directory;

      -----------------
      -- Delete_File --
      -----------------

      procedure Delete_File (Position : File_Set.Cursor) is
         Pathname : constant String := File_Set.Element (Position);
      begin
         Do_Delete (Pathname);
      end Delete_File;

      ---------------
      -- Do_Delete --
      ---------------

      procedure Do_Delete (Filename : String) is
         Success : Boolean;
      begin
         if Options.Dry_Run then
            Text_IO.Put_Line ("delete " & Filename);

         else
            OS_Lib.Delete_File (Filename, Success);
            Delete_Empty_Directory (Containing_Directory (Filename));
         end if;
      end Do_Delete;

      Dir  : constant String :=
               (if OS_Lib.Is_Absolute_Path (Install_Name)
                then Containing_Directory (Install_Name)
                else Options.Project_Dir & "manifests") & DS;

      Name : constant String :=
               (if OS_Lib.Is_Absolute_Path (Install_Name)
                then Install_Name
                else Dir & Install_Name);

      Man     : Text_IO.File_Type;
      Buffer  : String (1 .. 4096);
      Last    : Natural;
      Files   : File_Set.Set;
      Changed : File_Set.Set;

      --  Ranges in Buffer above, we have the MD5 (32 chars) a space and then
      --  the filename.

      subtype MD5_Range is Positive range Message_Digest'Range;
      subtype Name_Range is Positive range MD5_Range'Last + 2 .. Buffer'Last;

      File_Digest     : Message_Digest := (others => ASCII.NUL);
      Expected_Digest : Message_Digest;
      Removed         : Boolean;
      Prefix          : Path_Name.Object;

   begin
      --  Check if manifest for this project exists

      if not Exists (Name) then
         raise GPRinstall_Error with "Manifest " & Name & " not found.";
      end if;

      if Options.Verbosity > Quiet then
         Text_IO.Put_Line ("Uninstall project " & Install_Name);
      end if;

      --  Check each file to be deleted

      Text_IO.Open (Man, Text_IO.In_File, Name);

      while not Text_IO.End_Of_File (Man) loop
         Text_IO.Get_Line (Man, Buffer, Last);

         --  Skip first line if it is the original project's signature

         if Last > MD5_Range'Last
           and then Buffer (1 .. 2) /= Sig_Line
         then
            declare
               F_Name   : constant String := Buffer (Name_Range'First .. Last);
               Pathname : constant String := Dir & F_Name;
               Path     : constant Path_Name.Object :=
                            Path_Name.Create_File (Filename_Type (Pathname));

            begin
               Expected_Digest := Buffer (MD5_Range);

               if Exists (Pathname) then
                  File_Digest := Path.Content_MD5;
                  Removed := False;
               else
                  Removed := True;
               end if;

               if Options.Global_Prefix_Dir.Default then
                  if not Prefix.Is_Defined then
                     Prefix := Path_Name.Create_Directory
                       (Filename_Type (Path.Dir_Name));
                  else
                     Prefix := Prefix.Common_Prefix (Path);
                  end if;
               end if;

               --  Unconditionally add a file to the remove list if digest is
               --  ok, if we are running in force mode or the file has already
               --  been removed.

               if File_Digest = Expected_Digest
                 or else Options.Force_Installations
                 or else Removed
               then
                  Files.Include (Pathname);

               else
                  Changed.Include (Pathname);
               end if;
            end;
         end if;
      end loop;

      Text_IO.Close (Man);

      if Prefix.Is_Defined then
         Options.Global_Prefix_Dir := (-Prefix.Value, False);
      end if;

      --  Delete files

      if Changed.Is_Subset (Of_Set => Files) then
         Files.Iterate (Delete_File'Access);

         --  Then finally delete the manifest for this project

         Do_Delete (Name);

      else
         if Options.Verbosity > Quiet then
            Text_IO.Put_Line ("Following files have been changed:");

            declare
               procedure Display (Position : File_Set.Cursor);
               --  Display only if not part of Files set

               -------------
               -- Display --
               -------------

               procedure Display (Position : File_Set.Cursor) is
                  F_Name : constant String := File_Set.Element (Position);
               begin
                  if not Files.Contains (F_Name) then
                     Text_IO.Put_Line (F_Name);
                  end if;
               end Display;

            begin
               Changed.Iterate (Display'Access);
            end;

            raise GPRinstall_Error
              with "use option -f to force file deletion.";
         end if;
      end if;
   end Process;

end GPRinstall.Uninstall;
