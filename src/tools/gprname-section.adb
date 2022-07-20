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

with Ada.Text_IO;

with GPR2.Path_Name;

package body GPRname.Section is

   use GPR2;
   use GPR2.Path_Name;

   --------------------------
   -- Add_Directories_File --
   --------------------------

   procedure Add_Directories_File
     (Self : in out Object;
      File : String) is
   begin
      Self.Directories_Files.Append (Create_File (Filename_Type (File)));
   end Add_Directories_File;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Self      : in out Object;
      Directory : String;
      Root_Dir  : String)
   is
      Dir_And_OFile : Source_Dir.Dir_And_Optional_File;
   begin
      Dir_And_OFile :=
        Create (Filename_Type (Directory),
                Filename_Optional (Root_Dir));
      Self.Directories.Append (Dir_And_OFile.Dir);
   end Add_Directory;

   -----------------------------------
   -- Add_Excluded_Language_Pattern --
   -----------------------------------

   procedure Add_Excluded_Language_Pattern
     (Self     : in out Object;
      Language : Language_Type;
      Pattern  : Pattern_Type)
   is
      Compiled                : constant GPRname.Pattern.Object :=
                                  GPRname.Pattern.Object (Create (Pattern));
      Is_New_Key              : Boolean;
      Position                : Language_Patterns_Map.Cursor;
      Compiled_Pattern_Vector : GPRname.Pattern.Vector.Object;

   begin
      Compiled_Pattern_Vector.Append (Compiled);
      Self.Excluded_Patterns.Insert
        (Language, Compiled_Pattern_Vector, Position, Is_New_Key);

      if not Is_New_Key then
         Self.Excluded_Patterns.Reference (Position).Append (Compiled);
      end if;
   end Add_Excluded_Language_Pattern;

   --------------------------
   -- Add_Language_Pattern --
   --------------------------

   procedure Add_Language_Pattern
     (Self     : in out Object;
      Language : Language_Type;
      Pattern  : Pattern_Type) is
   begin
      Self.Patterns.Append
        (GPRname.Pattern.Language.Object (Create (Pattern, Language)));
   end Add_Language_Pattern;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Object) return Boolean is
     (not Self.Patterns.Is_Empty);

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Self : in out Object;  Root : String) is

      use Ada.Text_IO;

      procedure Get_Source_Dirs_And_Files_From_File (File : String);
      --  Add source directories and source files from a file

      -----------------------------------------
      -- Get_Source_Dirs_And_Files_From_File --
      -----------------------------------------

      procedure Get_Source_Dirs_And_Files_From_File (File : String)
      is
         F            : File_Type;
         Source_Dirs  : Source_Dir.Vector.Object;
         Source_Files : Source.Set.Object;
      begin
         Open (F, In_File, File);

         while not End_Of_File (F) loop
            declare
               Dir_And_OFile : constant Source_Dir.Dir_And_Optional_File
                 := Create (Filename_Type (Get_Line (F)),
                            GPR2.Filename_Optional (Root));
            begin
               if not Source_Dirs.Contains (Dir_And_OFile.Dir)
               then
                  Source_Dirs.Append (Dir_And_OFile.Dir);
               end if;

               if Dir_And_OFile.Has_File then
                  Source_Files.Insert
                    (New_Item => Source.Create
                       (File => Dir_And_OFile.File,
                        Language => "",
                        Unit_Based => False));
               end if;
            end;
         end loop;

         Close (F);

         Self.Directories.Append_Vector (Source_Dirs);
         Self.Files := Source_Files;

      exception
         when GPRname_Exception =>
            raise;
         when others =>
            raise GPRname_Exception with "Could not read file '" & File & "'";
      end Get_Source_Dirs_And_Files_From_File;

   begin
      --  First, add the directories listed in the source dir files

      for File of Self.Directories_Files loop
         Get_Source_Dirs_And_Files_From_File (File.Value);
      end loop;

      --  Second, add the default source dir (".") if there is none

      if Self.Directories.Is_Empty then
         Self.Directories.Append
           (Create (".", GPR2.Filename_Optional (Root)).Dir);
      end if;

      --  Third, add the default pattern ("*") is there is none

      if Self.Patterns.Is_Empty then
         Self.Add_Language_Pattern
           (Language_Type'("Ada"), Pattern_Type'("*"));
      end if;
   end Prepare;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Object) is
   begin
      Self.Directories.Clear;
      Self.Files.Clear;
      Self.Directories_Files.Clear;
      Self.Patterns.Clear;
      Self.Excluded_Patterns.Clear;
   end Reset;

end GPRname.Section;
