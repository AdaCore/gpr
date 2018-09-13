------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

   procedure Add_Directories_File (Self : in out Object; Fil : String) is
   begin
      Self.Directories_Files.Append (Create_File (Name_Type (Fil)));
   end Add_Directories_File;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory (Self : in out Object; Dir : String) is
   begin
      Self.Directories.Append (Create (Name_Type (Dir)));
   end Add_Directory;

   -----------------------------------
   -- Add_Excluded_Language_Pattern --
   -----------------------------------

   procedure Add_Excluded_Language_Pattern
     (Self : in out Object;
      Lang : Language_Type;
      Patt : Pattern_Type)
   is
      Is_New_Key              : Boolean;
      Position                : Language_Patterns_Map.Cursor;
      Compiled_Pattern_Vector : Pattern.Vector.Object;
      Compiled                : constant Pattern.Object := Create (Patt);

   begin
      Compiled_Pattern_Vector.Append (Compiled);
      Self.Excluded_Patterns.Insert
        (Lang, Compiled_Pattern_Vector, Position, Is_New_Key);

      if not Is_New_Key then
         Self.Excluded_Patterns.Reference (Position).Append (Compiled);
      end if;
   end Add_Excluded_Language_Pattern;

   --------------------------
   -- Add_Language_Pattern --
   --------------------------

   procedure Add_Language_Pattern
     (Self : in out Object;
      Lang : Language_Type;
      Patt : Pattern_Type)
   is
   begin
      Self.Patterns.Append (Create (Patt, Lang));
   end Add_Language_Pattern;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Object) return Boolean is
     (not Self.Patterns.Is_Empty);

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Self : in out Object) is

      use Ada.Text_IO;

      function Get_Source_Dirs_From_File
        (File : String) return Source_Dir.Vector.Object;
      --  Add source directories from a file

      function Get_Source_Dirs_From_File
        (File : String) return Source_Dir.Vector.Object
      is
         F   : File_Type;
         Ret : Source_Dir.Vector.Object;
      begin
         Open (F, In_File, File);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               Ret.Append (Create (Name_Type (Line)));
            end;
         end loop;
         Close (F);
         return Ret;
      exception
         when others =>
            raise GPRname_Exception with "Could not read file '" & File & "'";
            return Source_Dir.Vector.Empty_Vector;
      end Get_Source_Dirs_From_File;

   begin
      --  First, add the directories listed in the source dir files

      for File of Self.Directories_Files loop
         Self.Directories.Append (Get_Source_Dirs_From_File (File.Value));
      end loop;

      --  Second, add the default source dir (".") if there is none

      if Self.Directories.Is_Empty then
         Self.Directories.Append (Create ("."));
      end if;

      --  Third, add the default pattern ("*") is there is none

      if Self.Patterns.Is_Empty then
         Self.Add_Language_Pattern (Language_Type'("Ada"),
                                    Pattern_Type'("*"));
      end if;
   end Prepare;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Object) is
   begin
      Self.Directories.Clear;
      Self.Directories_Files.Clear;
      Self.Patterns.Clear;
      Self.Excluded_Patterns.Clear;
   end Reset;

   -----------------
   -- Directories --
   -----------------

   function Directories (Self : Object) return Source_Dir.Vector.Object is
     (Self.Directories);

   --------------
   -- Patterns --
   --------------

   function Patterns (Self : Object) return Pattern.Language.Vector.Object is
     (Self.Patterns);

   -----------------------
   -- Excluded_Patterns --
   -----------------------

   function Excluded_Patterns
     (Self : Object) return Language_Patterns_Map.Map is
     (Self.Excluded_Patterns);

end GPRname.Section;
