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

with Ada.Strings.Unbounded;

with GPRname.Common;
with GPRname.Section;
with GPRname.Section.Vector;

package GPRname.Options is

   use Ada.Strings.Unbounded;

   use GPRname.Common;
   use GPRname.Section;

   type Object is tagged private;
   --  Options for gprname

   procedure Build_From_Command_Line (Self : in out Object);
   --  Fill out a gprname options object from the command line

   function Ignore_Duplicate_Files (Self : Object) return Boolean;
   --  Returns the Ignore_Duplicate_Files flag for Self

   function Ignore_Predefined_Units (Self : Object) return Boolean;
   --  Returns the Ignore_Predefined_Units flag for Self

   function No_Backup (Self : Object) return Boolean;
   --  Returns the No_Backup flag for Self

   function Prep_Switches (Self : Object) return String_Vector.Vector;
   --  Returns the preprocessor switches for Self

   function Project_File (Self : Object) return String;
   --  Returns the project file for Self

   function RTS (Self : Object) return String;
   --  Returns the runtime for Self

   function Target (Self : Object) return String;
   --  Returns the target for Self

   function Verbosity (Self : Object) return Verbosity_Level_Type;
   --  Return the verbosity level for Self

   function Sections (Self : Object) return Section.Vector.Object;
   --  Returns the sections for Self

   function Minimal_Dirs (Self : Object) return Boolean;
   --  Returns the Minimal_Dirs flag for Self

private

   type Object is tagged record
      Project_File            : Unbounded_String := Null_Unbounded_String;
      Target                  : Unbounded_String := Null_Unbounded_String;
      RTS                     : Unbounded_String := Null_Unbounded_String;
      Verbosity               : Verbosity_Level_Type := None;
      Ignore_Duplicate_Files  : Boolean := False;
      Ignore_Predefined_Units : Boolean := False;
      Follow_Symlinks         : Boolean := False;
      No_Backup               : Boolean := False;
      Minimal_Dirs            : Boolean := False;
      Verbose_Parsing         : Integer := 0;
      Sections                : Section.Vector.Object;
      Prep_Switches           : String_Vector.Vector;
   end record;

end GPRname.Options;
