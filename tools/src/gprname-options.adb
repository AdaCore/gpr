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

package body GPRname.Options is

   ----------------------------
   -- Ignore_Duplicate_Files --
   ----------------------------

   function Ignore_Duplicate_Files (Self : Object) return Boolean is
     (Self.Ignore_Duplicate_Files);

   -----------------------------
   -- Ignore_Predefined_Units --
   -----------------------------

   function Ignore_Predefined_Units (Self : Object) return Boolean is
     (Self.Ignore_Predefined_Units);

   ------------------
   -- Minimal_Dirs --
   ------------------

   function Minimal_Dirs (Self : Object) return Boolean is
      (Self.Minimal_Dirs);

   ---------------
   -- No_Backup --
   ---------------

   function No_Backup (Self : Object) return Boolean is
     (Self.No_Backup);

   -------------------
   -- Prep_Switches --
   -------------------

   function Prep_Switches (Self : Object) return String_Vector.Vector is
     (Self.Prep_Switches);

   ------------------
   -- Project_File --
   ------------------

   function Project_File (Self : Object) return String is
     (if Self.Project_File /= Null_Unbounded_String
      then To_String (Self.Project_File)
      else No_String);

   ---------
   -- RTS --
   ---------

   function RTS (Self : Object) return String is
     (if Self.Project_File /= Null_Unbounded_String
      then To_String (Self.RTS)
      else No_String);

   --------------
   -- Sections --
   --------------

   function Sections (Self : Object) return Section.Vector.Object is
     (Self.Sections);

   ------------
   -- Target --
   ------------

   function Target (Self : Object) return String is
     (if Self.Target /= Null_Unbounded_String
      then To_String (Self.Target)
      else No_String);

   ---------------
   -- Verbosity --
   ---------------

   function Verbosity (Self : Object) return Verbosity_Level_Type is
     (Self.Verbosity);

   -----------------------------
   -- Build_From_Command_Line --
   -----------------------------

   procedure Build_From_Command_Line (Self : in out Object) is separate;

end GPRname.Options;
