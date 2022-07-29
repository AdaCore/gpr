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

with Ada.Containers.Indefinite_Hashed_Maps;

with GPRname.Common;
with GPRname.Source;
with GPRname.Source.Set;
with GPRname.Source_Dir;
with GPRname.Source_Dir.Vector;
with GPRname.Pattern;
with GPRname.Pattern.Vector;
with GPRname.Pattern.Language;
with GPRname.Pattern.Language.Vector;

package GPRname.Section is

   use GPRname.Common;
   use GPRname.Source_Dir;
   use GPRname.Source_Dir.Vector;
   use GPRname.Pattern;
   use GPRname.Pattern.Language;

   type Object is tagged private;
   --  Object to store per-section information (dirs, patterns...)

   Empty_Section : constant Object;

   package Language_Patterns_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Language_Type,
      Pattern.Vector.Object,
      Str_Hash_Case_Insensitive,
      "=",
      Pattern.Vector."=");
   --  For excluded patterns: use a map from Languages to Pattern vectors
   --  because we're not interested in the insertion order, just lookup.

   procedure Add_Directory
     (Self      : in out Object;
      Directory : String;
      Root_Dir  : String);
   --  Adds a source directory to the section

   procedure Add_Directories_File
     (Self : in out Object;
      File : String);
   --  Adds a source directories file to the section

   procedure Add_Language_Pattern
     (Self     : in out Object;
      Language : Language_Type;
      Pattern  : Pattern_Type);
   --  Adds a naming pattern to the section

   procedure Add_Excluded_Language_Pattern
     (Self     : in out Object;
      Language : Language_Type;
      Pattern  : Pattern_Type);
   --  Adds an excluded naming pattern to the section

   function Is_Valid (Self : Object) return Boolean;
   --  Returns True if the section is valid (i.e. contains one pattern)

   procedure Prepare (Self : in out Object; Root : String);
   --  Processes a section to finalize it for use by gprname:
   --  Add some default values, and complete the source dirs by reading the
   --  directories files.

   procedure Reset (Self : in out Object);
   --  Resets a section (empty it)

   function Directories (Self : Object) return Source_Dir.Vector.Object;
   --  Returns the list of directories declared in section Self

   function Patterns (Self : Object) return Pattern.Language.Vector.Object;
   --  Returns the list of patterns and the associated languages, declared in
   --  section Self.

   function Files (Self : Object) return Source.Set.Object;
   --  Returns the list of files declared in section Self.

   function Excluded_Patterns
     (Self : Object) return Language_Patterns_Map.Map;
   --  Returns the map from languages to excluded patterns, as defined in
   --  section Self.

private

   type Object is tagged record
      Directories       : Source_Dir.Vector.Object;
      Files             : Source.Set.Object;
      Directories_Files : Path_Name_Vector.Vector;
      Patterns          : Pattern.Language.Vector.Object;
      Excluded_Patterns : Language_Patterns_Map.Map;
   end record;
   --  Description of a section on the command-line.
   --  For non-excluded patterns, we use a vector instead of a map since we
   --  need to preserve the command-line order.

   Empty_Section : constant Object :=
     (Directories       => Source_Dir.Vector.Empty_Vector,
      Files             => Source.Set.Empty_Set,
      Directories_Files => Path_Name_Vector.Empty_Vector,
      Patterns          =>
        Pattern.Language.Vector.Empty_Vector,
      Excluded_Patterns => Language_Patterns_Map.Empty_Map);

   function Directories (Self : Object) return Source_Dir.Vector.Object is
     (Self.Directories);

   function Files (Self : Object) return Source.Set.Object is
     (Self.Files);

   function Patterns (Self : Object) return Pattern.Language.Vector.Object is
     (Self.Patterns);

   function Excluded_Patterns
     (Self : Object) return Language_Patterns_Map.Map
   is (Self.Excluded_Patterns);

end GPRname.Section;
