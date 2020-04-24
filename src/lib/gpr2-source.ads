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

--  This package defines a source Object. This source object is shared with all
--  loaded project tree.

with Ada.Calendar;

with GPR2.Unit;
with GPR2.Unit.List;
with GPR2.Path_Name;
with GPR2.Source_Info;

package GPR2.Source is

   use type GPR2.Unit.Library_Unit_Type;

   type Object is new Source_Info.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function "<" (Left, Right : Object) return Boolean;

   overriding function "=" (Left, Right : Object) return Boolean;
   --  A source object is equal if it is the same unit for unit based language,
   --  and if it is the same filename otherwise.

   function Path_Name (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Returns the path-name for the given source

   function Language (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the language for this source

   function Timestamp (Self : Object) return Ada.Calendar.Time
     with Pre => Self.Is_Defined;
   --  Returns the creation time for this source

   function Check_Timestamp (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Check is the timestamp kept in the object corresponds to source filename
   --  timestamp.

   function Create
     (Filename : GPR2.Path_Name.Object;
      Language : Name_Type;
      Kind     : Unit.Library_Unit_Type) return Object
     with Pre  => Filename.Is_Defined and then Language /= "Ada",
          Post => Create'Result.Is_Defined;
   --  Constructor for a non-Ada source object

   function Create_Ada
     (Filename      : GPR2.Path_Name.Object;
      Units         : Unit.List.Object;
      Is_RTS_Source : Boolean) return Object
     with Pre  => Filename.Is_Defined and then not Units.Is_Empty,
          Post => Create_Ada'Result.Is_Defined;
   --  Constructor for an Ada source object.
   --  Information in Units parameter came from filenames and
   --  project information only. It can be different from function
   --  Units call results because Separate Ada units can be
   --  determined only on parsing source files.

private

   type Object is new Source_Info.Object with record
      Path_Name : GPR2.Path_Name.Object;
      Timestamp : Calendar.Time := No_Time;
      Language  : Unbounded_String;
      Ada_Key   : Unbounded_String;
   end record;

   Undefined : constant Object :=
                 Object'(Source_Info.Undefined
                         with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Timestamp (Self : Object) return Ada.Calendar.Time is
     (Self.Timestamp);

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path_Name);

   function Language (Self : Object) return Name_Type is
     (Name_Type (-Self.Language));

end GPR2.Source;
