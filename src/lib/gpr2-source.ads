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

with GPR2.Compilation_Unit;
with GPR2.Compilation_Unit.List;
with GPR2.Path_Name;
with GPR2.Source_Reference.Identifier.Set;

package GPR2.Source is

   use Ada;

   use type GPR2.Path_Name.Object;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function "<" (Left, Right : Object) return Boolean;

   overriding function "=" (Left, Right : Object) return Boolean;
   --  A source object is equal if it is the same unit for unit based language,
   --  and if it is the same filename otherwise.

   function Path_Name (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Returns the filename for the given source

   function Language (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the language for the given source

   function Time_Stamp (Self : Object) return Calendar.Time
     with Pre => Self.Is_Defined;
   --  Returns the time-stamp for this source

   function Create
     (Filename : GPR2.Path_Name.Object;
      Language : Name_Type;
      Kind     : Kind_Type) return Object
     with Pre  => Filename.Is_Defined and then Language /= "Ada",
          Post => Create'Result.Is_Defined;
   --  Constructor for a non-Ada source object

   function Create_Ada
     (Filename          : GPR2.Path_Name.Object;
      Compilation_Units : Compilation_Unit.List.Object;
      Is_RTS_Source     : Boolean) return Object
     with Pre  => Filename.Is_Defined and then not Compilation_Units.Is_Empty,
          Post => Create_Ada'Result.Is_Defined;
   --  Constructor for an Ada source object.
   --  Information in Compilation_Units parameter came from filenames and
   --  project information only. It can be different from function
   --  Compilation_Units call results because Separate Ada units can be
   --  determined only on parsing source files.

   procedure Release (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Releases source object if not referenced anymore

   function Has_Units (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if source is unit-based (i.e. Ada)

   --
   --  The following routines only make sense if Has_Units is True
   --

   function Has_Single_Unit (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self has only one compilation unit

   function Compilation_Units
     (Self : Object) return Compilation_Unit.List.Object
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns the compilation units for Self

   type Unit_Index is new Positive;

   function Has_Compilation_Unit_At
     (Self : Object; Index : Natural) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self has a compilation unit at Index

   function Unit_Name (Self : Object; Index : Natural := 1) return Name_Type
     with Pre => Self.Is_Defined and then Self.Has_Units
                 and then Self.Has_Compilation_Unit_At (Index);
   --  Returns the unit name for the source Self at Index (default = 1)

   function Is_Generic
     (Self : Object; Index : Natural := 1) return Boolean;
   --  Returns True if Self is a generic unit

   function With_Clauses
     (Self  : Object;
      Index : Natural := 1) return Source_Reference.Identifier.Set.Object
     with Pre => Self.Is_Defined and then Self.Has_Units
                 and then Self.Has_Compilation_Unit_At (Index);
   --  Returns the list of withed units for the source Self at
   --  Index (default = 1).

   function With_Clauses
     (Self : Object;
      Unit : Name_Type) return Source_Reference.Identifier.Set.Object
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns the dependencies in Self associated with all the compilation
   --  units for Unit. The result may be empty.

   --
   --  The following routines may be used for both unit-based and
   --  non-unit-based sources. In the latter case, Index is not used.
   --

   function Kind (Self : Object; Index : Natural := 1) return Kind_Type
     with Pre =>
       Self.Is_Defined
       and then (not Self.Has_Units
                 or else Self.Has_Compilation_Unit_At (Index));
   --  Returns the kind for the source Self

   function Has_Other_Part (Self : Object) return Boolean;
   --  Returns True if Self has an other part

   function Other_Part (Self : Object) return Object
     with Pre => Self.Is_Defined and then Self.Has_Other_Part;
   --  Returns the other part for the source Self/
   --  The "other part" is the body for a spec, or the spec for a body, or
   --  the unit containing the stub for a (separate) subunit.

   procedure Set_Other_Part (Self : Object; Other_Part : Object)
     with Pre => Self.Is_Defined and then Other_Part.Is_Defined;
   --  Sets the other part for the source Self, and the other way around
   --  (see comment above for function Other_Part).

private

   type Object is tagged record
      Pathname : GPR2.Path_Name.Object;
   end record;

   Undefined : constant Object := (Pathname => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Source;
