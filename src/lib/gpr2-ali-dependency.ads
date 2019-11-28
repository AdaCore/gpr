------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Calendar;

with GNAT.Calendar;

with GPR2.Source;

package GPR2.ALI.Dependency is

   --
   --  Dependencies (D lines)
   --

   type Object is tagged private;

   Undefined : constant Object;

   function Create
     (Sfile     : Name_Type;
      Stamp     : Ada.Calendar.Time;
      Checksum  : Word;
      Unit_Name : Optional_Name_Type;
      Unit_Kind : Kind_Type) return Object;
   --  Creates and returns a Dependency_Data object.
   --  If Unit_Name is empty, than the Sfile is configuration pragmas file.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns True if Self is defined

   function Is_Configuration (Self : Object) return Boolean;
   --  Returns True if dependency is to configuration pragmas file

   function Checksum (Self : Object) return Word;
   --  Returns the Checksum for Self

   function Sfile (Self : Object) return Name_Type;
   --  Returns the Sfile for Self

   function Stamp (Self : Object) return Ada.Calendar.Time;
   --  Returns the Time Stamp for Self

   function Unit_Name (Self : Object) return Name_Type;
   --  Returns the Unit_Name for Self

private

   type Object is tagged record

      Sfile : Unbounded_String;
      --  Base name of the source file.
      --  Or full path name of the configuration pragmas files.

      Stamp : Ada.Calendar.Time;
      --  Time stamp value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files.

      Checksum : Word;
      --  Checksum value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files
      --  Zero if Sfile is configuration pragmas file.

      Unit_Name : Unbounded_String;
      --  Name of the unit or subunit.
      --  Empty if Sfile is configuration pragmas file.

      Unit_Kind : Kind_Type;
      --  Unit kind (S_Separate for a subunit)

   end record;

   Undefined : constant Object :=
                 Object'(Sfile     => <>,
                         Stamp     => GNAT.Calendar.No_Time,
                         Checksum  => 0,
                         Unit_Name => <>,
                         Unit_Kind => S_Spec);

   function Create
     (Sfile     : Name_Type;
      Stamp     : Ada.Calendar.Time;
      Checksum  : Word;
      Unit_Name : Optional_Name_Type;
      Unit_Kind : Kind_Type) return Object
   is
     (Object'(Sfile     => +String (Sfile),
              Stamp     => Stamp,
              Checksum  => Checksum,
              Unit_Name => +String (Unit_Name),
              Unit_Kind => Unit_Kind));

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Checksum (Self : Object) return Word is
     (Self.Checksum);

   function Sfile (Self : Object) return Name_Type is
     (Name_Type (-Self.Sfile));

   function Stamp (Self : Object) return Ada.Calendar.Time is
     (Self.Stamp);

   function Unit_Name (Self : Object) return Name_Type is
     (Name_Type (-Self.Unit_Name));

   function Is_Configuration (Self : Object) return Boolean is
      (Self.Unit_Name = Null_Unbounded_String);

end GPR2.ALI.Dependency;
