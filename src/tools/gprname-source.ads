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

with Ada.Strings.Unbounded;

with GPR2.Path_Name;

with GPRname.Common;
with GPRname.Unit;
with GPRname.Unit.Vector;

package GPRname.Source is

   use Ada.Strings.Unbounded;

   use GPR2;

   use GPRname.Common;
   use GPRname.Unit;
   use GPRname.Unit.Vector;

   type Object (Unit_Based : Boolean) is tagged private;
   --  A description of a source: its file name and language.
   --  For unit-based languages (namely, Ada) we store a description of the
   --  unit(s) defined in this source.

   function Create
     (File : Path_Name.Object; Language : Language_Type) return Object;
   --  Creates a non-unit based source

   function Create_Unit_Based
     (File : Path_Name.Object; Language : Language_Type) return Object;
   --  Creates a unit-based source

   procedure Append_Unit (Self : in out Object; U : Unit.Object)
     with Pre => Self.Unit_Based;
   --  Associates a new unit to a source

   function "<" (Left, Right : Object) return Boolean;
   --  Compares two source objects using Path_Name comparison on the File field

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Compares two source objects using Path_Name comparison on the File field

   function File (Self : Object) return Path_Name.Object;
   --  Returns the Path_Name object for the source Self

   function Language (Self : Object) return Language_Type;
   --  Returns the language for Self

   function Units (Self : Object) return Unit.Vector.Object
     with Pre => Self.Unit_Based;
   --  Returns the unit(s) declared in Self (requires Self to be unit-based)

private

   type Object (Unit_Based : Boolean) is tagged record
      File     : Path_Name.Object := GPR2.Path_Name.Undefined;
      Language : Unbounded_String;
      case Unit_Based is
         when True =>
            Units : Unit.Vector.Object := Unit.Vector.Empty_Vector;
         when others =>
            null;
      end case;
   end record with Dynamic_Predicate =>
     (if Language_Type (To_String (Language)) = Ada_Lang then
        Unit_Based = True);

end GPRname.Source;
