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
     (File       : Path_Name.Object;
      Language   : Language_Type;
      Unit_Based : Boolean := False) return Object;
   --  Creates a non-unit based source

   procedure Append_Unit
     (Self : in out Object;
      Unit : GPRname.Unit.Object)
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

   function Has_Units (Self : Object) return Boolean;
   --  Returns True if Self is unit based and has units

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
     (if Language_Type (To_String (Language)) = Ada_Lang
      then Unit_Based = True);

   function "<" (Left, Right : Object) return Boolean is
     (Path_Name."<" (Left.File, Right.File));

   overriding function "=" (Left, Right : Object) return Boolean is
     (Path_Name."=" (Left.File, Right.File));

   function File (Self : Object) return Path_Name.Object is
     (Self.File);

   function Language (Self : Object) return Language_Type is
     (Language_Type (To_String (Self.Language)));

   function Units (Self : Object) return Unit.Vector.Object is
     (Self.Units);

   function Has_Units (Self : Object) return Boolean is
     (Self.Unit_Based and then not Self.Units.Is_Empty);

end GPRname.Source;
