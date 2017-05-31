------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines a source Object. This source object is shared with all
--  loaded project tree.

package GPR2.Source is

   type Object is tagged private;

   Undefined : constant Object;

   type Kind_Type is (S_Spec, S_Body, S_Separate);

   function "<" (Left, Right : Object) return Boolean;

   overriding function "=" (Left, Right : Object) return Boolean;
   --  A source object is equal if it is the same unit for unit based language,
   --  and if it is the same filename otherwise.

   function Filename (Self : Object) return Full_Path_Name;
   --  Retruns the filename for the given source

   function Kind (Self : Object) return Kind_Type;
   --  Returns the kind of source

   function Other_Part (Self : Object) return Object;
   --  Returns the other-part of the source. This is either the spec for a body
   --  or the body for a spec.

   function Unit_Name (Self : Object) return Optional_Name_Type;
   --  Returns the unit name for the given source or the empty string if the
   --  language does not have support for unit.

   function Language (Self : Object) return Name_Type;
   --  Returns the language for the given source

   function Create
     (Filename  : Path_Name_Type;
      Kind      : Kind_Type;
      Language  : Name_Type;
      Unit_Name : Optional_Name_Type) return Object;
   --  Constructor for a source object

   procedure Set_Other_Part
     (Self       : in out Object;
      Other_Part : in out Object);
   --  Set the other-part for Self. The other-part is the body for a spec or
   --  the spec for a body or separate unit.

private

   type Object is tagged record
      Id : Natural := 0;
   end record;

   Undefined : constant Object := Object'(Id => 0);

end GPR2.Source;
