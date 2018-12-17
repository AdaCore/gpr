------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2018, Free Software Foundation, Inc.               --
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

with GPR2.Project.Name_Values;

private with GPR2.Project.Registry.Attribute;

package GPR2.Project.Typ is

   type Object is new Name_Values.Object with private;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   overriding function Image
     (Self : Object; Name_Len : Natural := 0) return String;
   --  Returns a string representation

private

   use all type GPR2.Project.Registry.Attribute.Value_Kind;
   use type GPR2.Project.Name_Values.Object;

   type Object is new Name_Values.Object with null record
     with Dynamic_Predicate =>
       Name_Values.Object (Object) = Name_Values.Undefined
       or else Object.Kind = List;

   Undefined : constant Object := (Name_Values.Undefined with null record);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Typ;
