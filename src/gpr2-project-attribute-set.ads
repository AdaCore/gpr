------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with Ada.Containers.Indefinite_Ordered_Maps;

private with Ada.Strings.Unbounded;

package GPR2.Project.Attribute.Set is

   package Set is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, Object);

   type Object is new Set.Map with private;

   subtype Cursor is Set.Cursor;

   function Iterate_Filter
     (Self     : Object;
      Name     : String := "";
      Language : String := "")
      return Set.Map_Iterator_Interfaces.Reversible_Iterator'Class;
   --  An iterator on an attribute set which can filter out based on the name
   --  or the language (or both) of the attribute.

   function Filter
     (Self     : Object;
      Name     : String := "";
      Language : String := "") return Object
     with Post => (if Name = "" and then Language = ""
                   then Filter'Result = Self);
   --  Returns an attribute set containing only the attribute corresponding to
   --  the given filter.

private

   use Ada.Strings.Unbounded;

   type Object is new Set.Map with null record;

   type Iterator is
     new Set.Map_Iterator_Interfaces.Reversible_Iterator with
   record
      Object   : Set.Map;
      Position : Cursor;
      Name     : Unbounded_String;
      Language : Unbounded_String;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object : Iterator; Position : Cursor) return Cursor;
   overriding function Previous
     (Object : Iterator; Position : Cursor) return Cursor;

end GPR2.Project.Attribute.Set;
