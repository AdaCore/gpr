------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

--  This package defines a Set of Source_Parts

with Ada.Iterator_Interfaces;

private with Ada.Containers.Hashed_Sets;
private with Ada.Containers.Ordered_Sets;

package GPR2.Project.Source.Part_Set is

   type Object (Sorted : Boolean) is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Source_Part;
   --  A Set of Source_Part objects

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;
   function Element (Position : Cursor) return Source_Part;

   type Constant_Reference_Type
     (Part : not null access constant Source_Part) is private
     with Implicit_Dereference => Part;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type with Inline;

   package Source_Part_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Is_Empty (Self : Object) return Boolean
     with Inline;

   procedure Clear (Self : in out Object);

   procedure Insert
     (Self    : in out Object;
      Element : Source_Part)
     with Inline;
   --  Insert Element in Self. If Element is alreadu in Self, this raises
   --  Constraint_Error.

   procedure Insert
     (Self     : in out Object;
      Element  : Source_Part;
      Position : out Cursor;
      Inserted : out Boolean)
     with Inline;
   --  Try to insert Element in Self.
   --  Position: the returned cursor points to the newly inserted element in
   --    Self, or points to the previously existing element.
   --  Inserted: is set if Element has actually been inserted.

   procedure Union
     (Self  : in out Object;
      Other : Object)
     with Inline;
   --  Add to Self all non previously existing elements from Other.

   function Iterate (Self : Object)
      return Source_Part_Iterator.Forward_Iterator'Class;

private

   use type Ada.Containers.Hash_Type;

   function Hash (Object : Source_Part) return Ada.Containers.Hash_Type
   is (Object.Source.Path_Name.Hash +
         Ada.Containers.Hash_Type (Object.Index));

   package Source_Part_Hashed_Sets is new Ada.Containers.Hashed_Sets
     (Source_Part, Hash, "=");

   package Source_Part_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Source_Part);

   type Object (Sorted : Boolean) is tagged record
      case Sorted is
         when True =>
            SS : aliased Source_Part_Ordered_Sets.Set;
         when False =>
            HS : aliased Source_Part_Hashed_Sets.Set;
      end case;
   end record;

   type Cursor (Sorted : Boolean := True) is record
      case Sorted is
         when True =>
            SC : Source_Part_Ordered_Sets.Cursor;
         when False =>
            HC : Source_Part_Hashed_Sets.Cursor;
      end case;
   end record;

   type Iterator (Sorted : Boolean) is
     new Source_Part_Iterator.Forward_Iterator
   with record
      case Sorted is
         when True =>
            SRoot : not null access constant Source_Part_Ordered_Sets.Set;
         when False =>
            HRoot : not null access constant Source_Part_Hashed_Sets.Set;
      end case;
   end record;

   type Constant_Reference_Type
     (Part   : not null access constant Source_Part)
   is null record;

   overriding function First (Iter : Iterator) return Cursor is
     (if Iter.Sorted
      then (Sorted => True, SC => Iter.SRoot.First)
      else (Sorted => False, HC => Iter.HRoot.First));
   overriding function Next (Iter : Iterator; Position : Cursor) return Cursor
   is (if Iter.Sorted
       then (Sorted => True,
             SC     => Source_Part_Ordered_Sets.Next (Position.SC))
       else (Sorted => False,
             HC     => Source_Part_Hashed_Sets.Next (Position.HC)));

   function Is_Empty (Self : Object) return Boolean is
     (if Self.Sorted
      then Self.SS.Is_Empty
      else Self.HS.Is_Empty);

   function Has_Element (Position : Cursor) return Boolean is
     (if Position.Sorted
      then Source_Part_Ordered_Sets.Has_Element (Position.SC)
      else Source_Part_Hashed_Sets.Has_Element (Position.HC));

   function Element (Position : Cursor) return Source_Part is
     (if Position.Sorted
      then Source_Part_Ordered_Sets.Element (Position.SC)
      else Source_Part_Hashed_Sets.Element (Position.HC));

end GPR2.Project.Source.Part_Set;
