--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
   function Element (Position : Cursor) return Source_Part with Inline;

   type Constant_Reference_Type
     (Part   : not null access constant Source_Part;
      Sorted : Boolean) is private
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
     (Part   : not null access constant Source_Part;
      Sorted : Boolean)
   is record
      case Sorted is
         when True =>
            SRef : Source_Part_Ordered_Sets.Constant_Reference_Type (Part);
         when False =>
            HRef : Source_Part_Hashed_Sets.Constant_Reference_Type (Part);
      end case;
   end record;

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
