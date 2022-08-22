--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package defines the list used to represent the units of a compilation
--  unit based source.
--  This list in particular ensures that all elements in there are properly
--  ordered and that Unit.Index can be used to access the unit.

with Ada.Containers;
with Ada.Iterator_Interfaces;
private with Ada.Containers.Vectors;

package GPR2.Unit.List is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Unit.Object;

   Empty_List : constant Object;

   type Cursor is private;

   No_Element : constant Cursor;

   function Is_Empty (Self : Object) return Boolean;
   --  True if the list has no element

   function Is_Indexed_List (Self : Object) return Boolean;
   --  True if the elements in the list are indexed.

   procedure Clear (Self : in out Object);

   function Length (Self : Object) return Ada.Containers.Count_Type;

   procedure Insert (Self : in out Object; Element : Unit.Object)
     with Pre => Element.Is_Defined;
   --  Insert a new Unit to the list.
   --  If the element can't be inserted, then Constraint_Error is raised.
   --  An element can only be inserted if:
   --  * the list is empty
   --  * or the list is indexed and the element defines an index, and the list
   --    has no element for that index

   procedure Insert (Self     : in out Object;
                     Element  : Unit.Object;
                     Position : out Cursor;
                     Inserted : out Boolean)
     with Pre => Element.Is_Defined;
   --  Insert a new Unit to the list.
   --  If the element can't be inserted, then Inserted is not set.
   --  An element can only be inserted if:
   --  * the list is empty
   --  * or the list is indexed and the element defines an index, and the list
   --    has no element for that index.

   function Element (Self  : Object;
                     Index : Unit_Index) return Unit.Object
     with Post =>
       (if Self.Has_Element (Index)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Element (Position : Cursor) return Unit.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Has_Element (Self : Object; Index : Unit_Index) return Boolean
     with Inline;

   function Has_Element (Position : Cursor) return Boolean
     with Inline;

   type Reference_Type (Unit : not null access GPR2.Unit.Object) is private
   with
      Implicit_Dereference => Unit;

   type Constant_Reference_Type
     (Unit : not null access constant GPR2.Unit.Object) is private
     with Implicit_Dereference => Unit;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type with Inline;
   function Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Reference_Type with Inline;

   function Constant_Reference
     (Self  : aliased Object;
      Index : Unit_Index) return Constant_Reference_Type
     with Inline, Pre => Self.Has_Element (Index);
   function Reference
     (Self  : aliased in out Object;
      Index : Unit_Index) return Reference_Type
     with Inline, Pre => Self.Has_Element (Index);

   package Unit_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate
     (Self : Object)
      return Unit_Iterator.Forward_Iterator'Class;

private

   --  Default value for index is 0, so is not in range of Multi_Unit_Index.
   --  This triggers a compiler warning.
   pragma Warnings (Off, "value not in range");
   package Unit_Vectors is new Ada.Containers.Vectors
     (Multi_Unit_Index, Unit.Object);
   pragma Warnings (On, "value not in range");

   type Object is tagged record
      Multi_Unit : Boolean := False;
      List       : Unit_Vectors.Vector;
   end record;

   type Cursor is record
      Current : Unit_Vectors.Cursor;
   end record;

   type Constant_Reference_Type
     (Unit : not null access constant GPR2.Unit.Object) is record
      Ref : Unit_Vectors.Constant_Reference_Type (Unit);
   end record;

   type Reference_Type
     (Unit : not null access GPR2.Unit.Object) is record
      Ref : Unit_Vectors.Reference_Type (Unit);
   end record;

   No_Element : constant Cursor := (Current => Unit_Vectors.No_Element);

   Empty_List : constant Object := (others => <>);

end GPR2.Unit.List;
