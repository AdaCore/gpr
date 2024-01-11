--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Build.Unit_Info.List is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Unit_Info.Object;
   --  The list is used to store units. It handles single unit and multi-unit
   --  cases.

   Empty_List : constant Object;

   function Is_Indexed_List (Self : Object) return Boolean;
   --  True if units in the list have index

   function Is_Empty (Self : Object) return Boolean;
   --  True if Self has no units

   function Length (Self : Object) return Natural;
   --  Number of units in Self

   procedure Insert
     (Self    : in out Object;
      Element : Unit_Info.Object);

   procedure Include
     (Self    : in out Object;
      Element : Unit_Info.Object);

   type Cursor is private;

   No_Element : constant Cursor;

   function Contains (Self  : Object;
                      Index : Unit_Index) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

   function Element (Position : Cursor) return Unit_Info.Object;
   function Element
     (Self  : Object;
      Index : Unit_Index) return Unit_Info.Object;
   --  Get a single unit

   type Reference_Type (Element : not null access Unit_Info.Object) is private
     with Implicit_Dereference => Element;

   function Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Reference_Type;

   type Constant_Reference_Type
     (Element : not null access constant Unit_Info.Object) is private
     with Implicit_Dereference => Element;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;
   function Constant_Reference
     (Self     : aliased Object;
      Position : Unit_Index) return Constant_Reference_Type;

   package Unit_Iterators is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate
     (Self : Object) return Unit_Iterators.Forward_Iterator'Class;

private

   package Unit_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (Unit_Index, Unit_Info.Object);

   type Reference_Type (Element : not null access Unit_Info.Object) is record
      Ref : Unit_Map.Reference_Type (Element);
   end record;

   type Constant_Reference_Type
     (Element : not null access constant Unit_Info.Object)
   is record
      Ref : Unit_Map.Constant_Reference_Type (Element);
   end record;

   type Object is tagged record
      Has_Index : Boolean := False;
      Units     : Unit_Map.Map;
   end record;

   type Cursor is new Unit_Map.Cursor;

   Empty_List : constant Object := (others => <>);
   No_Element : constant Cursor := Cursor (Unit_Map.No_Element);

   function Is_Empty (Self : Object) return Boolean is
     (Self.Units.Is_Empty);

   function Is_Indexed_List (Self : Object) return Boolean is
     (Self.Has_Index);

   function Length (Self : Object) return Natural is
     (Natural (Self.Units.Length));

   function Element (Self  : Object;
                     Index : Unit_Index) return Unit_Info.Object
   is (Self.Units.Element (Index));

   overriding function Has_Element (Position : Cursor) return Boolean is
     (Unit_Map.Has_Element (Unit_Map.Cursor (Position)));

   function Contains (Self  : Object;
                         Index : Unit_Index) return Boolean is
     (Self.Units.Contains (Index));

   overriding function Element (Position : Cursor) return Unit_Info.Object is
     (Unit_Map.Element (Unit_Map.Cursor (Position)));


end GPR2.Build.Unit_Info.List;
