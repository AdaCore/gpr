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

with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Strings.Less_Case_Insensitive;

package GPR2.Project.Attribute.Set is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Attribute.Object;

   subtype Attribute_Set is Object;

   function Length (Self : Object) return Containers.Count_Type;

   function Is_Empty (Self : Object) return Boolean;

   function Contains
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := "") return Boolean;
   --  Check whether the set constains the attribute with the given Name and
   --  possibly the given Index.

   procedure Clear (Self : in out Object);
   --  Removes all elements from Self

   function Element
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := "") return Attribute.Object
     with Post =>
       (if Self.Contains (Name, Index)
        then Element'Result /= Attribute.Undefined
        else Element'Result = Attribute.Undefined);

   procedure Insert
     (Self : in out Object; Attribute : Project.Attribute.Object)
     with Pre => not Self.Contains (Attribute.Name, Attribute.Index);
   --  Insert Attribute into the set

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Attribute.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result /= Attribute.Undefined
        else Element'Result = Attribute.Undefined);

   function Find
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := "") return Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package Attribute_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Attribute : not null access constant Project.Attribute.Object) is private
     with Implicit_Dereference => Attribute;

   type Reference_Type
     (Attribute : not null access Project.Attribute.Object) is private
   with Implicit_Dereference => Attribute;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;

   function Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Reference_Type;

   function Iterate
     (Self  : Object;
      Name  : String := "";
      Index : String := "")
      return Attribute_Iterator.Forward_Iterator'Class;

   function Filter
     (Self  : Object;
      Name  : String := "";
      Index : String := "") return Object
     with Post => (if Name = "" and then Index = ""
                   then Filter'Result = Self);
   --  Returns an attribute set containing only the attribute corresponding to
   --  the given filter.

private

   --  An attribute set object is:
   --
   --     1. A map at the first level with the attribute name as key
   --
   --     2. The above map point to another map containing the actual
   --        attributes. The map key is the index for the attributes.

   package Set_Attribute is new Ada.Containers.Indefinite_Ordered_Maps
     (Value_Type, Attribute.Object);
   --  The key in this set is the attribute index

   package Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Set_Attribute.Map,
      Ada.Strings.Less_Case_Insensitive, Set_Attribute."=");
   --  The key in this Set is the attribute name (not case sensitive)

   type Cursor is record
      CM    : Set.Cursor;               -- main map cursor
      CA    : Set_Attribute.Cursor;     -- inner map cursor (Set below)
      Set   : access Set_Attribute.Map; -- Set ref to current inner map
   end record;

   No_Element : constant Cursor :=
                  (Set.No_Element,
                   Set_Attribute.No_Element,
                   null);

   type Constant_Reference_Type
     (Attribute : not null access constant Project.Attribute.Object)
   is null record;

   type Reference_Type
     (Attribute : not null access Project.Attribute.Object)
   is null record;

   type Object is tagged record
      Attributes : Set.Map;
      Length     : Containers.Count_Type := 0;
   end record;

end GPR2.Project.Attribute.Set;
