--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Project.Attribute.Set is

   use type Attribute_Index.Object;

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Attribute.Object;

   Empty_Set : constant Object;

   function Length (Self : Object) return Containers.Count_Type;

   function Is_Empty (Self : Object) return Boolean;

   function Contains
     (Self   : Object;
      Name   : Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index) return Boolean;
   --  Checks whether the set contains the attribute with the given Name and
   --  possibly the given Index.

   function Contains
     (Self      : Object;
      Attribute : Project.Attribute.Object) return Boolean;
   --  Returns True if the set contains the given attribute

   procedure Clear (Self : in out Object);
   --  Removes all elements from Self

   function Element
     (Self   : Object;
      Name   : Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index) return Attribute.Object
     with Post => (if Self.Contains (Name, Index, At_Pos)
                   then Element'Result.Is_Defined
                   else not Element'Result.Is_Defined),
          Inline;

   procedure Insert
     (Self : in out Object; Attribute : Project.Attribute.Object)
     with Pre  => not Self.Contains (Attribute),
          Post => Self.Contains (Attribute);
   --  Inserts Attribute into the set

   procedure Include
     (Self : in out Object; Attribute : Project.Attribute.Object)
     with Post => Self.Contains (Attribute);
   --  Inserts or replaces an Attribute into the set

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Attribute.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Find
     (Self   : Object;
      Name   : Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index) return Cursor;

   function Find
     (Self      : Object;
      Attribute : Project.Attribute.Object) return Cursor;

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
     (Self          : Object;
      Name          : Optional_Attribute_Id  := No_Attribute;
      Index         : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos        : Unit_Index             := No_Index;
      With_Defaults : Boolean                := True)
      return Attribute_Iterator.Forward_Iterator'Class;

   function Filter
     (Self   : Object;
      Name   : Optional_Attribute_Id  := No_Attribute;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index) return Object
     with Post => (if Name = No_Attribute and then not Index.Is_Defined
                   then Filter'Result = Self);
   --  Returns an attribute set containing only the attribute corresponding to
   --  the given filter.

   --  Some helper routines on attributes in the set

   function Has_Languages (Self : Object) return Boolean;
   function Languages     (Self : Object) return Attribute.Object;

private

   package PRA renames GPR2.Project.Registry.Attribute;

   --  An attribute set object is:
   --
   --     1. A map at the first level with the attribute name as key
   --
   --     2. The above map point to another map containing the actual
   --        attributes. The map key is the index for the attributes.

   package Set_Attribute is new Ada.Containers.Indefinite_Ordered_Maps
     (Value_At_Pos, Attribute.Object);
   --  The key in this set is the attribute index and 'at' part

   package Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Attribute_Id, Set_Attribute.Map, "<", Set_Attribute."=");
   --  The key in this Set is the attribute name (not case sensitive)

   type Cursor is record
      CM  : Set.Cursor;               -- main map cursor
      CA  : Set_Attribute.Cursor;     -- inner map cursor (Set below)
   end record;

   No_Element : constant Cursor :=
                  (Set.No_Element,
                   Set_Attribute.No_Element);

   type Constant_Reference_Type
     (Attribute : not null access constant Project.Attribute.Object)
   is record
      --  We need to keep the underlying reference so that it is not cleared
      --  upon return of the getter, and so that the container has the proper
      --  busy state
      Ref : Set_Attribute.Constant_Reference_Type (Attribute);
   end record;

   type Reference_Type
     (Attribute : not null access Project.Attribute.Object)
   is record
      --  We need to keep the underlying reference so that it is not cleared
      --  upon return of the getter, and so that the container has the proper
      --  busy state
      Ref : Set_Attribute.Reference_Type (Attribute);
   end record;

   type Object is tagged record
      Attributes : aliased Set.Map;
      Length     : Containers.Count_Type := 0;
   end record;

   Empty_Set : constant Object := (others => <>);

   function Has_Languages (Self : Object) return Boolean is
     (Self.Contains (Name => PRA.Languages.Attr));

   function Languages (Self : Object) return Attribute.Object is
     (Self.Element (Name => PRA.Languages.Attr));

end GPR2.Project.Attribute.Set;
