------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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
     with Post =>
       (if Self.Contains (Name, Index, At_Pos)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

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
      With_Defaults : Boolean                := False)
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

   function Has_Source_Dirs (Self : Object) return Boolean;
   function Source_Dirs     (Self : Object) return Attribute.Object;

   function Has_Source_Files (Self : Object) return Boolean;
   function Source_Files     (Self : Object) return Attribute.Object;

   function Has_Excluded_Source_Files (Self : Object) return Boolean;
   function Excluded_Source_Files     (Self : Object) return Attribute.Object;

   function Has_Excluded_Source_List_File (Self : Object) return Boolean;
   function Excluded_Source_List_File (Self : Object) return Attribute.Object;

   function Has_Source_List_File (Self : Object) return Boolean;
   function Source_List_File     (Self : Object) return Attribute.Object;

   function Has_Library_Interface (Self : Object) return Boolean;
   function Library_Interface     (Self : Object) return Attribute.Object;

   function Has_Interfaces (Self : Object) return Boolean;
   function Interfaces     (Self : Object) return Attribute.Object;

private

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
     (Self.Contains (Registry.Attribute.Languages));

   function Languages (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Languages));

   function Has_Source_Dirs (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Source_Dirs));

   function Source_Dirs (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Source_Dirs));

   function Has_Source_Files (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Source_Files));

   function Source_Files (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Source_Files));

   function Has_Excluded_Source_Files (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Excluded_Source_Files));

   function Excluded_Source_Files (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Excluded_Source_Files));

   function Has_Excluded_Source_List_File (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Excluded_Source_List_File));

   function Excluded_Source_List_File (Self : Object) return Attribute.Object
     is (Self.Element (Registry.Attribute.Excluded_Source_List_File));

   function Has_Source_List_File (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Source_List_File));

   function Source_List_File (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Source_List_File));

   function Has_Library_Interface (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Library_Interface));

   function Library_Interface (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Library_Interface));

   function Has_Interfaces (Self : Object) return Boolean is
     (Self.Contains (Registry.Attribute.Interfaces));

   function Interfaces (Self : Object) return Attribute.Object is
     (Self.Element (Registry.Attribute.Interfaces));

end GPR2.Project.Attribute.Set;
