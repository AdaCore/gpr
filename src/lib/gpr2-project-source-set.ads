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
with GPR2.Containers;

private with Ada.Containers.Ordered_Sets;

package GPR2.Project.Source.Set is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Project.Source.Object;

   type Cursor is private;

   No_Element : constant Cursor;

   function Is_Empty (Self : Object) return Boolean;

   procedure Clear (Self : in out Object);

   function Length (Self : Object) return Containers.Count_Type;

   procedure Include (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;

   procedure Insert (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;

   procedure Insert
     (Self     : in out Object;
      Source   : Project.Source.Object;
      Position : out Cursor;
      Inserted : out Boolean)
     with Pre => Source.Is_Defined;
   --  Checks if an element equivalent to Source is already present in Self.
   --  If a match is found, Inserted is set to False and Position designates
   --  the matching element. Otherwise, Insert adds Source to Container;
   --  Inserted is set to True and Position designates the newly-inserted
   --  element.

   procedure Union (Self : in out Object; Sources : Object);
   --  Inserts into Self the elements of Source that are not equivalent to some
   --  element already in Self.

   function Contains
     (Self   : Object;
      Source : Project.Source.Object) return Boolean
     with Pre => Source.Is_Defined;
   --  Returns True if Self constains Source

   procedure Delete
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Deletes source from set

   procedure Replace
     (Self   : in out Object;
      Source : Project.Source.Object)
     with Pre => Source.Is_Defined and then Self.Contains (Source);
   --  Replaces Source in Self

   procedure Replace
     (Self     : in out Object;
      Position : Cursor;
      Source   : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Replaces Source in Self at the given Position

   function First_Element (Self : Object) return Project.Source.Object;

   function Find
     (Self : Object; Source : Project.Source.Object) return Cursor
     with Pre => Source.Is_Defined;

   function Element (Position : Cursor) return Project.Source.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Has_Element (Position : Cursor) return Boolean
     with Inline;

   package Source_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Source : not null access constant Project.Source.Object) is private
     with Implicit_Dereference => Source;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type with Inline;

   Empty_Set : constant Object;

   type Source_Filter is
     (S_Compilable,  --  Compilable sources only
      S_Spec,        --  Specs only
      S_Body,        --  Body
      S_Separate,    --  Separate
      S_All);

   function Iterate
     (Self   : Object;
      Filter : Source_Filter := S_All)
      return Source_Iterator.Forward_Iterator'Class;

private

   package Set is new Ada.Containers.Ordered_Sets (Project.Source.Object);

   type Object is tagged record
      S : aliased Set.Set;
   end record;

   type Cursor is record
      Current : Set.Cursor;
   end record;

   No_Element : constant Cursor := (Current => Set.No_Element);

   type Constant_Reference_Type
     (Source : not null access constant Project.Source.Object) is record
      Ref : Set.Constant_Reference_Type (Source);
   end record;
   --  We keep the ref in the object as well to keep the busy state of
   --  the set.

   Empty_Set : constant Object := Object'(S => Set.Empty_Set);

   function Find
     (Self : Object; Source : Project.Source.Object) return Cursor
   is (Current => Self.S.Find (Source));

   function Length (Self : Object) return Containers.Count_Type is
     (Self.S.Length);

   function Is_Empty (Self : Object) return Boolean is
     (Self.S.Is_Empty);

end GPR2.Project.Source.Set;
