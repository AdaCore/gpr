------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
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

   subtype Source_Set is Object;

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
     (Self : Object; Source : Project.Source.Object) return Boolean
     with Pre => Source.Is_Defined;

   procedure Replace (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;

   procedure Replace
     (Self : in out Object; Position : Cursor; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;

   function First_Element (Self : Object) return Project.Source.Object;

   function Find
     (Self : Object; Source : Project.Source.Object) return Cursor
     with Pre => Source.Is_Defined;

   function Element (Position : Cursor) return Project.Source.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Has_Element (Position : Cursor) return Boolean;

   package Source_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Source : not null access constant Project.Source.Object) is private
     with Implicit_Dereference => Source;

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return Constant_Reference_Type;

   Empty_Set : constant Object;

   type Source_Filter is mod 2 ** 8;

   S_Compilable : constant Source_Filter;
   S_Spec       : constant Source_Filter;
   S_Body       : constant Source_Filter;
   S_Separate   : constant Source_Filter;
   S_All        : constant Source_Filter;

   function Iterate
     (Self   : Object;
      Filter : Source_Filter := S_All)
      return Source_Iterator.Forward_Iterator'Class;

private

   package Set is new Ada.Containers.Ordered_Sets (Project.Source.Object);

   type Object is tagged record
      S : Set.Set;
   end record;

   type Cursor is record
      Current : Set.Cursor;
   end record;

   No_Element : constant Cursor := (Current => Set.No_Element);

   type Constant_Reference_Type
     (Source : not null access constant Project.Source.Object) is null record;

   Empty_Set : constant Object := Object'(S => Set.Empty_Set);

   S_Compilable : constant Source_Filter := 1;
   S_Spec       : constant Source_Filter := 2;
   S_Body       : constant Source_Filter := 4;
   S_Separate   : constant Source_Filter := 8;
   S_All        : constant Source_Filter :=
                    S_Compilable + S_Spec + S_Body + S_Separate;

   function Find
     (Self : Object; Source : Project.Source.Object) return Cursor
   is
     (Current => Self.S.Find (Source));

end GPR2.Project.Source.Set;
