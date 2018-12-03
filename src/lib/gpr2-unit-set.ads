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

private with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Unit.Set is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Unit.Object;

   subtype Unit_Set is Object;

   function Is_Empty (Self : Object) return Boolean;

   procedure Clear (Self : in out Object);

   function Length (Self : Object) return Containers.Count_Type;

   procedure Insert
     (Self : in out Object; Name : Name_Type; Unit : GPR2.Unit.Object)
     with Pre => Unit.Is_Defined;

   function Contains (Self : Object; Name : Name_Type) return Boolean;

   procedure Replace
     (Self : in out Object; Name : Name_Type; Unit : GPR2.Unit.Object)
     with Pre => Unit.Is_Defined;

   function First_Element (Self : Object) return Unit.Object;

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Unit.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Element (Self : Object; Name : Name_Type) return Unit.Object;

   function Has_Element (Position : Cursor) return Boolean;

   package Unit_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Unit : not null access constant GPR2.Unit.Object) is private
     with Implicit_Dereference => Unit;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;

   function Iterate
     (Self : Object) return Unit_Iterator.Forward_Iterator'Class;

private

   package Set is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, GPR2.Unit.Object);

   type Object is tagged record
      S : Set.Map;
   end record;

   type Cursor is record
      Current : Set.Cursor;
   end record;

   No_Element : constant Cursor := (Current => Set.No_Element);

   type Constant_Reference_Type
     (Unit : not null access constant GPR2.Unit.Object) is null record;

end GPR2.Unit.Set;
