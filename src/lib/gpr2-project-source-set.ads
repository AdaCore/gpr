------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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

   subtype Source_Set is Object;

   function Is_Empty (Self : Object) return Boolean;

   procedure Clear (Self : in out Object);

   function Length (Self : Object) return Containers.Count_Type;

   procedure Insert (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;

   function Contains
     (Self : Object; Source : Project.Source.Object) return Boolean
     with Pre => Source.Is_Defined;

   procedure Replace (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;

   function First_Element (Self : Object) return Project.Source.Object;

   type Cursor is private;

   No_Element : constant Cursor;

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
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;

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

   S_Compilable : constant Source_Filter := 1;
   S_Spec       : constant Source_Filter := 2;
   S_Body       : constant Source_Filter := 4;
   S_Separate   : constant Source_Filter := 8;
   S_All        : constant Source_Filter :=
                    S_Compilable + S_Spec + S_Body + S_Separate;

end GPR2.Project.Source.Set;
