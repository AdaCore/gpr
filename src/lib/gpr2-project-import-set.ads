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

--  This container is design to contain the set of import for a specific
--  project. It is used to detect duplicate import project in with clause
--  for example. We also have fast check/access to any import project name.

with Ada.Iterator_Interfaces;
with GPR2.Containers;

private with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Project.Import.Set is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Project.Import.Object;

   function Is_Empty (Self : Object) return Boolean;

   procedure Clear (Self : in out Object);

   function Length (Self : Object) return Containers.Count_Type;

   procedure Insert (Self : in out Object; Import : Project.Import.Object);

   procedure Delete (Self : in out Object; Path_Name : GPR2.Path_Name.Object)
     with Pre => Self.Contains (Path_Name);

   function Contains
     (Self : Object; Path_Name : GPR2.Path_Name.Object) return Boolean;

   function Contains
     (Self : Object; Base_Name : Name_Type) return Boolean;

   function Element (Self : Object; Base_Name : Name_Type) return Import.Object
     with Pre => Self.Contains (Base_Name);

   function Element
     (Self : Object; Path_Name : GPR2.Path_Name.Object) return Import.Object
     with Pre => Self.Contains (Path_Name);

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Project.Import.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result /= Project.Import.Undefined
        else Element'Result = Project.Import.Undefined);

   function Has_Element (Position : Cursor) return Boolean;

   package Import_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Import : not null access constant Project.Import.Object) is private
     with Implicit_Dereference => Import;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;

   function Iterate
     (Self : Object) return Import_Iterator.Forward_Iterator'Class;

private

   package Base_Name_Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Project.Import.Object);

   type Object is tagged record
      Set : Base_Name_Set.Map;
   end record;

   type Cursor is record
      Current : Base_Name_Set.Cursor;
   end record;

   No_Element : constant Cursor :=
                  Cursor'(Current => Base_Name_Set.No_Element);

   type Constant_Reference_Type
     (Import : not null access constant Project.Import.Object) is null record;

end GPR2.Project.Import.Set;
