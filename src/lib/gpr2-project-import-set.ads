--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This container is designed to contain the set of imports for a specific
--  project. It is used to detect duplicate imported projects in with clauses
--  for example. We also have fast check/access for any imported project name.

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

   function Contains (Self : Object; Base_Name : Name_Type) return Boolean;

   function Element
     (Self : Object; Base_Name : Name_Type) return Import.Object
     with Pre => Self.Contains (Base_Name);

   function Element
     (Self : Object; Path_Name : GPR2.Path_Name.Object) return Import.Object
     with Pre => Self.Contains (Path_Name);

   type Cursor is private;

   No_Element : constant Cursor;

   function Find
     (Self : Object; Path_Name : GPR2.Path_Name.Object) return Cursor;

   function Find
     (Self : Object; Base_Name : Name_Type) return Cursor;

   function Element (Position : Cursor) return Project.Import.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

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
     (Import : not null access constant Project.Import.Object) is record
      --  We need to keep the underlying reference so that it is not cleared
      --  upon return of the getter, and so that the container has the proper
      --  busy state
      Ref : Base_Name_Set.Constant_Reference_Type (Import);
   end record;

   function Find
     (Self : Object; Path_Name : GPR2.Path_Name.Object) return Cursor
   is
     ((Current => Self.Set.Find (Path_Name.Base_Name)));

   function Find
     (Self : Object; Base_Name : Name_Type) return Cursor
   is
     ((Current => Self.Set.Find (Base_Name)));

end GPR2.Project.Import.Set;
