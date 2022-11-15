--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Source iterator for View DataBase.
--
--  The goal here is to have an iterator rather than just a function returning
--  the list, because this list can be very large so the function call could,
--  if not inlined, cause unnecessary copies of a large object (the more
--  sources, the larger.
--
--  Also, this allows to have sorted/unsorted iteration while still maintening
--  the list as a hashed container for efficiency of source loading.

with Ada.Iterator_Interfaces;

with GPR2.Build.View_Db;

private with Ada.Containers.Ordered_Sets;
private with GPR2.Build.View_Tables;

package GPR2.Build.Source_Info.Sets is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Source_Info.Object;

   Empty_Set : constant Object;

   function Is_Empty (Self : Object) return Boolean;

   function Create (Db : Build.View_Db.Object;
                    Sorted : Boolean := False) return Object;
   --  Create a source iterator set representing the sources stored in the
   --  view db object.
   --  If sorted is set, the iterated list is sorted alphabetically (but doing
   --  so is slower).

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Source_Info.Object;

   function Has_Element (Position : Cursor) return Boolean;

   package Source_Iterators is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Element : not null access constant Source_Info.Object) is private
     with Implicit_Dereference => Element;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;

   function Iterate
     (Self : Object) return Source_Iterators.Forward_Iterator'Class;

private

   use GPR2.Build.View_Tables;

   package Path_Name_Sets is new Ada.Containers.Ordered_Sets
     (GPR2.Path_Name.Object,
      "<" => GPR2.Path_Name."<",
      "=" => GPR2.Path_Name."=");

   type Cursor is record
      Db            : Build.View_Db.Object;
      Sort          : Boolean := False;
      Current_Src   : Basename_Source_Maps.Cursor;
      Current_Path  : Path_Name_Sets.Cursor;
   end record;

   No_Element : constant Cursor := (others => <>);

   type Constant_Reference_Type
     (Element : not null access constant Source_Info.Object) is record
      Ref : Src_Info_Maps.Constant_Reference_Type (Element);
   end record;

   type Object is tagged record
      Db   : Build.View_Db.Object;
      Sort : Boolean := False;
   end record;

   Empty_Set : constant Object := (Db => <>, Sort => False);

end GPR2.Build.Source_Info.Sets;
