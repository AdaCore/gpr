--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Finalization;
with Ada.Iterator_Interfaces;

with GPR2.Build.View_Db;

private with GPR2.Build.View_Tables;

package GPR2.Build.Source.Sets is

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Source.Object;
   --  Represents the list of sources contained in a single View_Db. This
   --  Set cannot be used as a generic holder for sources. See
   --  GPR2.Build.Source.Lists for such container.

   Empty_Set : constant Object;

   type Source_Set_Option is
     (Unsorted,
      --  Iterate over the view db's hashed map: result is fast but unsorted
      Sorted,
      --  Use a local sorted container: more memory footprint but result is
      --  sorted
      Recurse
      --  Lookup the view's sources together with the view's visibility
      --  closure. Has to use a local container, so the result will be also
      --  sorted alphabetically.
     );

   type Filter_Data is interface;

   type Dummy_Filter_Data is new Filter_Data with null record;
   No_Data : Dummy_Filter_Data;

   type Filter_Function is access
     function (View   : GPR2.Project.View.Object;
               Source : GPR2.Build.Source_Base.Object'Class;
               Data   : Filter_Data'Class) return Boolean;
   --  Function that can be used to filter sources from the set.
   --  Must return True if the source is to be kept, false otherwise.
   --  The filter_data interface can be used to pass parameters to the
   --  filter.

   function Create
     (Db     : Build.View_Db.Object;
      Option : Source_Set_Option := Unsorted;
      Filter : Filter_Function := null;
      F_Data : Filter_Data'Class := No_Data) return Object;
   --  Create a source iterator set representing the sources stored in the
   --  view db object.
   --  If sorted is set, the iterated list is sorted alphabetically (but doing
   --  so is slower).
   --  If filter is set, the iterated list will only contain the sources that
   --  the filter allows.
   --  Important note: the Filter parameter is used by the returned object, so
   --  cannot be a nested subprogram, as filter may be called out of its
   --  nested context.

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Source.Object;

   function Has_Element (Position : Cursor) return Boolean;

   package Source_Iterators is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Source.Object;

   function Is_Empty (Self : Object) return Boolean;

   function Iterate
     (Self : Object) return Source_Iterators.Forward_Iterator'Class;

private

   use GPR2.Build.View_Tables;

   function Less (P1, P2 : Filename_Type) return Boolean;

   --  Use an ordered path with our custom Less function
   package Path_Source_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Filename_Type,
      Element_Type => Source_Proxy,
      "<"          => Less);

   type Cursor (From_View_Db : Boolean := False) is record
      Db : Build.View_Db.Object;

      case From_View_Db is
         when True =>
            --  we iterate directly the view db's "Sources" list
            Current_Src    : Filename_Source_Maps.Cursor;
         when False =>
            --  we iterate the list in the iterator
            Current_Path  : Path_Source_Maps.Cursor;
      end case;
   end record;

   No_Element : constant Cursor := (others => <>);

   package Filter_Data_Holders is new Ada.Containers.Indefinite_Holders
     (Filter_Data'Class);

   type Object is tagged record
      Db     : Build.View_Db.Object;
      Option : Source_Set_Option := Unsorted;
      Filter : Filter_Function;
      F_Data : Filter_Data_Holders.Holder;
   end record;

   Empty_Set : constant Object := (others => <>);

   type Source_Iterator (From_View_Db : Boolean) is
     new Ada.Finalization.Controlled and Source_Iterators.Forward_Iterator
   with record
      --  we keep a reference to the view db for faster retrieval of
      --  the source items
      Db : Build.View_Db.Object;

      case From_View_Db is
         when False =>
            --  we have our own list of source proxy
            Paths  : Path_Source_Maps.Map;
         when True =>
            Filter : Filter_Function;
      end case;
   end record;

   overriding function First (Self : Source_Iterator) return Cursor;
   overriding function Next
     (Self     : Source_Iterator;
      Position : Cursor) return Cursor;
   overriding procedure Finalize (Self : in out Source_Iterator);

end GPR2.Build.Source.Sets;
