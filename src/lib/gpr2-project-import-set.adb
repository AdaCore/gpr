--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Import.Set is

   type Iterator is new Import_Iterator.Forward_Iterator with record
     Root : not null access constant Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.Set.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : Base_Name_Set.Constant_Reference_Type renames
              Base_Name_Set.Constant_Reference (Self.Set, Position.Current);
   begin
      return Constant_Reference_Type'
        (Import => Ref.Element.all'Unrestricted_Access,
         Ref    => Ref);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Object; Path_Name : GPR2.Path_Name.Object) return Boolean is
   begin
      return Self.Contains (Path_Name.Base_Name);
   end Contains;

   function Contains (Self : Object; Base_Name : Name_Type) return Boolean is
   begin
      return Self.Set.Contains (Base_Name);
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self      : in out Object;
      Path_Name : GPR2.Path_Name.Object) is
   begin
      Self.Set.Delete (Path_Name.Base_Name);
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Project.Import.Object is
   begin
      return Base_Name_Set.Element (Position.Current);
   end Element;

   function Element
     (Self      : Object;
      Path_Name : GPR2.Path_Name.Object) return Import.Object is
   begin
      return Self.Element (Path_Name.Base_Name);
   end Element;

   function Element
     (Self : Object; Base_Name : Name_Type) return Import.Object is
   begin
      return Self.Set (Base_Name);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
   begin
      return Cursor'(Current => Base_Name_Set.First (Iter.Root.Set));
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Base_Name_Set.Has_Element (Position.Current);
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert (Self : in out Object; Import : Project.Import.Object) is
   begin
      Self.Set.Insert (Import.Path_Name.Base_Name, Import);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
   begin
      return Self.Set.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Import_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Root => Self'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Object) return Containers.Count_Type is
   begin
      return Self.Set.Length;
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Iter);
   begin
      return Cursor'(Current => Base_Name_Set.Next (Position.Current));
   end Next;

end GPR2.Project.Import.Set;
