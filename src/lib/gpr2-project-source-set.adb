--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Source.Set is

   type Iterator is new Source_Iterator.Forward_Iterator with record
      Root   : not null access constant Set.Set;
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
      Self.S.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : constant Set.Constant_Reference_Type :=
              Set.Constant_Reference (Self.S, Position.Current);
   begin
      return Constant_Reference_Type'
        (Source => Ref.Element.all'Unrestricted_Access,
         Ref    => Ref);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Object; Source : Project.Source.Object) return Boolean is
   begin
      return Self.S.Contains (Source);
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.S.Delete (Source);
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Project.Source.Object is
   begin
      return Set.Element (Position.Current);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
   begin
      return Cursor'(Current => Iter.Root.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Self : Object) return Project.Source.Object is
   begin
      return Self.S.First_Element;
   end First_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Set.Has_Element (Position.Current);
   end Has_Element;

   -------------
   -- Include --
   -------------

   procedure Include (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.S.Include (Source);
   end Include;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self     : in out Object;
      Source   : Project.Source.Object;
      Position : out Cursor;
      Inserted : out Boolean) is
   begin
      Self.S.Insert (Source, Position.Current, Inserted);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.S.Insert (Source);
   end Insert;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self   : Object) return Source_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Root => Self.S'Unrestricted_Access);
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      Next : constant Set.Cursor := Set.Next (Position.Current);
   begin
      return Cursor'(Current => Next);
   end Next;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self     : in out Object;
      Position : Cursor;
      Source   : Project.Source.Object) is
   begin
      Self.S.Replace_Element (Position.Current, Source);
   end Replace;

   procedure Replace (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.S.Replace (Source);
   end Replace;

   -----------
   -- Union --
   -----------

   procedure Union (Self : in out Object; Sources : Object) is
   begin
      Self.S.Union (Sources.S);
   end Union;

end GPR2.Project.Source.Set;
