--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Unit_Info.List is

   type Unit_Iterator is new Unit_Iterators.Forward_Iterator with record
      List : access constant Object;
   end record;

   overriding function First
     (Object : Unit_Iterator) return Cursor;

   overriding function Next
     (Object   : Unit_Iterator;
      Position : Cursor) return Cursor;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : constant Unit_Map.Constant_Reference_Type :=
              Self.Units.Constant_Reference (Unit_Map.Cursor (Position));
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Constant_Reference;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Unit_Index) return Constant_Reference_Type
   is
   begin
      return Self.Constant_Reference (Cursor (Self.Units.Find (Position)));
   end Constant_Reference;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self  : in out Object;
      Index : Unit_Index)
   is
   begin
      Self.Units.Delete (Index);

      if Self.Units.Is_Empty then
         Self.Has_Index := False;
      end if;
   end Delete;

   -----------
   -- First --
   -----------

   overriding function First
     (Object : Unit_Iterator) return Cursor is
   begin
      return Cursor (Object.List.Units.First);
   end First;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self    : in out Object;
      Element : Unit_Info.Object)
   is
   begin
      if Self.Units.Is_Empty then
         Self.Has_Index := Element.Index /= No_Index;

      else
         pragma Assert (Element.Index = No_Index or else Self.Has_Index);
      end if;

      Self.Units.Include (Element.Index, Element);
   end Include;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self    : in out Object;
      Element : Unit_Info.Object)
   is
   begin
      Self.Units.Insert (Element.Index, Element);

      if Element.Index /= No_Index then
         Self.Has_Index := True;
      end if;
   end Insert;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Unit_Iterators.Forward_Iterator'Class
   is
   begin
      return Result : Unit_Iterator do
         Result := (List => Self'Unchecked_Access);
      end return;
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object   : Unit_Iterator;
      Position : Cursor) return Cursor
   is
      Result : Unit_Map.Cursor := Unit_Map.Cursor (Position);
   begin
      Unit_Map.Next (Result);
      return Cursor (Result);
   end Next;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Reference_Type
   is
      Ref : constant Unit_Map.Reference_Type :=
              Self.Units.Reference (Unit_Map.Cursor (Position));
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Reference;

end GPR2.Build.Unit_Info.List;
