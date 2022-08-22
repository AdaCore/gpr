--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Unit.List is

   type Iterator is new Unit_Iterator.Forward_Iterator with record
      Root : not null access constant Object;
   end record;

   overriding function First (Iter : Iterator) return Cursor;
   overriding function Next (Iter : Iterator; Position : Cursor) return Cursor;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.List.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self : aliased Object; Index : Unit_Index) return Constant_Reference_Type
   is
      Cursor : Unit_Vectors.Cursor;
   begin
      if Self.Multi_Unit then
         Cursor := Self.List.To_Cursor (Index);
      else
         Cursor := Self.List.First;
      end if;

      return Self.Constant_Reference (Position => (Current => Cursor));
   end Constant_Reference;

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return Constant_Reference_Type
   is
      Ref : constant Unit_Vectors.Constant_Reference_Type :=
              Self.List.Constant_Reference (Position.Current);
   begin
      return (Unit => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Constant_Reference;

   -------------
   -- Element --
   -------------

   function Element (Self : Object; Index : Unit_Index) return Unit.Object is
   begin
      if Self.Multi_Unit then
         return Self.List.Element (Index);
      else
         pragma Assert (Index = No_Index);
         return Self.List.First_Element;
      end if;
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Unit.Object is
   begin
      return Unit_Vectors.Element (Position.Current);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
   begin
      return (Current => Iter.Root.List.First);
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Object; Index : Unit_Index) return Boolean is
   begin
      if Self.Multi_Unit then
         if Index not in Multi_Unit_Index then
            return False;
         else
            return Self.List.Last_Index >= Index;
         end if;
      elsif Index = No_Index then
         return not Self.List.Is_Empty;
      else
         return False;
      end if;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Unit_Vectors.Has_Element (Position.Current);
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert (Self : in out Object; Element : Unit.Object) is
      Inserted : Boolean;
      Position : Cursor;
   begin
      Self.Insert (Element, Position, Inserted);

      if not Inserted then
         raise Constraint_Error with "Element already exists";
      end if;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self     : in out Object; Element : Unit.Object; Position : out Cursor;
      Inserted :    out Boolean)
   is
   begin
      if Element.Index = No_Index then
         if not Self.List.Is_Empty then
            Inserted := False;
            Position := (Current => Self.List.First);

         else
            Self.Multi_Unit := False;
            Self.List.Append (Element);
            Inserted := True;
            Position := (Current => Self.List.First);
         end if;

      elsif not Self.Multi_Unit and then not Self.List.Is_Empty then
         --  Trying to insert an indexed element while a non indexed
         --  one already exists.
         Inserted := False;
         Position := (Current => Self.List.First);

      else
         Self.Multi_Unit := True;

         if Element.Index <= Self.List.Last_Index then
            Position := (Current => Self.List.To_Cursor (Element.Index));

            if Self.List (Position.Current).Is_Defined then
               Inserted := False;
            else
               Self.List (Position.Current) := Element;
               Inserted := True;
            end if;
         else
            --  add padding elements if needed
            while Self.List.Last_Index + 1 /= Element.Index loop
               Self.List.Append (Unit.Undefined);
            end loop;

            Self.List.Append (Element);

            Inserted := True;
            Position := (Current => Self.List.Last);
         end if;
      end if;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
   begin
      return Self.List.Is_Empty;
   end Is_Empty;

   ------------------------
   -- Is_Multi_Unit_List --
   ------------------------

   function Is_Indexed_List (Self : Object) return Boolean is
   begin
      return Self.Multi_Unit;
   end Is_Indexed_List;

   -------------
   -- Iterate --
   -------------

   function Iterate (Self : Object) return Unit_Iterator.Forward_Iterator'Class
   is
   begin
      return Iterator'(Root => Self'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Object) return Ada.Containers.Count_Type is
   begin
      return Self.List.Length;
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next (Iter : Iterator; Position : Cursor) return Cursor
   is
      Next : constant Unit_Vectors.Cursor :=
               Unit_Vectors.Next (Position.Current);
   begin
      return Cursor'(Current => Next);
   end Next;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self : aliased in out Object; Index : Unit_Index) return Reference_Type
   is
      Cursor : Unit_Vectors.Cursor;
   begin
      if Self.Multi_Unit then
         Cursor := Self.List.To_Cursor (Index);
      else
         pragma Assert (Index = No_Index);
         Cursor := Self.List.First;
      end if;

      return Reference (Self, (Current => Cursor));
   end Reference;

   function Reference
     (Self : aliased in out Object; Position : Cursor) return Reference_Type
   is
      Ref : constant Unit_Vectors.Reference_Type :=
              Self.List.Reference (Position.Current);
   begin
      return (Unit => Ref.Element.all'Unchecked_Access, Ref => Ref);
   end Reference;

end GPR2.Unit.List;
