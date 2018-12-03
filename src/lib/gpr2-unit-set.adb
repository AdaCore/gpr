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

package body GPR2.Unit.Set is

   type Iterator is new Unit_Iterator.Forward_Iterator with
   record
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
      Self.S.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type is
   begin
      return Constant_Reference_Type'
        (Unit => Set.Constant_Reference
           (Self.S, Position.Current).Element);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains (Self : Object; Name : Name_Type) return Boolean is
   begin
      return Self.S.Contains (Name);
   end Contains;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Unit.Object is
   begin
      return Set.Element (Position.Current);
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Self : Object; Name : Name_Type) return Unit.Object is
   begin
      return Self.S.Element (Name);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
      Position : constant Cursor :=
                   Cursor'(Current => Set.First (Iter.Root.S));
   begin
      if not Has_Element (Position)
      then
         return Position;
      else
         return Next (Iter, Position);
      end if;
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Self : Object) return Unit.Object is
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

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self : in out Object; Name : Name_Type; Unit : GPR2.Unit.Object) is
   begin
      Self.S.Insert (Name, Unit);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
   begin
      return Self.S.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Unit_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Root => Self'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Object) return Containers.Count_Type is
   begin
      return Self.S.Length;
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Iter);
      Next : Cursor := Cursor'(Current => Set.Next (Position.Current));
   begin
      while Has_Element (Next)
      loop
         Next := Cursor'(Current => Set.Next (Next.Current));
      end loop;

      return Next;
   end Next;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self : in out Object; Name : Name_Type; Unit : GPR2.Unit.Object) is
   begin
      Self.S.Replace (Name, Unit);
   end Replace;

end GPR2.Unit.Set;
