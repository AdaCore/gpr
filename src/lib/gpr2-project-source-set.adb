------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
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

package body GPR2.Project.Source.Set is

   type Iterator (Filter : Source_Filter) is
     new Source_Iterator.Forward_Iterator with
   record
     Root : not null access constant Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Match_Filter
     (Iter : Iterator'Class; Source : Project.Source.Object) return Boolean;
   --  ??

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
      pragma Unreferenced (Self);
   begin
      return Constant_Reference_Type'
        (Source => Set.Constant_Reference
           (Position.Sources.S,
            Position.Current).Element);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Object; Source : Project.Source.Object) return Boolean is
   begin
      return Self.S.Contains (Source);
   end Contains;

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
      Position : constant Cursor :=
                   Cursor'(Iter.Root, Set.First (Iter.Root.S));
   begin
      if not Has_Element (Position)
        or else Match_Filter (Iter, Set.Element (Position.Current))
      then
         return Position;
      else
         return Next (Iter, Position);
      end if;
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

   ------------
   -- Insert --
   ------------

   procedure Insert (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.S.Insert (Source);
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
     (Self   : Object;
      Filter : Source_Filter := S_All)
      return Source_Iterator.Forward_Iterator'Class
   is
   begin
      return Iterator'(Filter => Filter, Root => Self'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Object) return Containers.Count_Type is
   begin
      return Self.S.Length;
   end Length;

   ------------------
   -- Match_Filter --
   ------------------

   function Match_Filter
     (Iter : Iterator'Class; Source : Project.Source.Object) return Boolean
   is
      use all type GPR2.Source.Kind_Type;
   begin
      --  We check the S_All filter here as getting the Kind for a source may
      --  need a parsing to know whether we have a body or a separate unit.
      --  So to avoid any parsing when we just want all sources we check this
      --  specific case now.

      if Iter.Filter = S_All then
         return True;

      else
         declare
            Kind : constant GPR2.Source.Kind_Type := Source.Source.Kind;
         begin
            return
              (case Iter.Filter is
                  when S_Compilable =>
                     (Source.Source.Other_Part = GPR2.Source.Undefined
                      and then Kind /= GPR2.Source.S_Separate)
                     or else Kind = GPR2.Source.S_Body,

                  when S_Spec       =>
                     Kind = GPR2.Source.S_Spec,

                  when S_Body       =>
                     Kind = GPR2.Source.S_Body,

                  when S_Separate   =>
                     Kind = GPR2.Source.S_Separate,

                  when others       =>
                     True);
         end;
      end if;
   end Match_Filter;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      Next : Cursor :=
               Cursor'(Position.Sources, Set.Next (Position.Current));
   begin
      while Has_Element (Next)
        and then not Match_Filter (Iter, Set.Element (Next.Current))
      loop
         Next := Cursor'(Next.Sources, Set.Next (Next.Current));
      end loop;

      return Next;
   end Next;

   -------------
   -- Replace --
   -------------

   procedure Replace (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.S.Replace (Source);
   end Replace;

end GPR2.Project.Source.Set;
