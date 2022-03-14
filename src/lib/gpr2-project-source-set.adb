------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

with GPR2.Unit;

package body GPR2.Project.Source.Set is

   type Iterator is new Source_Iterator.Forward_Iterator with record
      Filter : Source_Filter;
      Root   : not null access constant Set.Set;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Match_Filter
     (Filter : Source_Filter; Source : Project.Source.Object) return Boolean
   with Inline;
   --  Returns True if Source matches the iterator Filter (see Source_Filter)

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
      Ref : Set.Constant_Reference_Type renames
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
      Position : constant Cursor :=
                   Cursor'(Current => Iter.Root.First);
   begin
      if not Has_Element (Position)
        or else Match_Filter (Iter.Filter, Set.Element (Position.Current))
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
     (Self   : Object;
      Filter : Source_Filter := S_All)
      return Source_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Filter => Filter, Root => Self.S'Unrestricted_Access);
   end Iterate;

   ------------------
   -- Match_Filter --
   ------------------

   function Match_Filter
     (Filter : Source_Filter;
      Source : Project.Source.Object) return Boolean
   is
   begin
      case Filter is
         when S_Compilable =>
            if Source.Has_Units then
               for CU of Source.Units loop
                  if Is_Compilable (Source, CU.Index) then
                     --  At least one compilable unit
                     return True;
                  end if;
               end loop;

               return False;

            elsif not Source.Is_Compilable then
               return False;

            else
               return Source.Kind = GPR2.Unit.S_Body;
            end if;

         when S_Spec     =>
            if Source.Has_Units then
               return (for some CU of Source.Units =>
                         CU.Kind in GPR2.Unit.Spec_Kind);
            else
               return Source.Kind in GPR2.Unit.Spec_Kind;
            end if;

         when S_Body     =>
            if Source.Has_Units then
               return (for some CU of Source.Units =>
                         CU.Kind in GPR2.Unit.Body_Kind);
            else
               return Source.Kind in GPR2.Unit.Body_Kind;
            end if;

         when S_Separate =>
            if Source.Has_Units then
               return (for some CU of Source.Units =>
                         CU.Kind in GPR2.Unit.S_Separate);
            else
               return Source.Kind = GPR2.Unit.S_Separate;
            end if;

         when S_All =>
            return True;
      end case;
   end Match_Filter;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      Next : Set.Cursor := Set.Next (Position.Current);
   begin
      if Iter.Filter = S_All then
         return Cursor'(Current => Next);
      end if;

      loop
         exit when not Set.Has_Element (Next);
         exit when Match_Filter (Iter.Filter, Set.Element (Next));
         Next := Set.Next (Next);
      end loop;

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
