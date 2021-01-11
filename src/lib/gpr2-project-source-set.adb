------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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
      Position : Cursor) return Constant_Reference_Type is
   begin
      return Constant_Reference_Type'
        (Source => Set.Constant_Reference
           (Self.S, Position.Current).Element);
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
                   Cursor'(Current => Set.First (Iter.Root.S));
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
      return Source_Iterator.Forward_Iterator'Class is
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
     (Iter   : Iterator'Class;
      Source : Project.Source.Object) return Boolean
   is
      Src : constant GPR2.Source.Object := Source.Source;
   begin
      --  We check the S_All filter here as getting the Kind for a source may
      --  require a parsing to know whether we have a body or a separate unit.
      --  So to avoid any parsing when we just want all sources we check this
      --  specific case now.

      if Iter.Filter = S_All then
         return True;

      else
         case Iter.Filter is
            when S_Compilable =>
               if Src.Has_Units then
                  for CU of Src.Units loop
                     if (Src.Has_Single_Unit
                         and then not Source.Has_Other_Part
                         and then CU.Kind /= GPR2.Unit.S_Separate
                         and then Src.Language = "Ada")
                        --  The condition above is about Ada package specs
                        --  without a body, which have to be compilable.
                        or else CU.Kind in GPR2.Unit.Body_Kind
                     then
                        --  At least one compilable unit
                        return True;
                     end if;
                  end loop;

                  return False;

               else
                  return Src.Kind = GPR2.Unit.S_Body;
               end if;

            when S_Spec     =>
               if Src.Has_Units then
                  return (for some CU of Src.Units =>
                            CU.Kind in GPR2.Unit.Spec_Kind);
               else
                  return Src.Kind in GPR2.Unit.Spec_Kind;
               end if;

            when S_Body     =>
               if Src.Has_Units then
                  return (for some CU of Src.Units =>
                            CU.Kind in GPR2.Unit.Body_Kind);
               else
                  return Src.Kind in GPR2.Unit.Body_Kind;
               end if;

            when S_Separate =>
               if Src.Has_Units then
                  return (for some CU of Src.Units =>
                            CU.Kind = GPR2.Unit.S_Separate);
               else
                  return Src.Kind = GPR2.Unit.S_Separate;
               end if;

            when others     =>
               return True;
         end case;
      end if;
   end Match_Filter;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      Next : Cursor := Cursor'(Current => Set.Next (Position.Current));
   begin
      while Has_Element (Next)
        and then not Match_Filter (Iter, Set.Element (Next.Current))
      loop
         Next := Cursor'(Current => Set.Next (Next.Current));
      end loop;

      return Next;
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
