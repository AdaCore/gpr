--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Source.Part_Set is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      if Self.Sorted then
         Self.SS.Clear;
      else
         Self.HS.Clear;
      end if;
   end Clear;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
   begin
      if Self.Sorted then
         declare
            Ref : constant Source_Part_Ordered_Sets.Constant_Reference_Type :=
                    Self.SS.Constant_Reference (Position.SC);
         begin
            return (Part   => Ref.Element.all'Unrestricted_Access,
                    Sorted => True,
                    SRef   => Ref);
         end;
      else
         declare
            Ref : constant Source_Part_Hashed_Sets.Constant_Reference_Type :=
                    Self.HS.Constant_Reference (Position.HC);
         begin
            return (Part   => Ref.Element.all'Unrestricted_Access,
                    Sorted => False,
                    HRef   => Ref);
         end;
      end if;
   end Constant_Reference;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self    : in out Object;
      Element : Source_Part) is
   begin
      if Self.Sorted then
         Self.SS.Insert (Element);
      else
         Self.HS.Insert (Element);
      end if;
   end Insert;

   procedure Insert
     (Self     : in out Object;
      Element  : Source_Part;
      Position : out Cursor;
      Inserted : out Boolean) is
   begin
      if Self.Sorted then
         Position := (Sorted => True,
                      SC     => Source_Part_Ordered_Sets.No_Element);
         Self.SS.Insert (Element, Position.SC, Inserted);
      else
         Position := (Sorted => False,
                      HC     => Source_Part_Hashed_Sets.No_Element);
         Self.HS.Insert (Element, Position.HC, Inserted);
      end if;
   end Insert;

   -------------
   -- Iterate --
   -------------

   function Iterate (Self : Object)
                     return Source_Part_Iterator.Forward_Iterator'Class is
   begin
      if Self.Sorted then
         return Iterator'(Sorted => True, SRoot => Self.SS'Unchecked_Access);
      else
         return Iterator'(Sorted => False, HRoot => Self.HS'Unchecked_Access);
      end if;
   end Iterate;

   -----------
   -- Union --
   -----------

   procedure Union
     (Self  : in out Object;
      Other : Object)
   is
      C        : Cursor;
      Inserted : Boolean;
   begin
      if Self.Sorted /= Other.Sorted then
         for Item of Other loop
            Self.Insert (Item, C, Inserted);
         end loop;

      elsif Self.Sorted then
         Self.SS.Union (Other.SS);

      else
         Self.HS.Union (Other.HS);
      end if;
   end Union;

end GPR2.Project.Source.Part_Set;
