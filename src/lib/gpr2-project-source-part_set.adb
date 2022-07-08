------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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
         return (Part => Self.SS.Constant_Reference (Position.SC).Element);
      else
         return (Part => Self.HS.Constant_Reference (Position.HC).Element);
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
