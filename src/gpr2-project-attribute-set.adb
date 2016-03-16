------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with Ada.Strings.Equal_Case_Insensitive; use Ada;

package body GPR2.Project.Attribute.Set is

   function Is_Matching (Object : Iterator; Position : Cursor) return Boolean;
   --  Returns True if the current Position is matching the Iterator

   ------------
   -- Filter --
   ------------

   function Filter
     (Self  : Object;
      Name  : String := "";
      Index : String := "") return Object is
   begin
      if Name = "" and then Index = "" then
         return Self;

      else
         declare
            Result : Object;
         begin
            for C in Self.Iterate_Filter (Name, Index) loop
               Result.Insert (Set.Key (C), Set.Element (C));
            end loop;

            return Result;
         end;
      end if;
   end Filter;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Cursor is
      Position : constant Cursor := Object.Object.First;
   begin
      if not Is_Matching (Object, Position) then
         return Next (Object, Position);
      else
         return Position;
      end if;
   end First;

   -----------------
   -- Is_Matching --
   -----------------

   function Is_Matching
     (Object : Iterator; Position : Cursor) return Boolean
   is
      A     : constant Attribute.Object := Set.Element (Position);
      Name  : constant String := To_String (Object.Name);
      Index : constant String := To_String (Object.Index);
   begin
      return
        (Name = "" or else Strings.Equal_Case_Insensitive (A.Name, Name))
        and then (Index = "" or else A.Index_Equal (Index));
   end Is_Matching;

   -------------
   -- Iterate --
   -------------

   function Iterate_Filter
     (Self  : Object;
      Name  : String := "";
      Index : String := "")
      return Set.Map_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return It : Iterator do
         It.Object   := Set.Map (Self);
         It.Position := Self.First;
         It.Name     := To_Unbounded_String (Name);
         It.Index    := To_Unbounded_String (Index);
      end return;
   end Iterate_Filter;

   ----------
   -- Last --
   ----------

   overriding function Last  (Object : Iterator) return Cursor is
      Position : constant Cursor := Object.Object.Last;
   begin
      if not Is_Matching (Object, Position) then
         return Previous (Object, Position);
      else
         return Position;
      end if;
   end Last;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object : Iterator; Position : Cursor) return Cursor
   is
      Result : Cursor := Set.Next (Position);
   begin
      while Set.Has_Element (Result)
        and then not Is_Matching (Object, Result)
      loop
         Result := Set.Next (Result);
      end loop;

      return Result;
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object : Iterator; Position : Cursor) return Cursor
   is
      Result : Cursor := Set.Previous (Position);
   begin
      while Set.Has_Element (Result)
        and then not Is_Matching (Object, Result)
      loop
         Result := Set.Previous (Result);
      end loop;

      return Result;
   end Previous;

end GPR2.Project.Attribute.Set;
