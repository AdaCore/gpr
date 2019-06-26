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

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body GPR2.ALI.Unit_Data is

   --------------
   -- Add_With --
   --------------

   procedure Add_With (Self : in out Object; W : With_Data.Object) is
   begin
      Self.Withs.Append (W);
   end Add_With;

   -----------
   -- Image --
   -----------

   function Image (F : Flag) return String is
      Result           : String   := F'Img;
      Index            : Positive := Result'First;
      Uppercase_Letter : Boolean  := True;

      function Skip_Uppercase_Word (Word : String) return Boolean;
      --  If the word at current index is Word, skip to next word, and
      --  return True.

      -------------------------
      -- Skip_Uppercase_Word --
      -------------------------

      function Skip_Uppercase_Word (Word : String) return Boolean is
      begin
         if Index + Word'Length - 1 in Result'Range
           and then Result (Index .. Index + Word'Length - 1) = Word
           and then (Index + Word'Length > Result'Last
                     or else Result (Index + Word'Length) = '_')
         then
            Index := Index + Word'Length + 1;
            return True;
         else
            return False;
         end if;
      end Skip_Uppercase_Word;

   begin
      --  Result is initially all uppercase

      while Index in Result'Range loop
         --  Check for case of word to leave as uppercase

         if Result (Index) = '_' then
            Uppercase_Letter := True;
            Index := Index + 1;
         end if;

         if Uppercase_Letter then
            if Skip_Uppercase_Word ("RCI")
              or else Skip_Uppercase_Word ("RACW")
              or else Skip_Uppercase_Word ("SAL")
            then
               null;
            else
               Index := Index + 1;
               Uppercase_Letter := False;
            end if;

         else
            Result (Index) := To_Lower (Result (Index));
            Index := Index + 1;
         end if;
      end loop;

      return Result;
   end Image;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags (Self : in out Object; Flags : Flag_Array) is
   begin
      Self.Flags := Flags;
   end Set_Flags;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Self : in out Object; Kind : Unit_Kind) is
   begin
      Self.Kind := Kind;
   end Set_Kind;

   ---------------
   -- Set_Utype --
   ---------------

   procedure Set_Utype (Self : in out Object; Utype : Unit_Type) is
   begin
      Self.Utype := Utype;
   end Set_Utype;

end GPR2.ALI.Unit_Data;
