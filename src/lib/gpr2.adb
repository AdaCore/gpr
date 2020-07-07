------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Directories;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;

with GNAT.OS_Lib;

package body GPR2 is

   ---------
   -- "<" --
   ---------

   overriding function "<" (Left, Right : Optional_Name_Type) return Boolean is
      use Ada.Strings;
   begin
      return Less_Case_Insensitive (String (Left), String (Right));
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Optional_Name_Type) return Boolean is
      use Ada.Strings;
   begin
      return Equal_Case_Insensitive (String (Left), String (Right));
   end "=";

   -------------------------
   -- Get_Tools_Directory --
   -------------------------

   function Get_Tools_Directory return String is
      use type GNAT.OS_Lib.String_Access;

      GPRls : GNAT.OS_Lib.String_Access :=
                GNAT.OS_Lib.Locate_Exec_On_Path ("gprls");
      --  Check for GPRls executable
   begin
      if GPRls = null then
         return "";
      else
         return Result : constant String :=
                           Directories.Containing_Directory
                             (Directories.Containing_Directory (GPRls.all))
         do
            GNAT.OS_Lib.Free (GPRls);
         end return;
      end if;
   end Get_Tools_Directory;

   -----------
   -- Quote --
   -----------

   function Quote
     (Str        : Value_Type;
      Quote_With : Character := '"') return Value_Type is
   begin
      return Quote_With & Str & Quote_With;
   end Quote;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (Enable : Boolean) is
   begin
      Debug := Enable;
   end Set_Debug;

   -------------------
   -- To_Hex_String --
   -------------------

   function To_Hex_String (Num : Word) return String is
      Hex_Digit : constant array (Word range 0 .. 15) of Character :=
                    "0123456789abcdef";
      Result : String (1 .. 8);
      Value  : Word := Num;
   begin
      for J in reverse Result'Range loop
         Result (J) := Hex_Digit (Value mod 16);
         Value := Value / 16;
      end loop;

      return Result;
   end To_Hex_String;

   -------------
   -- Unquote --
   -------------

   function Unquote (Str : Value_Type) return Value_Type is
   begin
      if Str'Length >= 2
        and then
          ((Str (Str'First) = ''' and then Str (Str'Last) = ''')
           or else (Str (Str'First) = '"' and then Str (Str'Last) = '"'))
      then
         return Str (Str'First + 1 .. Str'Last - 1);
      else
         return Str;
      end if;
   end Unquote;

end GPR2;
