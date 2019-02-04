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

with Ada.Strings.Unbounded;

package body GPR2.Containers is

   -----------
   -- Image --
   -----------

   function Image (Values : Value_List) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      First  : Boolean := True;
   begin
      Append (Result, '(');

      for V of Values loop
         if not First then
            Append (Result, ", ");
         end if;

         Append (Result, '"' & String (V) & '"');
         First := False;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   function Image (Values : Source_Value_List) return String is
      L : Value_List;
   begin
      for V of Values loop
         L.Append (V.Text);
      end loop;

      return Image (L);
   end Image;

   ----------------------
   -- Value_Or_Default --
   ----------------------

   function Value_Or_Default
     (Map     : Name_Value_Map;
      Key     : Name_Type;
      Default : Value_Type := No_Value) return Value_Type
   is
      C : constant Name_Value_Map_Package.Cursor := Map.Find (Key);
   begin
      if Name_Value_Map_Package.Has_Element (C) then
         return Map (C);
      else
         return Default;
      end if;
   end Value_Or_Default;

end GPR2.Containers;
