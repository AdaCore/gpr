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

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body InDash is

   -------------
   -- Display --
   -------------

   procedure Display (This : access Instrument) is
   begin
      New_Line;
      Put (Head (To_String (This.Name), 13));
      Put (" : ");
   end Display;

   ----------
   -- Name --
   ----------

   function Name (This : access Instrument) return String is
   begin
      return To_String (This.Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This : access Instrument; To : String) is
   begin
      This.Name := To_Unbounded_String (To);
   end Set_Name;

end InDash;
