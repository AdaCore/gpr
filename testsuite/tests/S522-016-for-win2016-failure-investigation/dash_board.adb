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


with Ada.Text_IO;

package body Dash_Board is

   --------------
   -- Register --
   --------------

   procedure Register (Device : InDash.Any_Instrument) is
   begin
      Registry.Append (Device);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Device : InDash.Any_Instrument) is
   begin
      Registry.Delete (Registry.Find_Index (Device));
   end Unregister;

   use Instruments;

   -------------
   -- Display --
   -------------

   procedure Display is
      C : Cursor := First (Registry);
   begin
      while C /= No_Element loop
         Element (C).Display; -- dispatches
         Next (C);
      end loop;
      Ada.Text_IO.New_Line;
   end Display;

   ------------
   -- Update --
   ------------

   procedure Update (Millisec : Integer) is
      C : Cursor := First (Registry);
   begin
      while C /= No_Element loop
         Element (C).Update (Millisec); -- dispatches
         Next (C);
      end loop;
   end Update;

end Dash_Board;
