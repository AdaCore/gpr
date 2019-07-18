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

package body GPR2.Version is

   use Ada.Text_IO;

   -------------
   -- Display --
   -------------

   procedure Display
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String) is
   begin
      Put_Line (Tool_Name & " " & Version_String);
      Put_Line
        ("Copyright (C) " & Initial_Year & '-' & Current_Year & ", "
         & Copyright_Holder);
   end Display;

   ---------------------------
   -- Display_Free_Software --
   ---------------------------

   procedure Display_Free_Software is
   begin
      Put_Line (Free_Software);
      New_Line;
   end Display_Free_Software;

end GPR2.Version;
