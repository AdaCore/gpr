------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with GPR2.Path_Name;

procedure Main is

   use Ada;
   use GPR2;

   C1 : constant Path_Name.Object := Path_Name.Create_File ("css/*.css");
   C2 : constant Path_Name.Object := Path_Name.Create_File ("doc/README");

begin
   Text_IO.Put_Line ("C1.Simple_Name: " & String (C1.Simple_Name));
   Text_IO.Put_Line ("C2.Simple_Name: " & String (C2.Simple_Name));
   Text_IO.Put_Line ("C1.Base_Name: " & String (C1.Base_Name));
   Text_IO.Put_Line ("C2.Base_Name: " & String (C2.Base_Name));
end Main;
