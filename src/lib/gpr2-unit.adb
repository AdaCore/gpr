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

package body GPR2.Unit is

   -----------------------
   -- Set_Separate_From --
   -----------------------

   procedure Set_Separate_From (Self : in out Object; Name : Name_Type) is
   begin
      Self.Sep_From := To_Unbounded_String (String (Name));
      Self.Kind     := S_Separate;
   end Set_Separate_From;

   -----------------
   -- Update_Kind --
   -----------------

   procedure Update_Kind (Self : in out Object; Kind : Library_Unit_Type) is
   begin
      Self.Kind := Kind;
   end Update_Kind;

end GPR2.Unit;
